----Diet item check----

library(readxl)
library(tidyverse)
library(rfishbase)
library(taxize)
library(dplyr)
library(tidyr)

install.packages(c("taxize","rfishbase"))

data <- read_excel("data/Dietdata_corrected.xlsx", sheet = "Sheet1")
names(data)

  ##Remove both trailing and leading whitespace
data<- data.frame(lapply(data, trimws, which="both"))

  ##Item spname variable
data %>%
  unite(col="item_spname", c(item_gen, item_sp), remove=FALSE) #doesn't work WHY ?!

data$item_spname<-paste(data$item_gen, data$item_sp, sep=" ")


  ##Check fish species name and extract families
levels(data$item_class)
item_fish<-unique(data$item_spname[data$item_class=="Actinopterygii"])

item_fish_cor<-suppressWarnings(rfishbase::validate_names(item_fish))
item_fish_errors<-item_fish[!item_fish %in% item_fish_cor]

print(item_fish_errors)

ok_fish<-c("NA",                       "Acanthurus sp",             
"Jenkinsia sp",                "Monacanthus sp",             
"Halichoeres sp" ,             "Opisthognathus sp",          
"Hypoatherina harringtonensis",   "Haemulon sp" ,               
"Harengula sp",                "Serranus sp",                
"Anchoa sp",                   "Scarus sp",                  
"Tylosurus sp",                "Acanthurus sp",              
"Mugil sp",                    "Stephanolepis setifer",        
"Sargocentron coruscum",        "Malacoctenus gilli",          
"Pomacentrus sp",              "Stegastes pictus",         
"Priolepis hipoliti",        "Hypoatherina harringtonensis",   
"Caranx crysos",                "Decapterus sp",              
"Diodon sp",                   "Echidna catenata",           
"Trachinocephalus myops",      "Sphoeroides sp",             
"Starksia sp",                 "Sparisoma sp",               
"Gymnothorax sp",              "Haemulon vittatum",            
"Holocentrus sp",              "Lactophrys sp",              
"Lutjanus sp" ,                "Gymnothorax miliaris",           
"Stegastes fuscus",          "Heteropriacanthus cruentatus",     
"Scarus iseri",           "Stegastes planifrons",     
"Phaeoptyx pigmentaria",         "Coryphopterus sp",           
"Holocentrus adscensionis",     "Sphoeroides spengleri",      
              "Haemulon plumierii",          
"Myrichthys sp",               "Sparisoma viride",           
"Saurida sp",                 "Lactophrys triqueter",       
      "Calamus sp",                 
"Coryphopterus sp",               "Hippocampus sp",             
"Plectroglyphidodon imparipennis",     
"Pristiapogon kallopterus",              "Canthigaster sp",            
"Stegastes fasciolatus",        "Sargocentron xantherythrum",  
"Blenniella gibbifrons",     "Cirripectes sp" ,            
"Siganus sp",                  "Cirripectes variolosus" ,    
"Plectroglyphidodon dickii",             "Plectroglyphidodon leucozonus",       
"Epinephelus sp" ,             "Abudefduf sp",               
"Ostorhinchus novemfasciatus",       "Hypoatherina ovalaua",          
"Ctenogobius sp",              "Chrysiptera brownriggii",         
"Blenniella paula",         "Enneapterygius minutus",       
"Chromis ternatensis",           "Trachinotus baillonii",        
"Selar crumenophthalmus", "Pristiapogon exostigma",            
"Eviota sp",                   "Istiblennius sp" ,           
"Neoniphon sammara",         "Sargocentron diadema",        
"Pseudocheilinus sp",          "Naso sp" ,                   
"Chrysiptera glauca",           "Hypoatherina temminckii",     
"Diplogrammus goramensis",    "Chrysiptera biocellata")

length(ok_fish)

## Replace the errors
data$item_fish <- data$item_spname
data$item_fish[data$item_fish%in%item_fish_errors] <- sapply(data$item_fish[data$item_fish%in%item_fish_errors],FUN = function(x){
  line <- which(item_fish_errors==x)
  y <- gsub(item_fish_errors[item_fish_errors==x],ok_fish[line],x = x)
  print(paste("correcting ",x))
  return(y)})


  ##Split into corrected genus and sp names
data <- data[,-46]
data$item_cor<-data$item_fish

names(data)[47] <- "item_cor"

data <- data[,-46]



  #Extract item_fish families from Fishbase classification
item_fish_fam<-load_taxa(gen=data$item_gen_cor)$Family #Works ! Goal is to replace Family names

  #Try with WORMS package to do all items at the same time, bot good coz "tibble" structure
library(worrms)

wm_classification_(name = data$item_gen_cor)$scientificname[8]
warnings() # errors coz conflicting names when giving just the genus name

wm_classification_(name = c('Cantherhines pullus','Holocentrus diadema'))$scientificname[8] #doesn't seem to work for several elements

wm_classification_(name = "Holocentrus diadema")$scientificname[8] #give family even when name not accepted

wm_classification_(name = "Undinula vulgaris")

  #Check with another package taxize
library(acs)
api.key.install("748f08277d8b379af2d4d27561b458c9c108", file = "key.rda")

Sys.setenv(ENTREZ_KEY = "748f08277d8b379af2d4d27561b458c9c108")

item_tax <- tax_name(data$item_cor, get=c("family", "order"), db="ncbi") #Create a data frame 
item_tax <- item_tax[,-1]
names(item_tax)[2] <- "item_fam_cor"
names(item_tax)[3] <- "item_order_cor"
names(item_tax)[1] <- "item_cor"

library(xlsx)
write.xlsx(item_tax, "item_tax.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

item_tax <- read_excel("item_tax.xlsx", sheet = "Sheet1")

data$item_cor <- item_tax$query #correction on excel re assign to the variable

data <- data %>% separate(item_cor, c("item_gen_cor", "item_sp_cor"), sep=" ")

data$item <- as.character(data$item)
class(data$item)
class(data$item_cor)

data$item_cor[data$item_cor=="NA"] <- data$item[data$item_cor=="NA"] #replace NA values by the one from the item column


    #The same with family
data$item_fam_cor <- item_tax$family #add the family column to the dataset
class(data$item_fam)
data$item_fam <- as.character(data$item_fam)
data$item_fam_cor[is.na(data$item_fam_cor)] <- data$item_fam[is.na(data$item_fam_cor)]

print(unique(data$item_fam_cor))

  #Get the order from the corrected family

Sys.setenv(ENTREZ_KEY = "748f08277d8b379af2d4d27561b458c9c108")

order <- tax_name(data$item_fam_cor, get="order", db="ncbi")

data$item_ord_cor <- order$order
data$item_ord <- as.character(data$item_ord)

data$item_ord_cor[is.na(data$item_ord_cor)] <- data$item_ord[is.na(data$item_ord_cor)]

 #Check item resolution

apply(data$site_code, FUN = function(x){
  table(data$item_phylum[data$item_phylum %in% data$item_cor])
}) 


data %>% 
  group_by (site_code, item_phylum) %>% 
  count (data$item_phylum %in% data$item_cor)


data$site_code <- as.character(data$site_code)
class(data$site_code)

intersect(data$item_kingdom,data$item_cor) #gives just the name of the element

table(data$item_kingdom)
table(data$item_phylum)

## Example to get % of items ID to the genus level per site
 nb.gen <- data %>% 
   group_by(site_code) %>%
   count(item_gen_cor)
 
 nb.gen <- nb.gen[-which(nb.gen$item_gen_cor == "NA"),]
 
per.gen <- tapply(nb.gen$n, nb.gen$site_code ,sum)*100/2061

per.gen <- round(per.gen, 2)
 
nb.gen$item_gen_cor

##Example to get % of items ID per Class for exemple

?ddply

nb.gen.class <- data %>% 
  group_by(item_class) %>%
  count(item_gen_cor)

nb.class <- data %>% 
  group_by(item_class) %>%
  count(item_class)

nb.class$item_class <- as.character(nb.class$item_class)

nb.class$per <- nb.class$n/sum(nb.class$n)*100

nb.gen.class <- nb.gen.class[-which(nb.gen.class$item_gen_cor == "NA"),]

sum.gen.class <- as.data.frame(tapply(nb.gen.class$n, nb.gen.class$item_class ,sum))

sum.gen.class <- sum.gen.class %>% rownames_to_column("class")
names(sum.gen.class)[2] <- "n"

per.gen.class <- as.data.frame(sum.gen.class$n/nb.class$n*100)
per.gen.class$class <- sum.gen.class$class
names(per.gen.class)[1] <- "% items"

per.gen.class[1] <- round(per.gen.class[1], 2)

 
write.xlsx(data, "dietdata2.0.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

length(unique(data$item_cor))
