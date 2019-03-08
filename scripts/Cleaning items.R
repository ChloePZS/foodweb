library(readxl)
library(tidyverse)
library(dplyr)
library(tidyr)
library(data.table)

data_raw <- read_excel("data/dietdata2.0.xlsx", sheet = "Sheet1", trim_ws=TRUE, na="NA")

?read_excel
warnings()

data <- data_raw %>% 
  select(site_code,class,family_cor,fish_sp,genus_cor,sp_cor,item_type,
         item_kingdom, item_phylum , item_class,item_ord_cor,item_fam_cor,
         item_cor,item_gen_cor, item_sp_cor,grp_raw, item_raw, nb_sample,nb_guts ,    
         nb_item ,     item_freq ,   item_volper,item_massper, item_numper,
         mean_SL  ,    min_SL  ,max_SL, min_TL ,max_TL,time,source) 

data <- unique(data)

data <- filter(data,!is.na(item_cor))
data <- filter(data %>% filter(!item_cor=="Gurry"))  #remove gurry items
data <- filter(data, !item_cor=="Animalia" & !item_cor=="Plantae") 
data <- filter(data, is.na(time) | time =="day") #don't keep samples at night
data <- unique(data)

names(data_raw)

#Create variable spname and replace "sp" by NA
data$item_spname <- paste(data$item_gen_cor, data$item_sp_cor, " ") #new variable with sp name
data$item_spname <- trimws(data$item_spname, which="both") 
data$item_spname[data$item_spname=="NA NA"] <- NA
data$item_sp_cor[data$item_sp_cor=="sp"] <- NA

data$item_type <- as.character(data$item_type)
data$item_kingdom <- as.character(data$item_kingdom)  
data$item_phylum <- as.character(data$item_phylum)
data$item_class <- as.character(data$item_class)
data$item_gen_cor <- as.character(data$item_gen_cor)
data$item_sp_cor <- as.character(data$item_sp_cor)
data$item_ord_cor <- as.character(data$item_ord_cor)
data$item_fam_cor <- as.character(data$item_fam_cor)
data$item_cor <- as.character(data$item_cor)

data$site_code <- as.character(data$site_code)

    ####Check the % items ID per site####
#haw
haw <- data%>% filter(site_code=="haw")

haw_tax <- haw %>% select(7:15) %>%
  summarise_all(funs(sum(!is.na(.))))

haw_per <- round(haw_tax*100/892, 2)

#mad
mad <- data%>% filter(site_code=="mad")

mad_tax <- mad %>% select(7:15) %>%
  summarise_all(funs(sum(!is.na(.))))

mad_per <- round(mad_tax*100/1611, 2)

#vir
vir <- data%>% filter(site_code=="vir")

vir_tax <- vir %>% select(7:15) %>%
  summarise_all(funs(sum(!is.na(.))))

vir_per <- round(vir_tax*100/3318, 2)


#mari
mari <- data%>% filter(site_code=="mari")

mari_tax <- mari %>% select(7:15) %>%
  summarise_all(funs(sum(!is.na(.))))

mari_per <- round(mari_tax*100/1126, 2)

tax <- bind_rows(mad_per, haw_per, vir_per, mari_per) %>%
  rownames_to_column(var="site_code") %>%
  mutate(site_code = recode(site_code, "1" = "mad" , "2" = "haw","3" = "vir" , "4"="mari"))

    ####Cleaning !!####

#create a another data for removing items 
datatest <- data

#datatest$item_cor <- data$item_spname #get rid of the item_cor variable as was a mix of all items col

 #datatest <- datatest %>% select(-item_spname) %>% rename(item_spname = item_cor)
 

    ####Discard unrelevant items row

#Here just remove rows under certain conditions, pretty straight forward
datatest <- filter(datatest,!is.na(item_type)) #Remove items for which the type is NA  
datatest <- filter(datatest,!is.na(item_cor) , item_kingdom %in% c("Animalia","Plantae")) #Remove row for which item = Na and kingdom Animalia or Plantae
datatest <- filter(datatest %>% filter(!item_cor=="Gurry")) #remove gurry items
datatest <- filter(datatest, !item_cor=="Animalia" & !item_cor=="Plantae")


datatest<- datatest %>%
  mutate(item_cor=recode(item_cor,"Fish"="Actinopterygii")) %>% #rename 
  filter(time!="night") %>%
  mutate(lower_level = case_when(
    !is.na(item_spname) ~ "Species",
    item_cor==item_fam_cor ~ "Family", 
    item_cor==item_ord_cor ~ "Order",
    item_cor==item_class ~ "Class",
    item_cor==item_phylum ~ "Phylum")) #create a variable of the last levels


##Pb is to remove the duplicate rows, and those that carry the same info under higher taxonomic level
#Try 1 : create a variable that would have the lowest tax level for each species, then remove 
#the rows of higher level if lower level present
datatest$lower_level
ord <- c("Species","Family","Order","Class","Phylum")
datatest$lower_level <- factor(datatest_cor$lower_level, levels=ord, ordered=TRUE) #creating a factor with specified levels

class(datatest$lower_level)

min(datatest$lower_level, na.rm=T)


datatest$lower_level_sp <- sapply(datatest_cor$fish_sp, function(x){
  min(datatest_cor$lower_level)
}) #doesnt work

#Try 2 : Nina's : remove row for which more than 1 obs per class/per sp and with NA at family or order levels
##Modif Chloe
sum <- dplyr::summarise(group_by(datatest, site_code, item_class,fish_sp ), tot = length(item_cor))

test <- dplyr::left_join(datatest, sum)
length(which(is.na(test$item_ord_cor) & test$tot > 1))

sum(is.na(test$item_ord_cor))

class(test$item_cor)
class(test$item_ord_cor)
test$item_cor <- as.character(test$item_cor)
test$item_ord_cor <- as.character(test$item_ord_cor)

test2 <- test[is.na(test$item_ord_cor) & test$tot > 1,]
test2 <- test %>% filter(is.na(test$item_fam_cor) & test$tot > 1 & test$item_cor!=test$item_ord_cor)

nrow(unique(dplyr::select(test, site_code, fish_sp)))

sel <- which(is.na(test$item_fam_cor) & test$tot > 1 & test$item_cor!=test$item_ord_cor) #rows for which fam = NA and more than one observations/sp

test_sel <- test[-sel,] #remove way too much infos, especially for Harmelin Vivien which items are mostly ID to high levels
#it doesnt removes duplicates


#Try 3
datatest_cor <- datatest %>% 
  gather("item_kingdom", "item_phylum", "item_class", "item_ord_cor",
         "item_fam_cor", "item_gen_cor", "item_spname", key = "level", 
         value = "Tax_id")

#Try 4
?duplicated


    ####Add a broad item groups colomn####

#Create a vector of corrected names from the "grp_raw" vector
data$item_guild <- data$grp_raw
unique(data$grp_raw)
grp_err <- unique(data$grp_raw)
grp_cor <- c(
  "Fish"               ,          "Cephalopoda",                 
  "Sea turtles"        ,          "Sipuncula",                 
  "Crab"               ,          "Polychaeta",                 
  "Bivalvia",                   "Shrimp",                      
  "Hemichordata",                "Stomatopoda",                 
  "Gastropoda",                   "Copepoda",                    
  "Zooplankton",                    "Plants",                      
  "Animalia",                  "Zooplankton",                      
  "Tunicata",                    "Eggs",                        
  "Siphonophora",                "Ostracoda",                   
  "Crustacea",                    "Amphipoda",                  
  "Cephalopoda",                       NA ,                           
  "Cephalopoda",                    "Echinoidea",                   
  "Insecta",                      "Paguroidea",                 
  "Seagrass",                     "Isopoda",                     
  "Ophiuroidea",                    "Polyplacophora",                     
  "Scyllaridae",            "Cirripedia",                    
  "Foraminifera",                 "Echiuroidea",                  
  "Palinuridae",                "Eggs",                   
  "Tanaidacea",                      "Polychaeta",                  
  "Ctenophora",                  "Heteropoda",                  
  "Zooplankton",                   "Asteroidea",                   
  "Anthozoa",                   "Porifera",                     
  "Zooplankton",                 "Scaphopoda",                  
  "Priapulida",                  "Bryozoa",                     
  "Holothuria",                 "Polyplacophora",             
  "Algae",                        "Eggs",                 
  "Worms",                    "Organic",    
  "Zoantharia",                 "Hydrozoa",                  
  "Algae",            "Scyphozoa",                 
  "Mollusca",                      "Cnidaria",         
  "Algae",               "Echinodermata",                 
  "Pycnogonida",                  "Alcyonaria",                  
  "Spermatophyta", "Annelida",                     
  "Scyllaridae",            "Detritus",            
  "Anthozoa",                 NA,                  
  "Tunicata",                   "Arachnida",                   
  "Sipuncula",        "Gastropoda",      
  "Stomatopoda",                  "Gastropoda",             
  "Bivalvia",                   "Gastropoda",                     
  "Paguroidea",                 "Cephalopoda",                    
  "Eggs",              "Algae",                 
  "Organic",     "Detritus",                      
  "Algae",         "Hydrozoa",                    
  "Algae",            "Detritus",    
  "Diatoms",                      "Nemertea",                  
  "Cyanobacteria",              "Crab",                     
  "Tunicata",                        "Insecta",                       
  "Eggs",                "Algae",         
  "Actiniaria",                     "Algae",               
  "Detritus",                    "Inorganic",                        
  "Hydrozoa",               "Bivalvia",                 
  "Chaetognatha",                 NA,               
  "Diatoms",             "Foraminifera",           
  "Detritus",                  "Algae",                  
  NA,             "Detritus",                  
  "Detritus",                     "Algae",       
  "Coralline algae",              "Algae",                
  "Gastropoda",                   NA,                     
  "Ostracoda",                    "Mollusca",                    
  "Chordata",                     "Cephalopoda" ,                
  NA,                        "Foraminifera",           
  "Larvae",                  "Cnidaria",                
  "Polyplacophora",                   "Detritus",               
  "Inorganic",                      "Inorganic",                
  "Sipuncula",           "Nemertea",                  
  NA,               "Detritus",               
  "Ophiuroidea",              "Porifera",                      
  "Porifera",                     "Copepoda",                   
  "Algae",                   "Amphipoda",                  
  "Spermatophyta",                 "Echinoidea",                   
  "Scleractinia",                "Gastropoda",              
  "Cirripedia",          "Alcyonaria",                 
  "Ostracoda",                   "Gastropoda",                 
  "Porifera",                      "Mollusca",                  
  "Hydrozoa",                    "Polychaeta",                
  "Tunicata",                     "Zooplankton",                     
  "Crab",                       "Sipuncula",                
  "Animalia",                      "Detritus",                   
  "Detritus",                           "Algae",         
  "Nematoda",                    "Cirripedia",                  
  "Tunicata",              "Paguroidea",                     
  "Pycnogonida",                 "Tanaidacea",                  
  "Ophiuroidea",                   "Bryozoa",                 
  "Alcyonaria",                   "Crinoidea",                     
  "Zoantharia",                  "Actiniaria",                 
  "Spermatophyta",          "Hemichordata",              
  "Detritus",                  "Holothuria",                
  "Polychaeta",                     "Cephalopoda",                
  "Stomatopoda",           "Arachnida",                    
  "Arachnida",                   "Crustacea",                    
  NA,             "Scaphopoda",                 
  "Scyllaridae",                    "Cephalochordata",            
  "Detritus",                      "Crab",                 
  NA,                       "Crustacea")

grp <- data.frame(avant = grp_err, apres = grp_cor) #create a data frame of before and after correction

write.csv(grp, file="grp_cor.csv") 

grp <-read.csv("grp.csv", sep=",") #export the csv, so i can modify the csv directly
unique(grp)
data_replace <- merge(data, grp[,c("grp_raw","grp")], by="grp_raw") #merge

data_replace <- data_replace[,-32]
names(data_replace)[32] <- "grp"

#Previous grp were incorrect, so new tab based on item_cor, item_raw,grp_raw)
m <-data %>% 
  select(grp_raw, item_raw, item_class, item_ord_cor, item_cor)

mm <- unique(m)

write.csv(mm, "grp_item.csv")
grp_item <-read.csv("grp_item.csv", sep=";") #export the csv, so i can modify the csv directly
grp_item <- grp_item[,c(3,6)]
names(grp_item)[1] <- "grp_new"
grp_item <- (unique(grp_item))


data$item_raw <- as.character(data$item_raw)
grp_item$item_cor<- as.character(grp_item$item_cor)
data$item_cor <- as.character(data$item_cor)

#Check number of unique prey items
data %>% select(item_cor) %>% n_distinct()
grp_item %>% select(item_cor) %>% n_distinct() #same 

data_replace %>% select(grp_new) %>% n_distinct()
grp_item %>% select(grp_new) %>% n_distinct()


#Create new data frame with data and former grp for items
data_replace <- dplyr::left_join(data, grp_item, by = "item_cor") %>% # SOLVED there was pb in single key values. so why merge create much more rows !!!!
                rename(grp = grp)

#Get a new item data frame to create new group base on grp raw and item raw
grp_item2 <-data_replace %>% 
  select(grp_raw, grp, item_raw, item_class, item_ord_cor, item_cor)

grp_item2 <- unique(grp_item2)

write.csv(grp_item2, "grp_item2.csv")

grp_item2 <- read.csv("grp_item2.csv", sep=";")
grp_item2$item_raw <- as.character(grp_item2$item_raw)

grp_item2 <- unique(grp_item2)

#Upload and merge data with grps
data2 <- dplyr::left_join(data, grp_item2[,c("item_raw","grp1","grp2")], by="item_raw") # SOLVED there was pb in single key values. so why merge create much more rows !!!!
data2 <- unique(data2)

data2$grp1 <- as.character(data2$grp1)
data2$grp2 <- as.character(data2$grp2)

grps <- data2 %>% count(grp1, grp2)

write.csv(grps,"nbgrp_data2.csv")

#Pb with the merge so checked for duplicates | SOLVED
all(data$item_raw %in% grp_item2$item_raw)

setdiff(data$item_raw , grp_item2$item_raw)

data %>% select(item_raw) %>% n_distinct()
grp_item2 %>% select(item_raw) %>% n_distinct()

a <- grp_item2 %>% count(item_raw, grp1)
a[duplicated(a$item_raw),]

b <- grp_item2 %>% count(item_raw, grp2)
b[duplicated(b$item_raw),]

rm(a); rm(b)


##Check nb of items for each group per site

per_grp <- data2 %>% group_by(site_code) %>%
  count(grp1, grp2) %>%
  mutate (per = n/sum(n)*100)

few_obs <- per_grp %>% filter(n <=3)        
#pb for virgin islands very few detritus, becoz item as algae&detritus ...
#would be possible to duplicate rows and have a row for algae and one for detritus


##Let's try to duplicate each rows and change item names for the duplicate as detritus
vir.algae_detritus <- data2 %>% filter(grepl("Algae&", item_raw)) #select rows for which item_raw contains ""

dup <- vir.algae_detritus %>% slice(rep(1:n(), each=2)) #duplicate each row

dup$item_cor[c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38)]<- "Detritus" #rename deplicate by "Detritus"

#Try add the new rows
new_data <- rbind(data2, dup[dup$item_cor=="Detritus",])
new_data %>% filter(grepl("Algae&", item_raw),item_cor=="Detritus") %>% 
  select(item_raw, item_cor, grp1,grp2) #to check selection

#Renaming the grp1 and grp 2 if item_cor == Detritus than values = Detritus
new_data <- new_data %>% mutate(grp1 = replace(grp1, item_cor == "Detritus", "Detritus")) %>%
  mutate(grp2 = replace(grp2, item_cor == "Detritus", "Detritus")) 
         