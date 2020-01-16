#####################
#Cleaning items rows#
#####################

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
datatest <- new_data

#datatest$item_cor <- data$item_spname #get rid of the item_cor variable as was a mix of all items col
#datatest <- datatest %>% select(-item_spname) %>% rename(item_spname = item_cor)
 

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

write.csv(grp_item2, "grp_item3.csv")

grp_item2 <- read.csv("grp_item2.csv", sep=";")
grp_item2$item_raw <- as.character(grp_item2$item_raw)

grp_item2 <- unique(grp_item2)

#Upload and merge data with grps
data2 <- dplyr::left_join(data, grp_item2[,c("item_raw","grp1","grp2","grp3")], by="item_raw") # SOLVED there was pb in single key values. so why merge create much more rows !!!!
data2 <- unique(data2)

data2$grp1 <- as.character(data2$grp1)
data2$grp2 <- as.character(data2$grp2)
data2$grp3 <- as.character(data2$grp3)

grps <- data2 %>% count(grp1, grp2)

write.csv(grps,"nbgrp_data2.csv")

#SOLVED | Pb with the merge so checked for duplicates | SOLVED
#all(data$item_raw %in% grp_item2$item_raw)
#setdiff(data$item_raw , grp_item2$item_raw)

#data %>% select(item_raw) %>% n_distinct()
#grp_item2 %>% select(item_raw) %>% n_distinct()

a <- grp_item2 %>% count(item_raw, grp3)
a[duplicated(a$item_raw),]

#rm(a); rm(b)


##Check nb of items for each group per site
per_grp <- data2 %>% group_by(site_code) %>%
  count(grp1, grp2) %>%
  mutate (per = n/sum(n)*100)

few_obs <- per_grp %>% filter(n <=3)        
#pb for virgin islands very few detritus, becoz item as algae&detritus ...
#would be possible to duplicate rows and have a row for algae and one for detritus
#same for solenogastrids_chitons


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
  mutate(grp2 = replace(grp2, item_cor == "Detritus", "Detritus")) %>%
  mutate(grp3 = replace(grp3, item_cor == "Detritus", "Detritus"))


#For Solenogastrids_Chitons
new_data %>% filter(grp_raw=="Amphineura")
solchi <- new_data %>% filter(item_raw=="Solenogastrids_Chitons")
new_data <- new_data %>% mutate(grp3 = replace(grp3, item_cor == "Solenogastres", "Solenogastres"))

#Remove some unrelevant rows
new_data <- filter(new_data, !item_raw=="Spermatophyta") #remove rows for which item_raw is spermtophyta
new_data <- filter(new_data, !item_raw=="Crustacea")
new_data <- filter(new_data, !item_raw=="Crustacea_unid")
new_data <- filter(new_data, !item_raw=="Crustacea_frags")
new_data <- filter(new_data, !item_raw=="Crustacea_parts")
new_data <- filter(new_data, !item_raw=="Coelenterata_polyps")
new_data <- filter(new_data, !item_raw=="Echinoderms remains")
new_data <- filter(new_data, !item_raw=="Mollusk_shells")
new_data <- filter(new_data, !item_raw=="Mollusk_crushed")
new_data <- filter(new_data, !(item_raw=="Algae&orgdetritus" & item_cor=="Algae"))
new_data <- filter(new_data, !(item_raw=="Algae&detritus" & item_cor=="Algae"))

  ##Remove duplicated info
datatest <- new_data
datatest<- datatest %>%
  mutate(item_cor=recode(item_cor,"Fish"="Actinopterygii")) %>% #rename 
  mutate(lower_level = case_when(
    !is.na(item_spname) ~ "Species",
    item_cor==item_fam_cor ~ "Family", 
    item_cor==item_ord_cor ~ "Order",
    item_cor==item_class ~ "Class",
    item_cor==item_phylum ~ "Phylum")) #create a variable of the last levels

lower_level_na <- datatest %>% filter(is.na(lower_level)) 
unique(lower_level_na$item_cor)
unique(lower_level_na$grp3)

echinoidea <- datatest %>% filter(item_cor=="Echinoidea")

rm(echinoidea)

datatest <- datatest %>% mutate(lower_level = replace(lower_level, is.na(lower_level) & grp3 == "Malacostraca", "Class")) %>% 
  mutate(lower_level = replace(lower_level, is.na(lower_level) & grp3 == "Hexanauplia", "Class")) %>%
  mutate(lower_level = replace(lower_level, is.na(lower_level) & grp3 == "Actinopterygii", "Class")) %>%
  mutate(lower_level = replace(lower_level, is.na(lower_level) & grp3 == "Polychaeta", "Class")) %>%
  mutate(lower_level = replace(lower_level, is.na(lower_level) & grp3 == "Tunicata", "SubPhylum")) %>%
  mutate(lower_level = replace(lower_level, is.na(lower_level) & grp3 == "Arachnida", "Class")) %>%
  mutate(lower_level = replace(lower_level, is.na(lower_level) & grp3 == "Ostracoda", "Class")) %>% 
  mutate(lower_level = replace(lower_level, is.na(lower_level) & grp3 == "Anthozoa", "Class")) %>%
  mutate(lower_level = replace(lower_level, is.na(lower_level) & grp3 == "Gastropoda", "Class")) %>%
  mutate(lower_level = replace(lower_level, is.na(lower_level) & grp3 == "Foraminifera", "Phylum")) %>%
  mutate(lower_level = replace(lower_level, is.na(lower_level) & grp3 == "Echinoidea", "Class"))
  
write.csv(datatest,"datatest.csv")

##Pb is to remove the duplicate rows, and those that carry the same info under higher taxonomic level
#Try 1 : create a variable that would have the lowest tax level for each species, then remove 
#the rows of higher level if lower level present
datatest$lower_level
ord <- c("Species","Family","Order","Class","SubPhylum","Phylum")
datatest$lower_level <- factor(datatest$lower_level, levels=ord, ordered=TRUE) #creating a factor with specified levels

class(datatest$lower_level)

min(datatest$lower_level, na.rm=T)

?ddply
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
#Select variables
datatest_select <- datatest %>% 
  select(site_code, class, family_cor, genus_cor, sp_cor, fish_sp, item_kingdom, item_phylum, item_class,
         item_ord_cor, item_fam_cor, item_gen_cor, item_sp_cor, item_spname, item_cor, item_raw, grp1, grp2, grp3)

#Rename variables and transform data set into a long format
data_transfo <- datatest_select %>% 
  dplyr::rename(Kingdom = item_kingdom, Phylum = item_phylum, Class = item_class, Order = item_ord_cor,
                Family = item_fam_cor, Genus = item_gen_cor, Species = item_sp_cor) %>% 
  gather("Kingdom", "Phylum", "Class", "Order",
         "Family", "Genus", "Species", key = "Level", 
         value = "Tax_id") %>% 
  mutate(Level = str_replace_all(Level, c("Kingdom" = "1", 
                                          "Phylum" = "2",
                                          "Class" = "3",
                                          "Order" = "4",
                                          "Family" = "5",
                                          "Genus" = "6",
                                          "Species" = "7"))) %>% 
  mutate(Level = as.numeric(Level)) %>%
  filter(!is.na(Tax_id))

#Get the lowest level by site, sp, and grp
library(plyr)
Test <- ddply(data_transfo, .(site_code, fish_sp, grp2), summarize, Lvlmin = max(Level, na.rm = TRUE))

#Merge the data sets to have the lowest level per item
data_merge <- new_data_select %>% 
  merge(., Test, by = c("fish_sp", "site_code","grp2"), all.x = TRUE)

#data_merge <- unique(data_merge) #With unique : removes nearly 200 rows ....

#Create a variable for the lowest row level
data_merge <- data_merge %>% mutate(row_level = case_when(
    !is.na(item_sp_cor) ~ "7",
    !is.na(item_gen_cor) ~ "6",
    !is.na(item_fam_cor) ~ "5", 
    !is.na(item_ord_cor) ~ "4" ,
    !is.na(item_class) ~ "3",
    !is.na(item_phylum) ~ "2",
    !is.na(item_kingdom) ~ "1")) %>% 
    filter(!is.na(fish_sp))

data_merge$Lvlmin <- as.character(data_merge$Lvlmin)

diff <- data_merge[!(data_merge$Lvlmin == data_merge$row_level),] %>%
  filter(!is.na(fish_sp))


#Removing rows according to Lvlmin and row_level
data_merge <- data_merge %>% filter(!(Lvlmin==7 & row_level==1))
data_merge <- data_merge %>% filter(!(Lvlmin==7 & row_level==2))
#data_merge <- data_merge %>% filter(!(Lvlmin==7 & row_level==3) & item_cor=="Fish_eggs" & item_cor=="Fish_larvae")
a <- which(data_merge$Lvlmin==7 & data_merge$row_level==3 & !(data_merge$item_cor=="Fish_eggs") & !(data_merge$item_cor=="Fish_larvae"))
data_merge <- data_merge[-a,]

b <- diff %>% filter(Lvlmin==7 , row_level==4 , grp2 != "Zooplankton", grp2 !="Cephalopoda" , grp2 != "Echinoidea" , grp2 !="Copepoda")
b <- which(data_merge$Lvlmin==7 & data_merge$row_level==4 & !(data_merge$grp2 == "Zooplankton") & !(data_merge$grp2 =="Cephalopoda") &!(data_merge$grp2 == "Echinoidea") & !(data_merge$grp2 =="Copepoda"))
data_merge <- data_merge[-b,]

c <- diff %>% filter(Lvlmin==6 , row_level==4, grp2 != "Isopoda" , grp2!= "Polychaeta" , grp2 != "Zooplankton",grp2 != "Fish")
c <- which(data_merge$Lvlmin==6 & data_merge$row_level==4 & !(data_merge$grp2 == "Zooplankton") & !(data_merge$grp2 =="Isopoda") & !(data_merge$grp2 == "Polychaeta") & !(data_merge$grp2 =="Fish"))
data_merge <- data_merge[-c,]

d <- diff %>% filter(Lvlmin==6 , row_level==1)
data_merge <- data_merge %>% filter(!(Lvlmin==6 & row_level==1))

e <- diff %>% filter(Lvlmin==6 , row_level==2)
data_merge <- data_merge %>% filter(!(Lvlmin==6 & row_level==2))

f <- diff %>% filter(Lvlmin==6 , row_level==3, grp3 != "Zooplankton")
f <- which(data_merge$Lvlmin==6 & data_merge$row_level==3 & data_merge$grp3 != "Zooplankton")
data_merge <- data_merge[-f,]

g <- diff %>% filter(Lvlmin==5 , row_level==2)
data_merge <- data_merge %>% filter(!(Lvlmin==5 & row_level==2))

g <- diff %>% filter(Lvlmin==5 , row_level==3, grp3 != "Zooplankton", grp3 != "Tunicata")
g <- which(data_merge$Lvlmin==5 & data_merge$row_level==3 & data_merge$grp3 != "Zooplankton" & data_merge$grp3 != "Tunicata")
data_merge <- data_merge[-g,]

h <- diff %>% filter(Lvlmin==5 , row_level==4 , item_cor == "Shrimp" | item_cor=="Crab" |  item_cor == "Amphipoda") %>%
  filter(grp3!= "Zooplankton" & grp3!= "Polychaeta")
h <- which(data_merge$Lvlmin==5 & data_merge$row_level==4 & !(data_merge$grp3 == "Zooplankton") & !(data_merge$grp3 == "Polychaeta") & (data_merge$item_cor == "Shrimp" | data_merge$item_cor=="Crab" |  data_merge$item_cor == "Amphipoda"))
data_merge <- data_merge[-h,]

i <- diff %>% filter(Lvlmin==4 , row_level==3, grp3 != "Gastropoda")
i <- which(data_merge$Lvlmin==4 & data_merge$row_level==3 & !(data_merge$grp3 == "Gastropoda"))
data_merge <- data_merge[-i,]

sp <- data_merge %>% filter(fish_sp == "Labroides dimidiatus")


#Export a clean dataset
data_merge %>% filter(is.na(fish_sp)) #check if no NA
data_clean <- unique(data_merge) %>%
  select(-Lvlmin, -row_level)

write.csv(data_clean, "data/data_clean.csv")

#Need to create another proper grp from grp 2
data_clean_grp <- data_clean %>% select(item_raw, item_class, item_ord_cor, grp1, grp2, grp3)
names(data_clean)

data_clean_grpunique <- unique(data_clean_grp)
write.csv(data_clean_grpunique, "grp_item_data_clean.csv")
rm(data_clean_grpunique)

#Reimport a new data with gp4
grp_item_data_clean <- read.csv2("grp_item_data_clean.csv")

#Merge grp4 with data clean
data_clean2 <- dplyr::left_join(data_clean, grp_item_data_clean)


data_clean %>% filter(item_cor=="Detritus_OM")
data_merge %>% filter(item_cor=="Detritus_OM")
new_data %>% filter(item_cor=="Detritus")
new_data %>% filter(item_cor=="Detritus_OM")
data2 %>% filter(item_cor=="Detritus_OM")
data2 %>% filter(item_cor=="Detritus")
data_replace %>% filter(item_cor=="Detritus_OM")

data_raw %>% filter(item_cor=="Detritus_OM")
