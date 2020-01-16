#Cleaning items - Proper script#
library(readxl)
library(tidyverse)
library(dplyr)
library(tidyr)
library(data.table)

#Load original dataset
data_raw <- read_excel("data/dietdata2.0.xlsx", sheet = "Sheet1", trim_ws=TRUE, na="NA")

#Select only relevant variables
data <- data_raw %>% 
  select(site_code,class,family_cor,fish_sp,genus_cor,sp_cor,item_type,
         item_kingdom, item_phylum , item_class,item_ord_cor,item_fam_cor,
         item_cor,item_gen_cor, item_sp_cor,grp_raw, item_raw, nb_sample,nb_guts ,    
         nb_item ,     item_freq ,   item_volper,item_massper, item_numper,
         mean_SL  ,    min_SL  ,max_SL, min_TL ,max_TL,time,source) 

data <- unique(data)

#Create variable spname and replace "sp" by NA
data$item_spname <- paste(data$item_gen_cor, data$item_sp_cor, " ") #new variable with sp name
data$item_spname <- trimws(data$item_spname, which="both") 
data$item_spname[data$item_spname=="NA NA"] <- NA
data$item_sp_cor[data$item_sp_cor=="sp"] <- NA

#As character
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

#Remove some rows 
data <- filter(data,!is.na(item_cor))
data <- filter(data %>% filter(!item_cor=="Gurry"))  #remove gurry items
data <- filter(data, !item_cor=="Animalia" & !item_cor=="Plantae") 
data <- filter(data, is.na(time) | time == "day")


#Import of first grp file
grp_item <-read.csv("grp_item.csv", sep=";") #export the csv, so i can modify the csv directly
grp_item <- grp_item[,c(3,6)]
names(grp_item)[1] <- "grp_new"
grp_item <- (unique(grp_item))

#Create new data frame with data and former grp for items
data_replace <- dplyr::left_join(data, grp_item, by = "item_cor") %>% # SOLVED there was pb in single key values. so why merge create much more rows !!!!
  rename(grp = grp)

#Get a new item data frame to create new group base on grp raw and item raw
grp_item2 <-data_replace %>% 
  select(grp_raw, grp, item_raw, item_class, item_ord_cor, item_cor)

grp_item2 <- unique(grp_item2)

write.csv(grp_item2, "grp_item3.csv")

grp_item2 <- read.csv("grp_item2.csv", sep=";") #problem coz I modified the grp2 on the excel 
grp_item2$item_raw <- as.character(grp_item2$item_raw)

grp_item2 <- unique(grp_item2)

#Upload and merge data with grps
data2 <- dplyr::left_join(data, grp_item2[,c("item_raw","grp1","grp2","grp3")], by="item_raw") # SOLVED there was pb in single key values. so why merge create much more rows !!!!
data2 <- unique(data2)


#Adding "Detritus" rows for Virgin Islands
##Let's try to duplicate each rows and change item names for the duplicate as detritus
vir.algae_detritus <- data2 %>% filter(grepl("Algae&", item_raw)) #select rows for which item_raw contains ""

dup <- vir.algae_detritus %>% slice(rep(1:n(), each=2)) #duplicate each row

dup$item_cor[c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38)]<- "Detritus_OM" #rename deplicate by "Detritus"

#Try add the new rows
new_data <- rbind(data2, dup[dup$item_cor=="Detritus_OM",])
new_data$grp2 <- as.character(new_data$grp2)
new_data$grp1 <- as.character(new_data$grp1)

new_data %>% filter(grepl("Algae&", item_raw),item_cor=="Detritus_OM") %>% 
  select(item_raw, item_cor, grp1,grp2) #to check selection

#Renaming the grp1 and grp 2 if item_cor == Detritus than values = Detritus
new_data <- new_data %>% mutate(grp1 = replace(grp1, item_cor == "Detritus_OM", "Detritus")) %>%
  mutate(grp2 = replace(grp2, item_cor == "Detritus_OM", "Detritus")) %>%
  mutate(grp3 = replace(grp3, item_cor == "Detritus_OM", "Detritus"))

new_data %>% filter(item_cor=="Detritus_OM")
new_data %>% filter(item_cor=="Inorganic")

#For Solenogastrids_Chitons
new_data %>% filter(grp_raw=="Amphineura")
solchi <- new_data %>% filter(item_raw=="Solenogastrids_Chitons")
new_data <- new_data %>% mutate(grp3 = replace(grp3, item_cor == "Solenogastres", "Solenogastres")) %>%
  mutate(grp2 = replace(grp2, item_cor == "Solenogastres", "Solenogastres")) %>%
  mutate(grp1 = replace(grp1, item_cor == "Solenogastres", "Solenogastres"))

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

#Remove the tricky group rows
#Select variables
new_data_select <- new_data %>% 
  select(site_code, class, family_cor, genus_cor, sp_cor, fish_sp, item_kingdom, item_phylum, item_class,
         item_ord_cor, item_fam_cor, item_gen_cor, item_sp_cor, item_spname, item_cor, item_raw, grp1, grp2, grp3)

#Rename variables and transform data set into a long format
data_transfo <- new_data_select %>% 
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

#PB from data transfo
data_transfo %>% filter(item_cor=="Detritus_OM")
data_transfo %>% filter(item_cor=="Inorganic")

#Get the lowest level by site, sp, based on grp2
library(plyr)
Test <- ddply(data_transfo, .(site_code, fish_sp, grp2), summarize, Lvlmin = max(Level, na.rm = TRUE))

#Merge the data sets to have the lowest level per item
data_merge <- new_data_select %>% 
  merge(., Test, by = c("fish_sp", "site_code","grp2"), all= TRUE)

dim(data_merge)
dim(new_data_select)

new_data_select <- unique(new_data_select)
data_merge <- unique(data_merge)

new_data_select %>% filter(item_raw=="Sand") 
data_merge %>% filter(item_raw=="Sand")
data_merge %>% filter(item_cor=="Detritus_OM")

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

data_merge %>% filter(is.na(fish_sp)) #Okay no Na's values

#Check which rows with diff Lvlmin and row_level diff
diff <- data_merge[!(data_merge$Lvlmin == data_merge$row_level),] 

data_merge %>% filter(item_cor=="Detritus_OM") 
data_merge %>% filter(item_raw=="Sand")

#Removing rows according to Lvlmin and row_level : USE of which and not filter coz removed the Na lines so the detritus
x <- data_merge %>% filter(Lvlmin==7 &row_level==1 & !(grp3 == "Zooplankton"))
x <- which(data_merge$Lvlmin==7 & data_merge$row_level==1 & !(data_merge$grp3 == "Zooplankton"))
data_merge <- data_merge[-x,]

data_merge <- data_merge %>% filter(is.na(Lvlmin) | !(Lvlmin==7 & row_level==2))
#data_merge <- data_merge %>% filter(!(Lvlmin==7 & row_level==3) & item_cor=="Fish_eggs" & item_cor=="Fish_larvae")
a <- diff %>% filter(Lvlmin==7 , row_level==3 , item_cor != "Fish_eggs", item_cor !="Fish_larvae" , item_cor !="Ascidiacea", grp3 != "Zooplankton")
a <- which(data_merge$Lvlmin==7 & data_merge$row_level==3 & !(data_merge$item_cor=="Fish_eggs") & !(data_merge$item_cor=="Fish_larvae") & !(data_merge$item_cor=="Ascidiacea") & !(data_merge$grp3=="Zooplankton"))
data_merge <- data_merge[-a,]

b <- diff %>% filter(Lvlmin==7 , row_level==4 , grp3 != "Zooplankton", grp2 !="Cephalopoda" , grp2 != "Echinoidea" , grp2 !="Copepoda", item_cor !="Caridea")
b <- which(data_merge$Lvlmin==7 & data_merge$row_level==4 & !(data_merge$grp3 == "Zooplankton") & !(data_merge$grp2 =="Cephalopoda") &!(data_merge$grp2 == "Echinoidea") & !(data_merge$grp2 =="Copepoda") &!(data_merge$item_cor =="Caridea"))
data_merge <- data_merge[-b,]

c <- diff %>% filter(Lvlmin==6 , row_level==4, grp2 != "Isopoda" , grp2!= "Polychaeta" , grp3 != "Zooplankton",grp2 != "Fish", item_cor != "Caridea", item_cor != "Paguroidea")
c <- which(data_merge$Lvlmin==6 & data_merge$row_level==4 & !(data_merge$grp3 == "Zooplankton") & !(data_merge$grp2 =="Isopoda") & !(data_merge$grp2 == "Polychaeta") & !(data_merge$grp2 =="Fish") & !(data_merge$item_cor == "Caridea") & !(data_merge$item_cor == "Paguroidea"))
data_merge <- data_merge[-c,]

d <- diff %>% filter(Lvlmin==6 , row_level==1)
d <- which(data_merge$Lvlmin==6 & data_merge$row_level==1)
data_merge <- data_merge[-d,]

e <- diff %>% filter(Lvlmin==6 , row_level==2)
e <- which(data_merge$Lvlmin==6 & data_merge$row_level==2)
data_merge <- data_merge[-e,]

f <- diff %>% filter(Lvlmin==6 , row_level==3, grp3 != "Zooplankton", item_cor != "Porites")
f <- which(data_merge$Lvlmin==6 & data_merge$row_level==3 & data_merge$grp3 != "Zooplankton" & data_merge$item_cor != "Porites")
data_merge <- data_merge[-f,]

g <- diff %>% filter(Lvlmin==5 , row_level==2)
g <- which(data_merge$Lvlmin==5 & data_merge$row_level==2)
data_merge <- data_merge[-g,]

g <- diff %>% filter(Lvlmin==5 , row_level==3, grp3 != "Zooplankton", grp3 != "Tunicata")
g <- which(data_merge$Lvlmin==5 & data_merge$row_level==3 & data_merge$grp3 != "Zooplankton" & data_merge$grp3 != "Tunicata")
data_merge <- data_merge[-g,]

h <- diff %>% filter(Lvlmin==5 , row_level==4 , item_cor == "Shrimp" | item_cor=="Crab" |  item_cor == "Amphipoda") %>%
  filter(grp3!= "Zooplankton" & grp3!= "Polychaeta")
h <- which(data_merge$Lvlmin==5 & data_merge$row_level==4 & !(data_merge$grp3 == "Zooplankton") & !(data_merge$grp3 == "Polychaeta") & (data_merge$item_cor == "Shrimp" | data_merge$item_cor=="Crab" |  data_merge$item_cor == "Amphipoda"))
data_merge <- data_merge[-h,]

i <- diff %>% filter(Lvlmin==4 , row_level==3, grp3 != "Gastropoda" & item_raw == "Copepods" | item_raw=="Insecta")
i <- which(data_merge$Lvlmin==4 & data_merge$row_level==3 & !(data_merge$grp3 == "Gastropoda") & (data_merge$item_raw == "Copepods" | data_merge$item_raw=="Insecta"))
data_merge <- data_merge[-i,]

sp <- data_merge %>% filter(fish_sp == "Plesiops nigricans")


#Export a clean data set
data_clean <- data_merge %>% select(-Lvlmin, -row_level)

write.csv(data_clean, "data/data_clean.csv")

data_clean %>% filter(item_cor=="Detritus_OM")
data_clean %>% filter(item_cor=="Inorganic")
data_clean %>% filter(item_raw=="Sand")


#Create a unique data set for groups
data_clean_grp <- data_clean %>% select(item_raw, item_class, item_ord_cor, grp1, grp2, grp3)
names(data_clean)
data_clean_grpunique <- unique(data_clean_grp)
write.csv(data_clean_grpunique, "grp_item_data_clean.csv")
rm(data_clean_grpunique)

#Reimport a new data with gp4
grp_item_data_clean <- read.csv2("grp_item_data_clean.csv")

#Merge grp4 with data clean
grp_item_data_clean <- grp_item_data_clean %>% 
  mutate_if(is.factor, as.character) %>% glimpse()

data_clean <- data_clean %>% 
  mutate_if(is.factor, as.character) %>% glimpse()

to_merge <- grp_item_data_clean %>% select(item_raw, grp1, grp2, grp3, grp4)


