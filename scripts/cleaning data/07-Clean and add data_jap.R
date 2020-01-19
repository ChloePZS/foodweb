##############################
#Clean and add Sano's dataset#
##############################

library(readxl)
library(tidyverse)
library(stringr)
library(rfishbase)
library(taxize)

# 1. Import data and change variables names
data_jap <- read_excel("data/Data_Sano.xlsx", na="NA", trim_ws = TRUE) %>% select(-item_phylum, -item_class, -item_ord, -item_fam, -item_gen, -item_sp)

# 2. Get item tax
jap_item <- data_jap %>% select(item_raw) %>% unique() 

write.csv(jap_item, "data/jap_item.csv") #save list of item to add tax

jap_item2 <- read_excel("data/jap_item.xlsx")

# 3. add grouping "prey_grp.csv" file
prey_grp <- read.csv("prey_grp.csv", sep=";") %>% select(-item_phylum, -item_class, -item_ord)
jap_item2 <- left_join(jap_item2, prey_grp, by =c("item_raw")) %>% unique()

write.csv(jap_item2, "data/jap_item2.csv")

jap_item3 <- read.csv("data/jap_item2.csv", sep=";")

# 4. add item on the dataset
data_jap <- left_join(data_jap, jap_item3, by="item_raw")

#some corrections on the item_raw
data_jap <- data_jap %>% mutate(item_raw = replace(item_raw, item_raw == "Gammaridean_amphipods", "Gammaridean amphipods"))
data_jap <- data_jap %>% mutate(item_raw = replace(item_raw, item_raw == "Sedentary_polychaetes", "Polychaetes sedentary"))
data_jap <- data_jap %>% mutate(item_raw = replace(item_raw, item_raw == "Gastropds", "Gastropods"))
data_jap <- data_jap %>% mutate(item_raw = replace(item_raw, item_raw == "Gastropdss", "Gastropods"))

# 5. removing unrelevant items
data_jap <- data_jap %>% filter(!is.na(item_raw)) #remove NA
data_jap <- data_jap %>% filter(!item_raw %in% c("Unid_frags", "Decapod_frags", "Crustacea_frags", "Calcareous powder"))


# 6. remove species with less than 3 guts sampled
data_jap$sp_name <- paste(data_jap$genus, data_jap$sp)
data_jap %>% filter(nb_guts < 3) %>% count(sp_name) #50 espèces with nb_guts < 3

data_jap %>% count(sp_name) #184 sp

data_jap <- data_jap %>% filter(nb_guts >= 3)

# 7.  check the fish species name

fish_sp<-unique(data_jap$sp_name)
sp_correct<-suppressWarnings(rfishbase::validate_names(fish_sp))
sp_errors<-fish_sp[!fish_sp %in% sp_correct]

length(unique(sp_correct)) #124
length(unique(sp_errors)) #33
length(unique(fish_sp)) #134

print(sp_errors)

corrected <- c("Neoniphon sammara", "Ostorhinchus aroubiensis","Ostorhinchus cyanosoma",                   
 "Nectamia fusca", "Ostorhinchus cookii","Labracinus melanotaenia",            
 "Labracinus cyclophthalmus","Pseudochromis tapeinosoma","Scolopsis lineata",              
 "Gnathodentex aureolineatus","Lethrinus genivittatus","Plectorhinchus diagrammus",          
 "Neosynchiropus ocellatus", "Atrosalarias holomelas","Asterropteryx semipunctata",        
 "Valenciennea longipinnis","Gnatholepis sp","Ptereleotris microlepis",
 "Chromis caerulea","Chrysiptera brownriggii","Neoglyphidodon melas",              
 "Pomacentrus chrysurus","Cheilio inermis","Thalassoma hardwicke",              
 "Scarus prasiognathos","Chlorurus sordidus","Scarus psittacus",                     
 "Scarus sp", "Naso sp", "Zebrasoma veliferum",                
 "Siganus punctatus","Rhinecanthus aculeatus", "Arothron nigropunctatus")


## Replace the errors
data_jap$fish_sp <- data_jap$sp_name
data_jap$fish_sp[data_jap$fish_sp%in%sp_errors] <- sapply(data_jap$fish_sp[data_jap$fish_sp%in%sp_errors],FUN = function(x){
  line <- which(sp_errors==x)
  y <- gsub(sp_errors[sp_errors==x],corrected[line],x = x)
  print(paste("correcting ",x))
  return(y)})

length(unique(data_jap$fish_sp))
[1] 132
length(unique(data_jap$sp_name))
[1] 134

#2 species were actually the same one


#Check the families and classes of the predators fish sp
library(acs)
api.key.install("748f08277d8b379af2d4d27561b458c9c108", file = "key.rda")
Sys.setenv(ENTREZ_KEY = "748f08277d8b379af2d4d27561b458c9c108")

fish_tax <- tax_name(data_jap$fish_sp, get=c("family", "order"), db="ncbi")

fish_tax <- fish_tax %>% rename(., fish_sp = query, family_cor = family)

data_jap <- data_jap %>% left_join(., fish_tax[,c(2,3)], by = "fish_sp") %>% unique() #add the corrected family names

#Check where couldn't find family and difference with original family variable
which(data_jap$family != data_jap$family_cor)
data_jap %>% filter(is.na(family_cor)) %>% select(fish_sp, family, family_cor) %>% unique()

#Replace NA by original family names
data_jap <- data_jap %>% mutate(family_cor = coalesce(family_cor, family))

data_jap <- data_jap %>% mutate(family_cor = replace(family_cor, family == "Scaridae", "Labridae")) #change scarids into labrids

data_jap %>% filter(family == "Scaridae") %>% select(family, family_cor) %>% unique()

#add variables for site_code and class
data_jap$site_code <- "jap"
data_jap$class <- "Actinopterygii"
data_jap$nb_sample <- data_jap$nb_guts + data_jap$nb_empty

#replace + by NA
data_jap <- data_jap %>% mutate(item_volper = na_if(item_volper, "+")) 
data_jap$item_volper <- as.numeric(data_jap$item_volper)

#Problem is for item_volper some items with "+" so considered as NA. If species only item_volper then not all items have info
#Should consider only species with itemfreq for quantative matrices?
#But then could the full data set for qualitative matrices ? coz loss of 24 sp when removing ones with is.na(item_freq)

data_jap %>% filter(is.na(item_volper) & is.na(item_freq)) %>% group_by(fish_sp) %>% summarise(n = n_distinct(fish_sp)) #12 species without item_freq and with "+"

data_jap %>% filter(!is.na(item_freq)) %>% group_by(fish_sp) %>% summarise(n = n_distinct(fish_sp)) #118 sp


#Error item_freq > 1
data_ISfull_grp_final %>% filter(item_freq > 1) %>% select(fish_sp)

data_ISfull_grp_final %>% filter(fish_sp == "Stethojulis bandanensis" & site_code =="jap") 

data_jap <- data_jap %>% mutate(item_freq = replace(item_freq, item_freq == 2, 1))

#Add data_jap to full data set
data_ISfull_grp_final <- plyr::rbind.fill(data_jap[,-c(1,6,7,18)], data_ISfull_grp_final)
names(data_ISfull_grp_final)
data_ISfull_grp_final <- data_ISfull_grp_final[, c(17, 18, 16, 15, 1,2, 19, 3,4,5,20, 9:12, 21,22, 6:8, 13,14, 23,24)]

length(unique(data_ISfull_grp_final$family_cor))


data_ISfull_grp_final %>% filter(is.na(item_freq)) %>% group_by(site_code) %>% summarise(n = n_distinct(fish_sp))

#rm is.na(item_freq) for JAP
data_jap2 <- data_jap %>% filter(!is.na(item_freq))

data_ISfull_grp_final <- data_ISfull_grp_final %>% filter(site_code != "jap")

data_ISfull_grp_final2 <- plyr::rbind.fill(data_jap2[,-c(1,6,7,9,18)],data_ISfull_grp_final)
data_ISfull_grp_final2 <- data_ISfull_grp_final2[, c(16,17,15,14,1,2,18,3,4,5,8:11,22,19,6,21,7,12,13)]

write.csv(data_ISfull_grp_final2, "data/data_ISfull_grp_final.csv")


#Some errors in species name .... and pred class as Insecta ... !!!! For fucksakes !!
data_ISfull_grp_final2$fish_sp <-   dplyr::recode(data_ISfull_grp_final2$fish_sp, 
                                                  "Neomyxus chaptalii" = "Neomyxus leuciscus",
                                                  "Zebrasoma veliferum" = "Zebrasoma velifer",
                                                  "Epinephelus macrosplos" = "Epinephelus macrospilos",
                                                  "Labracinus melanotaenia" = "Labracinus cyclophthalmus",
                                                  "Sargocentron spniferum" = "Sargocentron spiniferum",
                                                  "Parapercis cephalopunctata" = "Parapercis millepunctata",
                                                  "Tylosurus acus" = "Tylosurus acus acus",
                                                  "Archamia fucata" = "Taeniamia fucata",
                                                  "Chromis caerulea" = "Chromis ternatensis",
                                                  "Arothron hispdus" = "Arothron hispidus",
                                                  "Dasyatis americana" = "Hypanus americanus" # this is a stingray btw
)


data_ISfull_grp_final2$class <-   dplyr::recode(data_ISfull_grp_final2$class, "Insecta" = "Actinopterygii")

glimpse(data_ISfull_grp_final2)

