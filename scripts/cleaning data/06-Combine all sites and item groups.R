###########################################################
#Adding New Caledonia + final prey groups + final cleaning#
###########################################################


library(tidyverse)

  #1. Import last versions of the datasets : no item_freq or volume
nca <- read.csv("data/data_clean_nc.csv") %>%
  select(-X)

nca <- nca %>% select(Family,item_raw,   item_family, item_ord,    
                     item_class,  item_phylum, item_kingdom,site_code,   class,       
                     fish_sp,     genus_cor,   sp_cor) %>%
  dplyr::rename(family = Family, item_fam = item_family)
nca <- unique(nca)

names(nca)
data_clean2 <- read.csv("data/data_clean.csv") %>%
  select(-X) %>%
  dplyr::rename(family = family_cor, item_ord = item_ord_cor, item_fam = item_fam_cor) %>%
  dplyr::select(fish_sp,      site_code,    class,        family,       genus_cor,    sp_cor,      
                item_kingdom, item_phylum,  item_class,   item_ord,     item_fam,     item_gen_cor, item_sp_cor,  item_spname,  item_cor,     item_raw)
names(data_clean2)

data_clean2 %>% filter(item_cor=="Inorganic") #OK

  #2. Adding new caledonia rows
dim(nca)
dim(data_clean2)
data_full <- plyr::rbind.fill(data_clean2,nca)
data_full <- unique(data_full)


data_full <- data_full %>% 
  mutate_if(is.factor, as.character) %>% glimpse()

  #3. Get groups
#There will be an issue with item_raw == "Palinuridae/Scyllaridae
data_full <- data_full %>% mutate(item_raw = replace(item_raw, item_fam == "Palinuridae", "Palinuridae"),
                                  item_raw = replace(item_raw, item_fam == "Scyllaridae", "Scyllaridae"))

data_full %>% filter(item_raw=="Palinuridae/Scyllaridae") %>% select(fish_sp) #Check if renamed properly

#There will be an issue with item_raw == "Solenigastrids/chitons"
data_full %>% filter(item_raw == "Solenogastrids")

data_full <- data_full %>% mutate(item_raw = replace(item_raw, item_cor =="Polyplacophora", "Chitons"),
                                  item_raw = replace(item_raw, item_cor =="Solenogastres", "Solenogastrids"))


grp_full <- data_full %>% select(site_code,item_raw, item_class, item_ord, grp1, grp2, grp3, grp4)

grp_full <- unique(grp_full)

write_csv2(grp_full, "grp_full.csv")
rm(grp_full)


  #3. Prepare item groups that would fit all 5 sites
grp_full2 <- read.csv2("grp_full.csv") #Possible to change the groups on the csv file and re upload
glimpse(grp_full2)
grp_full2<- grp_full2 %>% 
  mutate_if(is.factor, as.character) %>% glimpse()

grp_full2 <- unique(grp_full2)

grp_full2 <- grp_full2 %>% select(item_raw, site_code,grp1,grp3,grp4,grp5, grp6, grp7)

data_full2 <- left_join(data_full, grp_full2, by = c("site_code","item_raw"), all.x=TRUE)
data_full2 <- unique(data_full2)

write.csv(data_full2, "data/data_full.csv")

#Problem coz Algae & Detritus only as Detritus ...not really a problem here coz quantitative data
#Possible to duplicate rows...Need to check with Vale and Jordan
vir.alg_det <- data_full2 %>% filter(grepl("Algae&", item_raw)) #select rows for which item_raw contains ""

dup <- vir.alg_det %>% slice(rep(1:n(), each=2)) #duplicate each row

dup$grp6[c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38)]<- "Benthic autotroph" #rename deplicate by "Detritus"
dup$grp5[c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38)]<- "Benthic autotroph"
dup$grp7[c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38)]<- "Benthic autotroph"
dup$grp4[c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38)]<- "Benthic autotroph"
dup$grp3[c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38)]<- "Benthic autotroph"
dup$grp1[c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38)]<- "Benthic autotroph"


#Remove the modified rows and add the new rows

data_full2 <- rbind(data_full2, dup)

data_full2 %>% filter(site_code=="vir", grepl("Algae&", item_raw)) %>% select(grp6)
data_full2_grp6 %>% filter(site_code=="vir", grepl("Algae&", item_raw)) %>% select(grp6)

data_full2_grp6 <- data_full2 %>% select(-grp1, -grp3, -grp4, -grp5, -grp7)


#Check with Worms but from data_ISfull 
data_ISfull%>% filter(genus =="Scarus")#Ok changed to labridae
data_ISfull %>% filter(grp6 == "Worm")

names(data_ISfull)

#Need to change the Worms
item_worm <- data_ISfull %>% filter(grp6 == "Worm") %>% mutate_if(is.factor, as.character)

item_worm <- item_worm %>% mutate(item = if_else(is.na(item) & !is.na(item_class) , item_class, item_phylum)) %>%
  mutate(item = replace(item, is.na(item), "Worms")) %>% 
  mutate(item = replace(item, item_class == "Polychaeta", item == "Polychaeta")) %>% 
  mutate(item = replace(item, item_raw =="Aspidosiphon_sp", item_class == "Phascolosomatidea")) %>%
  mutate(item = replace(item, item_raw =="Aspidosiphon_sp", item_phylum == "Sipuncula")) %>%
  mutate(item = replace(item, item_raw =="Aspidosiphon_sp", item == "Sipuncula")) #didn't work for the last lines so did it on excel directly

data_ISfull <- data_ISfull %>% filter(grp6 != "Worm") %>%
  rbind(item_worm)

data_ISfull %>% filter(grp6 == "Worm") %>% group_by(item) %>% summarise(n = n()) %>% mutate(per = n/sum(n)*100) #Percentage of dietary item as worms

#export corrected dataset
write.csv(data_ISfull, "data/data_ISfull.csv")


  #4. Check number of species and item tax per site #####
####Check the % items ID per site#### Datafull2 is qualitative dataset
#haw
haw <- data_full2 %>% filter(site_code=="haw")

haw_tax <- haw %>% select(8:13) %>%
  summarise_all(funs(sum(!is.na(.))))

haw_per <- round(haw_tax*100/nrow(haw), 2)

#mad
mad <- data_full2%>% filter(site_code=="mad")

mad_tax <- mad %>% select(8:13) %>%
  summarise_all(funs(sum(!is.na(.))))

mad_per <- round(mad_tax*100/nrow(mad), 2)

#vir
vir <- data_full2%>% filter(site_code=="vir")

vir_tax <- vir %>% select(8:13) %>%
  summarise_all(funs(sum(!is.na(.))))

vir_per <- round(vir_tax*100/nrow(vir), 2)


#mari
mari <- data_full2%>% filter(site_code=="mari")

mari_tax <- mari %>% select(8:13) %>%
  summarise_all(funs(sum(!is.na(.))))

mari_per <- round(mari_tax*100/nrow(mari), 2)

#nca
nca <- data_full2%>% filter(site_code=="nca")

nca_tax <- nca %>% select(8:13) %>%
  summarise_all(funs(sum(!is.na(.))))

nca_per <- round(nca_tax*100/nrow(nca), 2)

tax <- bind_rows(mad_per, haw_per, vir_per, mari_per, nca_per) %>%
  rownames_to_column(var="site_code") %>%
  mutate(site_code = recode(site_code, "1" = "mad" , "2" = "haw","3" = "vir" , "4"="mari", "5"="nca"))

write.table(tax, "tax.txt", sep="\t")

####Check the nb of predators species and families per site####

plyr::ddply(data_full2_grp6, ~site_code, summarize, 
            n_fish_sp = length(unique(fish_sp)),
            n_fish_fam = length(unique(family)))

plyr::ddply(data_ISfull_grp6, ~site_code, summarize, 
            n_fish_sp = length(unique(fish_sp)),
            n_fish_fam = length(unique(family_cor)))

length(unique(data_ISfull_grp6$fish_sp))
length(unique(data_ISfull_grp6$family_cor))

full_tax <- data_ISfull_grp6 %>% select(9:14) %>%
  summarise_all(funs(sum(!is.na(.))))

full_per <- round(full_tax*100/nrow(data_ISfull_grp6), 2)

unique(data_ISfull_grp6$grp6[order(data_ISfull_grp6$grp6)])

length(unique(data_raw$item_raw))

#Less species as unrelevant items removed
plyr::ddply(data_full, ~site_code, summarize, 
            n_fish_sp = length(unique(fish_sp)),
            n_fish_fam = length(unique(family)))

plyr::ddply(new_data, ~site_code, summarize, 
            n_fish_sp = length(unique(fish_sp)),
            n_fish_fam = length(unique(family_cor)))

plyr::ddply(data_raw, ~site_code, summarize, 
            n_fish_sp = length(unique(fish_sp)),
            n_fish_fam = length(unique(family_cor)))

length(unique(data_raw$fish_sp))

families_list <- c(data_raw$family_cor, nca$family)
length(unique(families_list))

species_list <- c(data_raw$fish_sp, nca$fish_sp)
length(unique(species_list))

data_raw %>% filter(item_raw == "Spermatophyta")%>% select(item_volper)
data_vir %>% filter(item_raw == "Spermatophyta")%>% select(item_volper)
data_vir_grp %>% filter(item_raw == "Spermatophyta")%>% select(item_volper, grp6)



#ERROR FAMILIES FOR 4 SPECIES
data_ISfull %>% filter(fish_sp == "Balistapus undulatus") %>% select(family_cor, site_code) %>% unique(.)
data_ISfull %>% filter(fish_sp == "Gobioclinus guppyi") %>% select(family_cor, site_code) %>% unique(.)
data_ISfull %>% filter(fish_sp == "Plectroglyphidodon dickii") %>% select(family_cor, site_code) %>% unique(.)
data_ISfull %>% filter(fish_sp == "Plectroglyphidodon lacrymatus") %>% select(family_cor, site_code) %>% unique(.)

plyr::ddply(data_ISfull, ~site_code, summarize, 
            n_fish_sp = length(unique(fish_sp)),
            n_fish_fam = length(unique(family_cor)))

#Cleaning data
data_ISfull$family_cor <- as.character(data_ISfull$family_cor)
data_ISfull <- data_ISfull %>% mutate(family_cor = replace(family_cor, fish_sp == "Balistapus undulatus", "Balistidae"),
                                      family_cor = replace(family_cor, fish_sp == "Gobioclinus guppyi","Labrisomidae"),
                                      family_cor = replace(family_cor, fish_sp =="Plectroglyphidodon dickii","Pomacentridae"),
                                      family_cor = replace(family_cor, fish_sp == "Plectroglyphidodon lacrymatus","Pomacentridae"))

write.csv(data_ISfull, "data/data_ISfull.csv")


#Re-open data_ISfull.csv (some modifications on excel...)

data_ISfull <- read.csv("data/data_ISfull.csv", sep =";") %>%
  mutate_if(is.factor, as.character)

#Do the group on excel
data_ISfull %>% select(item_raw, item, item_phylum, item_class, item_ord, item_fam, site_code, grp1, grp3, grp6) %>%
  unique(.)%>%
write.csv(., "grp_dataISfull.csv")

#Get the group variables
grp_dataISfull <- read.csv("grp_dataISfull.csv", sep = ";")  %>%
  mutate_if(is.factor, as.character)

data_ISfull_grp <- data_ISfull %>% select(-grp1,-grp3,-grp6) %>% mutate(id = row_number()) %>%
  left_join(., grp_dataISfull[,c(1,2,7,8,9,10)], by=c("item_raw","site_code")) %>% 
  unique(.) 

data_ISfull_grp$id[duplicated(data_ISfull_grp$id)] #check which lines are duplicated

write.csv(data_ISfull_grp, "data/data_ISfull_grp.csv") # then check on excel

data_ISfull_grp  <- data_ISfull_grp %>% filter(!id %in% c("1240","1242","1244","1246","1248","1250","1252","1254","1256",
                                     "1258","1260","1262","1264","1266","1268","1270","1272","1274"))

data_ISfull_grp  <- data_ISfull_grp %>% filter(!id == "1961" & !grp1 == "Polyplacophora" | !id == "1962" & !grp1 == "Solenogastres" ) 

#remove Sp_Unid
data_ISfull_grp  <- data_ISfull_grp %>% filter(!grp1 == "Unid") %>% select(-id)

#get a clean item column 
data_ISfull_grp <- read.csv("data/data_ISfull_grp.csv", sep =";") 

#Scaridae family to Labridae
data_ISfull_grp <- data_ISfull_grp %>% mutate(family_cor = replace(family_cor, family_cor == "Scaridae", "Labridae"))

#Some corrections
data_ISfull_grp <- data_ISfull_grp %>% mutate(item_ord = replace(item_ord, grp1 == "Amphipoda", "Amphipoda"))


###Check proportion of the items falling into broad categories
data_ISfull_grp %>% group_by(item_class) %>%
  count(item_class) %>%
  as.data.frame(.) %>%
  mutate(per  = n/sum(n)*100)


data_ISfull_grp %>% group_by(item_ord) %>%
  count(item_ord) %>%
  as.data.frame(.) %>%
  mutate(per  = n/sum(n)*100) # 51% with no order

data_ISfull_grp %>% group_by(item_fam) %>%
  count(item_fam) %>%
  as.data.frame(.) %>%
  mutate(per  = n/sum(n)*100) # 85% with no family

data_ISfull_grp %>% filter(is.na(item_fam) & !is.na(item_ord)) #1969 of items that can't be assigned further than order

data_ISfull_grp %>% filter(grp6 != "Detritus" , grp6 != "Inorganic" , grp7 != "Eggs_larvae")

#Check for Anthozoa
data_ISfull_grp %>% filter(item_class == "Anthozoa") %>% group_by(item_ord) %>%
  count(item_ord) %>%
  as.data.frame(.) %>%
  mutate(per = n/sum(n)*100) # 19% of item as "Anthozoa" only


data_ISfull_grp %>% filter(item_class == "Gastropoda") %>% group_by(item_ord) %>%
  count(item_ord) %>%
  as.data.frame(.) %>%
  mutate(per = n/sum(n)*100) # 81% of item as "Gastropoda" only


write.csv(data_ISfull_grp %>% select(item_raw, item_phylum,item_class, item_ord, grp1, grp6) %>% unique(.),
          "prey_grp.csv")


#Remove singletones & doubletones
 data_ISfull_grp %>% select(grp6) %>% group_by(grp6) %>% count(grp6) %>% data.frame(.) %>%
   filter(n < 3)

 
data_ISfull_grp_final <- data_ISfull_grp %>% filter(grp6 != "Brachiopoda" , grp6 != "Cephalochordata" , 
                                                      grp6 != "Crinoidea" , grp6 != "Sea turtles")

write.csv(data_ISfull_grp_final, 'data/data_ISfull_grp_final.csv')

#Noticed for madagascar and hawai, item_massper and item_volper were missing soooooo could combine
#doesn't work
a <- left_join(data_ISfull_grp_final, data_haw[,c("fish_sp","item_raw","item_freq","item_volper","site_code")],by=c("fish_sp","item_raw","item_freq","site_code"), all.x = TRUE) %>% unique()
a <- left_join(data_ISfull_grp_final, data_mad[,c("fish_sp","item_raw","item_freq","item_massper","site_code")],by=c("fish_sp","item_raw","item_freq","site_code"), all.x = TRUE) %>% unique()
