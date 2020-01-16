##########################################################
#Try to do the bipartite plots with quantitative measures#
##########################################################

library(tidyverse)
library(readxl)
library(plyr)
library(bipartite)
library(reshape2)

        #1. Import dataset - Work on dietdata2.0 coz data clean does'nt have the freq/% data
data_raw <- read_excel("data/dietdata2.0.xlsx", sheet = "Sheet1", trim_ws=TRUE, na="NA")

data_raw <- read_excel("data/dietdata2.0.xlsx", 
                       col_types = c("text", "text", "text", 
                                     "text", "text", "text", "text", "text", 
                                     "text", "text", "text", "text", "text", 
                                     "text", "text", "text", "text", "text", 
                                     "text", "text", "text", "text", "text", 
                                     "text", "text", "text", "text", "text", 
                                     "numeric", "numeric", "text", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "text", "text", "text", 
                                     "text", "text", "text", "text", "text"), 
                                     trim_ws=TRUE, na="NA")

#Check families in common for all five sites : 90 families in total, only 17 in common in all three sites
intersect_all <- function(a,b,...){
  Reduce(intersect, list(a,b,...))
}

fam_com <- intersect_all(nca$family, haw$family, vir$family, mad$family, mari$family) #Only 17 families in common between the 5 locations
fam_dif <- unique(data_full2$family[!data_full2$family %in% intersect_all(nca$family, haw$family, vir$family, mad$family, mari$family)])

        #2. Create data subset for each localities with according metrics + cleaning sp < 3 guts + cleaning items
  ####Randall - Virgin Islands - Volumetric %####

#Getting data for VIrgin Islnds and rows for which item_freq not NA
data_vir <- data_raw %>% select(site_code, class, family_cor, fish_sp, nb_sample, nb_empty, item_raw, item_cor, item_phylum, item_class, item_ord_cor, item_fam_cor,item_gen_cor, item_sp_cor, item_volper) %>%
  filter(site_code == "vir", !is.na(item_volper))

#Removing some rows
data_vir <- filter(data_vir,!is.na(item_cor))
data_vir <- filter(data_vir %>% filter(!item_cor=="Gurry"))  #remove gurry items
data_vir <- filter(data_vir, !item_cor=="Animalia")
data_vir <- filter(data_vir, !item_raw=="Crustacea")
data_vir <- filter(data_vir, !item_raw=="Crustacea_unid")
data_vir <- filter(data_vir, !item_raw=="Crustacea_frags")
data_vir <- filter(data_vir, !item_raw=="Crustacea_parts")
data_vir <- filter(data_vir, !item_raw=="Coelenterata_polyps")
data_vir <- filter(data_vir, !item_raw=="Echinoderms remains")
data_vir <- filter(data_vir, !item_raw=="Mollusk_shells")


data_vir_grp <- data_vir %>% left_join(grp_full2[,c("item_raw","grp6","grp1","grp3")], all.x=TRUE)
data_vir_grp <- unique(data_vir_grp)

#Remove species for which nb_guts/n_sample > 5
data_vir_grp$nb_empty <- as.numeric(data_vir_grp$nb_empty)
data_vir_grp <- data_vir_grp %>% mutate(nb_guts = nb_sample-nb_empty)
data_vir_grp %>% filter(is.na(nb_guts))
data_vir_grp %>% filter(nb_guts<5) %>% group_by(fish_sp) %>% summarise(n_distinct(fish_sp)) #33 espèces...
data_vir_grp %>% filter(nb_sample<5) %>% group_by(fish_sp) %>% summarise(n_distinct(fish_sp)) #Only 18 espèces si n_sample
data_vir_grp %>% filter(nb_guts<3, !is.na(nb_guts)) %>% group_by(fish_sp) %>% summarise(n_distinct(fish_sp)) ##19 espèces
data_vir_grp <- data_vir_grp %>% filter(!nb_guts < 3)

#Check Algae and Detritus --> Possible to divide contribution into two equal parts 
data_vir_grp %>% filter(grepl("Algae&", item_raw)) %>% select(item_raw, grp6, item_volper)

##Let's try to duplicate each rows and change item names for the duplicate as detritus
vir.algae_detritus <- data_vir_grp %>% filter(grepl("Algae&", item_raw)) #select rows for which item_raw contains ""

dup <- vir.algae_detritus %>% slice(rep(1:n(), each=2)) #duplicate each row

dup$grp6[c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34)]<- "Benthic autotroph" #rename deplicate by "Detritus"

dup$item_volper <- dup$item_volper/2

#Remove the modified rows and add the new rows
data_vir_grp %>% filter(grepl("Algae&", item_raw) & grp6 =="Detritus") #What i want to remove
remove <- which(data_vir_grp$grp6 == "Detritus" & data_vir_grp$item_raw == "Algae&orgdetritus" | data_vir_grp$item_raw == "Algae&detritus")
data_vir_grp <- data_vir_grp[-remove,]

data_vir_grp <- rbind(data_vir_grp, dup)

#Some items for which no grp6
data_vir_grp %>% filter(is.na(grp6)) %>% select(item_raw, grp6) %>% dplyr::count(item_raw)

data_vir_grp <- data_vir_grp %>% dplyr::mutate(grp6 = replace(grp6, item_raw == "Amphiopods", "Peracarida"),
                                        grp6 = replace(grp6, item_raw == "Exocorallina_antillensis", "Peracarida"),
                                        grp6 = replace(grp6, item_raw == "Panulirus_argus", "Decapoda"),
                                        grp6 = replace(grp6, item_raw == "Plants", "Benthic autotroph"),
                                        grp6 = replace(grp6, item_raw == "Scyllarid_lobster", "Decapoda"),
                                        grp6 = replace(grp6, item_raw == "Scyllaridae_larvae", "Zooplankton"),
                                        grp6 = replace(grp6, item_raw == "Scyllarides_aequinoctialis", "Decapoda"),
                                        grp6 = replace(grp6, item_raw == "Spermatophyta", "Benthic autotroph"),
                                        grp6 = replace(grp6, item_raw == "Spiny_lobster", "Decapoda"),
                                        grp6 = replace(grp6, item_raw == "Zoantharia", "Anthozoa"))
                                        
data_vir_grp %>% filter(is.na(grp6)) %>% select(item_raw, grp6) %>% dplyr::count(item_raw)                        
                                        
#Need to "sum up" the % volume according to the categorie of grp6 : if 7% of shrimp and 3% of crab = 10% Decapoda
data_vir_grp$item_volper <- as.numeric(data_vir_grp$item_volper)
data_vir_grp_sum <- data_vir_grp  %>%
  plyr::ddply(.(family_cor,fish_sp, grp6), summarise, sum = sum(item_volper))

  #group_by(family_cor, grp6) %>%
  #dplyr::mutate(mean_fam = mean(sum))

nb_spbyfam <- data_vir_grp_sum %>% group_by(family_cor) %>%
  dplyr::summarise(n = n_distinct(fish_sp))

data_vir_grp_sum <- data_vir_grp_sum %>% dplyr::left_join(nb_spbyfam[,c("family_cor", "n")], by="family_cor")

data_vir_grp_sum <-data_vir_grp_sum %>% group_by(family_cor, grp6) %>%
  dplyr::mutate(mean_fam = sum(sum)/n)

data_vir_sum_fam <- data_vir_grp_sum %>% select(family_cor, grp6, mean_fam)
data_vir_sum_fam <- unique(data_vir_sum_fam)

#Now need to have a matrix with the % per families
vir_ISmatrix <- reshape2::acast(data_vir_sum_fam, grp6 ~ family_cor, value.var = "mean_fam") 
vir_ISmatrix[is.na(vir_ISmatrix)] <- 0  

vir_ISmatrix_std <- apply(vir_ISmatrix, 2, function(x) x/sum(x)) #standardized matrix, colSums = 1
colSums(vir_ISmatrix_std)


#Now with only the families in common for better comparison of the networks : but useless....
vir_ISmatrix2_fam <- vir_ISmatrix2
vir_ISmatrix2_fam[,colnames(vir_ISmatrix2_fam) %in% fam_dif] <- 0 #work

colSums(vir_ISmatrix2_fam) 

plotweb(vir_ISmatrix2_fam,method="normal", empty = FALSE, arrow="no",text.rot=90, col.interaction = "grey80",
        col.low = box.colors$color, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.10, ybig = 1.3, bor.col.low = box.colors$color, bor.col.high = col.fish$color)


  #####Marshall Islands - Item freq####
data_mari <- data_raw %>% select(site_code, class, family_cor, fish_sp, nb_sample, nb_empty, item_raw, item_cor, item_phylum, item_class, item_ord_cor, item_fam_cor,item_gen_cor, item_sp_cor, item_freq) %>%
  filter(site_code == "mari", !is.na(item_freq))

#Get the data according to grp6
data_mari_grp <- data_mari %>% left_join(grp_full2[,c("item_raw","grp6","grp1","grp3")], all.x=TRUE)
data_mari_grp <- unique(data_mari_grp)

#Remove species for which less than 5 indiv
data_mari_grp$nb_empty <- as.numeric(data_mari_grp$nb_empty)
data_mari_grp <- data_mari_grp %>% mutate(nb_guts = nb_sample-nb_empty)
data_mari_grp %>% filter(is.na(nb_guts))

data_mari_grp %>% filter(nb_guts<5) %>% group_by(fish_sp) %>% summarise(n_distinct(fish_sp)) # 95 espèces...
data_mari_grp %>% filter(nb_sample < 5) %>% group_by(fish_sp) %>% summarise(n_distinct(fish_sp)) # 77 espèces si n_sample
data_mari_grp %>% filter(nb_sample < 3) %>% group_by(fish_sp) %>% summarise(n_distinct(fish_sp)) #53
data_mari_grp %>% filter(nb_guts < 3) %>% group_by(fish_sp) %>% summarise(n_distinct(fish_sp)) ##66 espèces
data_mari_grp <- data_mari_grp %>% filter(!nb_guts < 3)

#Check item for which no grp6 --> rename 
data_mari_grp %>% filter(is.na(grp6)) %>% dplyr::count(item_raw)
data_mari_grp <- data_mari_grp %>% dplyr::mutate(grp6 = replace(grp6, item_raw == "Larvae_unid", "Zooplankton"),
                                               grp6 = replace(grp6, item_raw == "Polychaeta pelagic", "Worm"),
                                               grp6 = replace(grp6, item_raw == "Scleractinia_polyps_skeletalpowder", "Anthozoa"),
                                               grp6 = replace(grp6, item_raw == "Solenogastrids_Chitons", "Polyplacophora"))
                             
#Remove unrelevant rows
data_mari_grp <- data_mari_grp %>% filter(item_raw != "Crustacea" , item_raw != "Crustacea_frags" , item_raw != "Garbage" , item_raw != "Gurry")


#Get the mean item_freq by group and family
data_mari_grp_sum <- data_mari_grp  %>%
  plyr::ddply(.(family_cor,fish_sp, grp6), summarise, mean = mean(item_freq))

nb_spbyfam <- data_mari_grp_sum %>% group_by(family_cor) %>%
  dplyr::summarise(n = n_distinct(fish_sp))

data_mari_grp_sum <- data_mari_grp_sum %>% dplyr::left_join(nb_spbyfam[,c("family_cor", "n")], by="family_cor")

data_mari_grp_sum <-data_mari_grp_sum %>% group_by(family_cor, grp6) %>%
  dplyr::mutate(mean_fam = sum(mean)/n) #item freq by family is the sum of item_freq divided by the nb of species 

data_mari_sum_fam <- data_mari_grp_sum %>% select(family_cor, grp6, mean_fam)
data_mari_sum_fam <- unique(data_mari_sum_fam)

#Now need to have a matrix with the % per families
mari_ISmatrix <- reshape2::acast(data_mari_sum_fam, grp6 ~ family_cor, value.var = "mean_fam") 
mari_ISmatrix[is.na(mari_ISmatrix)] <- 0  

mari_ISmatrix_std <- apply(mari_ISmatrix, 2, function(x) x/sum(x)) #standardized matrix, colSums = 1
colSums(mari_ISmatrix_std)

  #####Madagascar - Item freq####
data_mad <- data_raw %>% select(site_code, class, family_cor, fish_sp, nb_sample, nb_empty, item_raw, item_cor, item_phylum, item_class, item_ord_cor, item_fam_cor,item_gen_cor, item_sp_cor, item_freq, item_massper, time) %>%
  filter(site_code == "mad", !is.na(item_freq)) # , time=="day") or we could take into account night samples too ?!

length(unique(data_mad$fish_sp[which(is.na(data_mad$item_massper))])) #72 sp for which item_massper NA so work on item freq

data_full2 %>% filter(grp6=="Cephalochordata", site_code=="mad")
data_raw %>% filter(item_raw =="Cephalochordates", site_code=="mad") %>% select(item_freq, item_raw, fish_sp, time)

#Get the data according to grp6
data_mad_grp <- data_mad %>% left_join(grp_full2[,c("item_raw","grp6","grp1","grp3")], all.x=TRUE)
data_mad_grp <- unique(data_mad_grp)

#Remove species for which less than 5 indiv
data_mad_grp$nb_empty <- as.numeric(data_mad_grp$nb_empty)
data_mad_grp <- data_mad_grp %>% mutate(nb_guts = nb_sample-nb_empty)

data_mad_grp %>% filter(is.na(nb_guts))

data_mad_grp %>% filter(nb_guts<5) %>% group_by(fish_sp) %>% summarise(n_distinct(fish_sp)) #29 espèces...
data_mad_grp %>% filter(nb_guts<3) %>% summarise(n_distinct(fish_sp)) #15
data_mad_grp %>% filter(nb_sample<5) %>% group_by(fish_sp) %>% summarise(n_distinct(fish_sp)) #Only 13 espèces si n_sample
data_mad_grp %>% filter(nb_sample<3) %>% group_by(fish_sp) %>% summarise(n_distinct(fish_sp)) #6

data_mad_grp <- data_mad_grp %>% filter(!nb_guts < 3)

data_mad_grp %>% filter(fish_sp == "Lethrinus obsoletus") %>% select(nb_sample, nb_guts) #For madagascar, actually more sp kept coz night and day sampled

#Check item for which no grp6 --> rename 
data_mad_grp %>% filter(is.na(grp6)) %>% dplyr::count(item_raw)
data_mad_grp <- data_mad_grp %>% dplyr::mutate(grp6 = replace(grp6, item_raw == "Scyllarus", "Decapoda"))
                                                 
#Remove unrelevant rows
data_mad_grp <- data_mad_grp %>% filter(!is.na(grp6))

#Get the mean item_freq by group and family
data_mad_grp_sum <- data_mad_grp  %>%
  plyr::ddply(.(family_cor,fish_sp, grp6), summarise, mean = mean(item_freq))

nb_spbyfam <- data_mad_grp_sum %>% group_by(family_cor) %>%
  dplyr::summarise(n = n_distinct(fish_sp))

data_mad_grp_sum <- data_mad_grp_sum %>% dplyr::left_join(nb_spbyfam[,c("family_cor", "n")], by="family_cor")

data_mad_grp_sum <-data_mad_grp_sum %>% group_by(family_cor, grp6) %>%
  dplyr::mutate(mean_fam = sum(mean)/n) #item freq by family is the sum of item_freq divided by the nb of species 

data_mad_sum_fam <- data_mad_grp_sum %>% select(family_cor, grp6, mean_fam)
data_mad_sum_fam <- unique(data_mad_sum_fam)

#Now need to have a matrix with the % per families
mad_ISmatrix <- reshape2::acast(data_mad_sum_fam, grp6 ~ family_cor, value.var = "mean_fam") 
mad_ISmatrix[is.na(mad_ISmatrix)] <- 0  

mad_ISmatrix_std <- apply(mad_ISmatrix, 2, function(x) x/sum(x)) #standardized matrix, colSums = 1
colSums(mad_ISmatrix_std)


  #####Hawai - Item freq####
data_haw <- data_raw %>% select(site_code, class, family_cor, fish_sp, nb_sample, nb_guts, item_raw, item_raw, item_phylum, item_class, item_ord_cor, item_fam_cor, item_gen_cor, item_sp_cor, item_freq, item_volper) %>%
  filter(!is.na(item_freq), site_code == "haw") 

length(unique(data_haw$fish_sp[which(is.na(data_haw$item_volper))])) #17 sp for which item_volper NA so choose to work on item_freq

#Get the data according to grp6
data_haw_grp <- data_haw %>% left_join(grp_full2[,c("item_raw","grp6","grp1","grp3")], all.x=TRUE)
data_haw_grp <- unique(data_haw_grp)

#Remove species for which less than 5 indiv
data_haw_grp %>% filter(is.na(nb_guts))

data_haw_grp %>% filter(nb_guts<5) %>% group_by(fish_sp) %>% summarise(n_distinct(fish_sp)) #16 espèces...
data_haw_grp %>% filter(nb_guts < 3) %>% group_by(fish_sp) %>% summarise(n_distinct(fish_sp)) #8 espèces
data_haw_grp %>% filter(nb_sample<5) %>% group_by(fish_sp) %>% summarise(n_distinct(fish_sp)) #Only 9 espèces si n_sample
data_haw_grp <- data_haw_grp %>% filter(!nb_guts < 3)

data_haw_grp %>% filter(is.na(nb_guts)) %>% group_by(fish_sp) %>% summarise(n_distinct(fish_sp)) #Okay kept the NA's

#Check item for which no grp6 --> rename 
data_haw_grp %>% filter(is.na(grp6)) %>% dplyr::count(item_raw)

data_haw_grp %>% filter(item_raw =="Polychaeta_errant_frags") %>% select(fish_sp)

data_haw_grp %>% filter(fish_sp =="Chaetodon fremblii")

data_haw_grp <- data_haw_grp %>% dplyr::mutate(grp6 = replace(grp6, item_raw == "Polychaeta_errant_frags", "Worm"),
                                               grp6 = replace(grp6, item_raw == "Polychaetes heads", "Worm"))
                                               

#Remove unrelevant rows
data_haw_grp <- data_haw_grp %>% filter(!is.na(grp6))

#Get the mean item_freq by group and family
data_haw_grp_sum <- data_haw_grp  %>%
  plyr::ddply(.(family_cor,fish_sp, grp6), summarise, mean = mean(item_freq))

nb_spbyfam <- data_haw_grp_sum %>% group_by(family_cor) %>%
  dplyr::summarise(n = n_distinct(fish_sp))

data_haw_grp_sum <- data_haw_grp_sum %>% dplyr::left_join(nb_spbyfam[,c("family_cor", "n")], by="family_cor")

data_haw_grp_sum <-data_haw_grp_sum %>% group_by(family_cor, grp6) %>%
  dplyr::mutate(mean_fam = sum(mean)/n) #item freq by family is the sum of item_freq divided by the nb of species 

data_haw_sum_fam <- data_haw_grp_sum %>% select(family_cor, grp6, mean_fam)
data_haw_sum_fam <- unique(data_haw_sum_fam)

#Now need to have a matrix with the % per families
haw_ISmatrix <- reshape2::acast(data_haw_sum_fam, grp6 ~ family_cor, value.var = "mean_fam") 
haw_ISmatrix[is.na(haw_ISmatrix)] <- 0  
colSums(haw_ISmatrix)

haw_ISmatrix_std <- apply(haw_ISmatrix, 2, function(x) x/sum(x)) #standardized matrix, colSums = 1
colSums(haw_ISmatrix_std)


  #####New Caledonia - Item freq####
library(readr)
data_clean_nc <- read_csv("data/data_clean_nc.csv")
names(data_clean_nc)
data_nca <- data_clean_nc %>% select(site_code, class, Family, nom, nb_sample, nb_guts, item_raw, item_raw, item_phylum, item_class, item_ord, item_family, freq_item) %>%
  filter(!is.na(freq_item)) %>%
  dplyr::rename(item_freq = freq_item, family_cor = Family, fish_sp = nom, item_fam_cor = item_family, item_ord_cor = item_ord)


#Get the data according to grp6
data_nca_grp <- data_nca %>% left_join(grp_full2[,c("item_raw","grp6","grp1","grp3")], all.x=TRUE)
data_nca_grp <- unique(data_nca_grp)

#Remove species for which less than 5 indiv
data_nca_grp %>% filter(is.na(nb_guts))
data_nca_grp %>% filter(nb_guts<5) %>% group_by(fish_sp) %>% summarise(n_distinct(fish_sp)) # 99 espèces...
data_nca_grp %>% filter(nb_guts<3) %>% group_by(fish_sp) %>% summarise(n_distinct(fish_sp)) #64 espèces
data_nca_grp %>% filter(nb_sample<5) %>% group_by(fish_sp) %>% summarise(n_distinct(fish_sp)) #Only 19 espèces si n_sample
data_nca_grp <- data_nca_grp %>% filter(!nb_guts < 3)

#Check item for which no grp6 --> rename 
data_nca_grp %>% filter(is.na(grp6)) %>% dplyr::count(item_raw)
data_nca_grp <- data_nca_grp %>% dplyr::mutate(grp6 = replace(grp6, item_raw == "Palinuridae/Scyllaridae", "Decapoda"))


#Remove unrelevant rows
data_nca_grp <- data_nca_grp %>% filter(!is.na(grp6))

#Get the mean item_freq by group and family
data_nca_grp_sum <- data_nca_grp  %>%
  plyr::ddply(.(family_cor,fish_sp, grp6), summarise, mean = mean(item_freq))

nb_spbyfam <- data_nca_grp_sum %>% group_by(family_cor) %>%
  dplyr::summarise(n = n_distinct(fish_sp))

data_nca_grp_sum <- data_nca_grp_sum %>% dplyr::left_join(nb_spbyfam[,c("family_cor", "n")], by="family_cor")

data_nca_grp_sum <-data_nca_grp_sum %>% group_by(family_cor, grp6) %>%
  dplyr::mutate(mean_fam = sum(mean)/n) #item freq by family is the sum of item_freq divided by the nb of species 

data_nca_sum_fam <- data_nca_grp_sum %>% select(family_cor, grp6, mean_fam)
data_nca_sum_fam <- unique(data_nca_sum_fam)

#Now need to have a matrix with the % per families
nca_ISmatrix <- reshape2::acast(data_nca_sum_fam, grp6 ~ family_cor, value.var = "mean_fam") 
nca_ISmatrix[is.na(nca_ISmatrix)] <- 0  

nca_ISmatrix_std <- apply(nca_ISmatrix, 2, function(x) x/sum(x)) #standardized matrix, colSums = 1
colSums(nca_ISmatrix_std)



#####     #3. Clean full data after cleaning and removing species < 3 guts ####
data_ISfull_grp6 <- plyr::rbind.fill(data_vir_grp, data_mari_grp, data_mad_grp, data_haw_grp, data_nca_grp)
data_ISfull_grp6 %>% summarise(n = n_distinct(family_cor)) # 84 families going to be 83 after changing Scarids into Labrids

#Export a clean dataset

data_ISfull_grp6 <- data_ISfull_grp6 %>% mutate(family_cor = replace(family_cor, family_cor == "Scaridae", "Labridae")) 

write.csv(data_ISfull_grp6 %>% dplyr::rename(.,item_fam = item_fam_cor, item = item_cor, item_ord = item_ord_cor,
                                      item_gen = item_gen_cor, item_sp = item_sp_cor), "data/data_ISfull.csv")


##Check number of families and species per site

plyr::ddply(data_ISfull_grp6, ~site_code, summarize, 
            n_fish_sp = length(unique(fish_sp)),
            n_fish_fam = length(unique(family_cor)))

haw_tax <- data_haw %>% select(8:13) %>%
  summarise_all(funs(sum(!is.na(.))))

haw_per <- round(haw_tax*100/nrow(data_haw), 2)

#mad

  summarise_all(funs(sum(!is.na(.))))

mad_per <- round(mad_tax*100/nrow(data_mad), 2)

#vir
vir_tax <- data_vir %>% select(9:14) %>%
  summarise_all(funs(sum(!is.na(.))))

vir_per <- round(vir_tax*100/nrow(data_vir), 2)

#mari

mari_tax <- data_mari %>% select(9:14) %>%
  summarise_all(funs(sum(!is.na(.))))

mari_per <- round(mari_tax*100/nrow(data_mari), 2)

#nca
nca_tax <- data_nca %>% select(8:11) %>%
  summarise_all(funs(sum(!is.na(.)))) 

nca_per <- round(nca_tax*100/nrow(data_nca), 2)

tax_ISfull <- bind_rows(mad_per, haw_per, vir_per, mari_per, nca_per) %>%
  rownames_to_column(var="site_code") %>%
  mutate(site_code = recode(site_code, "1" = "mad" , "2" = "haw","3" = "vir" , "4"="mari", "5"="nca"))

#Much less coz here only items for which quantitative data

#####     #4. Matrix with full set of nodes accross the 5 locations + plots  ####
#Create nodes 
n <- unique(data_ISfull_grp6$grp6)
n <- n[order(n)]
m <- unique(data_ISfull_grp6$family) 
m <- m[order(m)] #alphabetic order 

#### Virgin Islands####

vir_ISmatrix2 <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))

#Find which are in common
cols <- colnames(vir_ISmatrix2)[colnames(vir_ISmatrix2) %in% colnames(vir_ISmatrix_std)]
rows <- rownames(vir_ISmatrix2)[rownames(vir_ISmatrix2) %in% rownames(vir_ISmatrix_std)]

vir_ISmatrix2[rows, cols] <- vir_ISmatrix_std[rows, cols] #Matrix with all item grps and fish families
colSums(vir_ISmatrix2)
colnames(vir_ISmatrix2)
rownames(vir_ISmatrix2)

#Colors
col.fish <- data.frame(ifelse(colSums(vir_ISmatrix2)!= 0,"black","grey60")) #families with values = 0, grey color
colnames(col.fish) <- "color"
col.fish$color <- as.character(col.fish$color)

col.grp6 <- unique(data.frame(data_ISfull_grp6$grp6)) 
col.grp6 <- data.frame(col.grp6[!is.na(col.grp6[1])])
names(col.grp6)[1] <- "taxon"

colfunc4 <- colorRampPalette(c("darkorchid4","slateblue4","turquoise1","lightseagreen","gold","coral1","lightpink2"))
col.grp6$colors <- colfunc4(38)
col.grp6$colors <- as.character(col.grp6$colors)

box.colors <- data.frame(ifelse(rowSums(vir_ISmatrix2)!= 0,col.grp6$colors,"grey60")) #if sum of rows = 0 (item_class), grey color
colnames(box.colors) <- "color"
box.colors$color <- as.character(box.colors$color)

#plot
plotweb(vir_ISmatrix2,method="normal", empty = FALSE, arrow="no",text.rot=90, col.interaction = "grey80",
        col.low = box.colors$color, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.10, ybig = 1.3, bor.col.low = box.colors$color, bor.col.high = col.fish$color)


#### Marshall Islands####
mari_ISmatrix2 <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))

#Find which are in common
cols <- colnames(mari_ISmatrix2)[colnames(mari_ISmatrix2) %in% colnames(mari_ISmatrix_std)]
rows <- rownames(mari_ISmatrix2)[rownames(mari_ISmatrix2) %in% rownames(mari_ISmatrix_std)]

mari_ISmatrix2[rows, cols] <- mari_ISmatrix_std[rows, cols]
colSums(mari_ISmatrix2)
rownames(mari_ISmatrix2)

#Colors
col.fish <- data.frame(ifelse(colSums(mari_ISmatrix2)!= 0,"black","grey60")) #families with values = 0, grey color
colnames(col.fish) <- "color"
col.fish$color <- as.character(col.fish$color)

col.grp6 <- unique(data.frame(data_ISfull_grp6$grp6)) 
col.grp6 <- data.frame(col.grp6[!is.na(col.grp6[1])])
names(col.grp6)[1] <- "taxon"

colfunc4 <- colorRampPalette(c("darkorchid4","slateblue4","turquoise1","lightseagreen","gold","coral1","lightpink2"))

col.grp6$colors <- colfunc4(38)
col.grp6$colors <- as.character(col.grp6$colors)

box.colors <- data.frame(ifelse(rowSums(mari_ISmatrix2)!= 0,col.grp6$colors,"grey60")) #if sum of rows = 0 (item_class), grey color
colnames(box.colors) <- "color"
box.colors$color <- as.character(box.colors$color)

#plot
plotweb(mari_ISmatrix2,method="normal", empty = FALSE, arrow="no",text.rot=90, col.interaction = "grey80",
        col.low = box.colors$color, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.10, ybig = 1.3, bor.col.low = box.colors$color, bor.col.high = col.fish$color)


#### Madagascar #####
mad_ISmatrix2 <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))

#Find which are in common
cols <- colnames(mad_ISmatrix2)[colnames(mad_ISmatrix2) %in% colnames(mad_ISmatrix_std)]
rows <- rownames(mad_ISmatrix2)[rownames(mad_ISmatrix2) %in% rownames(mad_ISmatrix_std)]

mad_ISmatrix2[rows, cols] <- mad_ISmatrix_std[rows, cols]
colSums(mad_ISmatrix2)
rownames(mad_ISmatrix2)

#Colors
col.fish <- data.frame(ifelse(colSums(mad_ISmatrix2)!= 0,"black","grey60")) #families with values = 0, grey color
colnames(col.fish) <- "color"
col.fish$color <- as.character(col.fish$color)

col.grp6 <- unique(data.frame(data_ISfull_grp6$grp6)) 
col.grp6 <- data.frame(col.grp6[!is.na(col.grp6[1])])
names(col.grp6)[1] <- "taxon"

colfunc4 <- colorRampPalette(c("darkorchid4","slateblue4","turquoise1","lightseagreen","gold","coral1","lightpink2"))

col.grp6$colors <- colfunc4(38)
col.grp6$colors <- as.character(col.grp6$colors)

box.colors <- data.frame(ifelse(rowSums(mad_ISmatrix2)!= 0,col.grp6$colors,"grey60")) #if sum of rows = 0 (item_class), grey color
colnames(box.colors) <- "color"
box.colors$color <- as.character(box.colors$color)

#plot
plotweb(mad_ISmatrix2,method="normal", empty = FALSE, arrow="no",text.rot=90, col.interaction = "grey80",
        col.low = box.colors$color, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.10, ybig = 1.3, bor.col.low = box.colors$color, bor.col.high = col.fish$color)



#### Hawai ####
haw_ISmatrix2 <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))

#Find which are in common
cols <- colnames(haw_ISmatrix2)[colnames(haw_ISmatrix2) %in% colnames(haw_ISmatrix_std)]
rows <- rownames(haw_ISmatrix2)[rownames(haw_ISmatrix2) %in% rownames(haw_ISmatrix_std)]

haw_ISmatrix2[rows, cols] <- haw_ISmatrix_std[rows, cols]
colSums(haw_ISmatrix2)
rownames(haw_ISmatrix2)

#Colors
col.fish <- data.frame(ifelse(colSums(haw_ISmatrix2)!= 0,"black","grey60")) #families with values = 0, grey color
colnames(col.fish) <- "color"
col.fish$color <- as.character(col.fish$color)

col.grp6 <- unique(data.frame(data_ISfull_grp6$grp6)) 
col.grp6 <- data.frame(col.grp6[!is.na(col.grp6[1])])
names(col.grp6)[1] <- "taxon"

colfunc4 <- colorRampPalette(c("darkorchid4","slateblue4","turquoise1","lightseagreen","gold","coral1","lightpink2"))

col.grp6$colors <- colfunc4(38)
col.grp6$colors <- as.character(col.grp6$colors)

box.colors <- data.frame(ifelse(rowSums(haw_ISmatrix2)!= 0,col.grp6$colors,"grey60")) #if sum of rows = 0 (item_class), grey color
colnames(box.colors) <- "color"
box.colors$color <- as.character(box.colors$color)

#plot
plotweb(haw_ISmatrix2,method="normal", empty = FALSE, arrow="no",text.rot=90, col.interaction = "grey80",
        col.low = box.colors$color, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.10, ybig = 1.3, bor.col.low = box.colors$color, bor.col.high = col.fish$color)



#### New Caledonia ####
nca_ISmatrix2 <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))

#Find which are in common
cols <- colnames(nca_ISmatrix2)[colnames(nca_ISmatrix2) %in% colnames(nca_ISmatrix_std)]
rows <- rownames(nca_ISmatrix2)[rownames(nca_ISmatrix2) %in% rownames(nca_ISmatrix_std)]

nca_ISmatrix2[rows, cols] <- nca_ISmatrix_std[rows, cols]
colSums(nca_ISmatrix2)
rownames(nca_ISmatrix2)

#Colors
col.fish <- data.frame(ifelse(colSums(nca_ISmatrix2)!= 0,"black","grey60")) #families with values = 0, grey color
colnames(col.fish) <- "color"
col.fish$color <- as.character(col.fish$color)

col.grp6 <- unique(data.frame(data_ISfull_grp6$grp6)) 
col.grp6 <- data.frame(col.grp6[!is.na(col.grp6[1])])
names(col.grp6)[1] <- "taxon"

colfunc4 <- colorRampPalette(c("darkorchid4","slateblue4","turquoise1","lightseagreen","gold","coral1","lightpink2"))

col.grp6$colors <- colfunc4(38)
plot(rep(1,38),col=(colfunc4(38)),pch=15,cex=2) #plot to see the colours
col.grp6$colors <- as.character(col.grp6$colors)

box.colors <- data.frame(ifelse(rowSums(nca_ISmatrix2)!= 0,col.grp6$colors,"grey60")) #if sum of rows = 0 (item_class), grey color
colnames(box.colors) <- "color"
box.colors$color <- as.character(box.colors$color)

#plot
plotweb(nca_ISmatrix2,method="normal", empty = FALSE, arrow="no",text.rot=90, col.interaction = "grey80",
        col.low = box.colors$color, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.10, ybig = 1.3, bor.col.low = box.colors$color, bor.col.high = col.fish$color)

colnames(nca_ISmatrix2)


#####       #5. Export Final matrices ####

write.csv(vir_ISmatrix2, "data/vir_ISmatrix2.csv")
write.csv(mari_ISmatrix2, "data/mari_ISmatrix2.csv")
write.csv(nca_ISmatrix2, "data/nca_ISmatrix2.csv")
write.csv(mad_ISmatrix2, "data/mad_ISmatrix2.csv")  
write.csv(haw_ISmatrix2, "data/haw_ISmatrix2.csv")

####Make plot sampling efforts####

#Here from data_raw so aaaaaaaaaall items
nb_links_guts <- data_raw %>% group_by(fish_sp, site_code) %>% dplyr::summarise(n = n_distinct(item_raw))
nb_links_guts <- left_join(nb_links_guts, data_raw[,c("fish_sp","site_code","nb_sample")], by=c("fish_sp","site_code"))
nb_links_guts <- unique(nb_links_guts)

nb_links_guts <- rbind(nb_links_guts,data_nca %>% group_by(fish_sp, site_code) %>% dplyr::summarise(n = n_distinct(item_raw)) %>%
  left_join(.,data_nca[,c("fish_sp","site_code","nb_sample")], by=c("fish_sp","site_code")) %>%
  unique(.)) %>%
  filter(nb_sample > 0)

ggplot(data=nb_links_guts, aes(x=nb_sample, y=n, color = site_code), na.rm=T) + 
  geom_point(shape = 16, alpha = 0.4) +
  xlim(0,200)  +
  geom_smooth(method="lm",   # Add linear regression line
              se=TRUE) +     # Don't add shaded confidence region
  labs(x= "Number of individual sampled" , y="Number of trophic interactions - all items", colour = "Site")

data_vir %>% filter(nb_sample > 250)

#Thing here we see clearly that different studies don't have the same effort in prey IDs
data_vir_grp %>% filter(fish_sp == "Abudefduf taurus") %>% select(item_raw, grp6)
data_vir_grp %>% filter(fish_sp == "Abudefduf saxatilis") %>% select(item_raw, grp6)

data_raw %>% filter(fish_sp == "Abudefduf saxatilis", site_code=='vir') %>% select(item_raw)

data_raw_grp %>% filter(fish_sp == "Abudefduf saxatilis", site_code=='vir') %>% select(item_raw, grp6)

data_vir_grp %>% group_by(fish_sp, site_code) %>% dplyr::summarise(n = n_distinct(item_raw))

#Let's try with quantified items- keep only items for freq or volume
data_raw_grp <- left_join(data_raw, grp_full2[,c("item_raw","grp6")], all.x=TRUE) %>% 
  filter(time =="day"| !is.na(item_volper) | !is.na(item_freq))
data_raw_grp <- unique(data_raw_grp)

data_raw %>% filter(fish_sp == "Zebrasoma flavescens", site_code =="mad") %>% select(nb_sample, time)
data_raw_grp %>% filter(fish_sp == "Zebrasoma flavescens", site_code =="mad") %>% select(nb_sample, time)

nb_links_guts2 <- data_raw_grp %>% group_by(fish_sp, site_code) %>% dplyr::summarise(n = n_distinct(item_raw))

nb_links_guts2 <- left_join(nb_links_guts2, data_raw_grp[,c("fish_sp","site_code","nb_sample")], by=c("fish_sp","site_code"), all.x=TRUE)
nb_links_guts2 <- unique(nb_links_guts2)

nb_links_guts2 <- rbind(nb_links_guts2, data_nca_grp %>% group_by(fish_sp, site_code) %>% dplyr::summarise(n = n_distinct(grp6)) %>%
                         left_join(.,data_nca_grp[,c("fish_sp","site_code","nb_sample")], by=c("fish_sp","site_code")) %>%
                         unique(.)) %>% 
  filter(nb_sample > 0)

ggplot(data=nb_links_guts2, aes(x=nb_sample, y=n, color = site_code), na.rm=T) + 
  geom_point(shape = 16, alpha = 0.4) +
  xlim(0,200)  +
  geom_smooth(method="lm",   # Add linear regression line
              se=TRUE) +     # Don't add shaded confidence region
  labs(x= "Number of individual sampled" , y="Number of trophic interactions", colour = "Site")


#Let's try with Grp6 classification
nb_links_guts_g6 <- data_raw_grp %>% group_by(fish_sp, site_code) %>% dplyr::summarise(n = n_distinct(grp6))

nb_links_guts_g6 <- left_join(nb_links_guts_g6, data_raw_grp[,c("fish_sp","site_code","nb_sample")], by=c("fish_sp","site_code"), all.x=TRUE)
nb_links_guts_g6 <- unique(nb_links_guts_g6)

nb_links_guts_g6 <- rbind(nb_links_guts_g6, data_nca_grp %>% group_by(fish_sp, site_code) %>% dplyr::summarise(n = n_distinct(grp6)) %>%
                          left_join(.,data_nca_grp[,c("fish_sp","site_code","nb_sample")], by=c("fish_sp","site_code")) %>%
                          unique(.)) %>% 
  filter(nb_sample > 0)

ggplot(data=nb_links_guts_g6, aes(x=nb_sample, y=n, color = site_code), na.rm=T) + 
  geom_point(shape = 16, alpha = 0.4) +
  xlim(0,300)  +
  geom_smooth(method="lm",   # Add linear regression line
              se=TRUE) +     # Don't add shaded confidence region
  labs(x= "Number of individual sampled" , y="Number of trophic interactions per species - Grp6", colour = "Site") +
  theme_classic()


####GLM####
hist(nb_links_guts_g6$nb_sample, xlim = c(0,200))
mod1 <- glm(formula = n~nb_sample, data = nb_links_guts_g6, family="poisson")
mod2 <- glm(formula = n~nb_sample, data = nb_links_guts_g6)
plot(mod1, main ="Residus mod")
summary(mod1)
summary(mod2)

(mod1$null.deviance-mod1$deviance)/mod1$null.deviance #heu c 'est super faible....

(mod2$null.deviance-mod2$deviance)/mod2$null.deviance

#eq mod
y1=(1.577+5.345^-4*nb_links_guts_g6$nb_sample) # mod2: notez la transformation exponentielle pour passer sur l'?chelle naturelle de NSPEC ? partir des coefficients du GLM
y2=(4.779+0.004*nb_links_guts_g6$nb_sample)
z=cbind(nb_links_guts_g6$nb_sample,y1,y2) # on cr?e par commodit? un objet qui stocke la variable d'int?r?t (DSOUR) et les pr?dictions
z=z[order(z[,1]),] # on ordonne cet objet en fonction des valeurs de DSOUR, ?a nous facilitera la t?che pour les graphiques


par(mfrow=c(1,2))
plot(nb_links_guts_g6$nb_sample,nb_links_guts_g6$n,main="mod?le lin?aire") # nuage de points des donn?es brutes
lines(z[,1],z[,2],col="red",lwd=2) # on ajoute sous forme de ligne la pr?diction du mod?le lin?aire
plot(nb_links_guts_g6$nb_sample,nb_links_guts_g6$n, main="GLM de Poisson") # on r?p?te la manip pour le GLM
lines(z[,1],z[,3],col="red",lty="dashed",lwd=2)
legend("topleft",legend=c("LM","GLM"),col="red",lty=c("solid","dashed"),lwd=2) 


