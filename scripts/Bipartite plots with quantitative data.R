##########################################################
#Try to do the bipartite plots with quantitative measures#
##########################################################

library(tidyverse)
library(readxl)
library(plyr)
library(bipartite)

##Work on dietdata2.0 coz data clean does'nt have the freq/% data
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

names(data_raw)


  ####Randall - Virgin Islands - Volumetric %####

#Getting data for VIrgin Islnds and rows for which item_freq not NA
data_vir <- data_raw %>% select(site_code, class, family_cor, fish_sp, nb_sample, nb_empty, item_raw, item_cor, item_phylum, item_class, item_ord_cor, item_fam_cor,item_gen_cor, item_volper) %>%
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


#Check Algae and Detritus 
data_vir_grp %>% filter(grepl("Algae&", item_raw)) %>% select(item_raw, grp6, item_volper)

data_vir_grp %>% filter( item_raw =="Exocorallina_antillensis") %>% select(item_ord_cor)

data_vir_grp <- data_vir_grp %>% mutate(grp6 = replace(grp6, item_raw=="Algae&orgdetritus", "Benthic autotroph"))
data_vir_grp <- data_vir_grp %>% mutate(grp6 = replace(grp6, item_raw=="Algae&detritus", "Benthic autotroph"))
 
data_vir_grp %>% filter(grp6 == "Detritus")

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
                                        
#Need to "sum up" the % volume according to the categorie of grp6
data_vir_grp$item_volper <- as.numeric(data_vir_grp$item_volper)
data_vir_grp_sum <- data_vir_grp  %>%
  plyr::ddply(.(family_cor,fish_sp, grp6), summarise, sum = sum(item_volper)) %>%
  group_by(family_cor, grp6) %>%
  dplyr::mutate(mean_fam = mean(sum))

data_vir_sum_fam <- data_vir_grp_sum %>% select(family_cor, grp6, mean_fam)
data_vir_sum_fam <- unique(data_vir_sum_fam)


#Now need to have a matrix with the % per families
vir_ISmatrix <- reshape2::acast(data_vir_sum_fam, grp6 ~ family_cor, value.var = "mean_fam") 
vir_ISmatrix[is.na(vir_ISmatrix)] <- 0  

vir_ISmatrix_std <- apply(vir_ISmatrix, 2, function(x) x/sum(x)) #standardized matrix, colSums = 1
colSums(vir_ISmatrix_std)
rownames(vir_ISmatrix_std)

#Let's plot it
plotweb(vir_ISmatrix_std)

#But i would need all families and grp6 items for comparison .... 
n <- unique(data_full2$grp6)
n <- n[order(n)]
m <- unique(data_full2$family) 
m<- m[order(m)] #alphabetic order 

vir_ISmatrix2 <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))

#Find which are in common
cols <- colnames(vir_ISmatrix2)[colnames(vir_ISmatrix2) %in% colnames(vir_ISmatrix_std)]
rows <- rownames(vir_ISmatrix2)[rownames(vir_ISmatrix2) %in% rownames(vir_ISmatrix_std)]

vir_ISmatrix2[rows, cols] <- vir_ISmatrix_std[rows, cols]
colSums(vir_ISmatrix2)
rownames(vir_ISmatrix2)

vir_ISmatrix2 <- vir_ISmatrix2[-39,]

#Colors
col.fish <- data.frame(ifelse(colSums(vir_ISmatrix2)!= 0,"black","grey60")) #families with values = 0, grey color
colnames(col.fish) <- "color"
col.fish$color <- as.character(col.fish$color)

col.grp6 <- unique(data.frame(data_full2$grp6)) 
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

#Carefull here Benthic autotroph regroups Algea, seagrass, Algae&detritus



  #####Marshall Islands - Item freq####
data_mari <- data_raw %>% select(site_code, class, family_cor, fish_sp, nb_sample, nb_empty, item_raw, item_cor, item_phylum, item_class, item_ord_cor, item_fam_cor,item_gen_cor, item_freq) %>%
  filter(site_code == "mari", !is.na(item_freq))

#Get the data according to grp6
data_mari_grp <- data_mari %>% left_join(grp_full2[,c("item_raw","grp6","grp1","grp3")], all.x=TRUE)
data_mari_grp <- unique(data_mari_grp)

#Check item for which no grp6 --> rename 
data_mari_grp %>% filter(is.na(grp6)) %>% dplyr::count(item_raw)
data_mari_grp <- data_mari_grp %>% dplyr::mutate(grp6 = replace(grp6, item_raw == "Larvae_unid", "Zooplankton"),
                                               grp6 = replace(grp6, item_raw == "Polychaeta pelagic", "Annelida"),
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

#Let's plot it
plotweb(mari_ISmatrix_std)

#But i would need all families and grp6 items for comparison .... 
n <- unique(data_full2$grp6)
n <- n[order(n)]
m <- unique(data_full2$family) 
m<- m[order(m)] #alphabetic order 

mari_ISmatrix2 <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))

#Find which are in common
cols <- colnames(mari_ISmatrix2)[colnames(mari_ISmatrix2) %in% colnames(mari_ISmatrix_std)]
rows <- rownames(mari_ISmatrix2)[rownames(mari_ISmatrix2) %in% rownames(mari_ISmatrix_std)]

mari_ISmatrix2[rows, cols] <- mari_ISmatrix_std[rows, cols]
colSums(mari_ISmatrix2)
rownames(mari_ISmatrix2)

mari_ISmatrix2 <- mari_ISmatrix2[-39,]

#Colors
col.fish <- data.frame(ifelse(colSums(mari_ISmatrix2)!= 0,"black","grey60")) #families with values = 0, grey color
colnames(col.fish) <- "color"
col.fish$color <- as.character(col.fish$color)

col.grp6 <- unique(data.frame(data_full2$grp6)) 
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



  #####Madagascar - Item freq####
data_mad <- data_raw %>% select(site_code, class, family_cor, fish_sp, nb_sample, nb_empty, item_raw, item_cor, item_phylum, item_class, item_ord_cor, item_fam_cor,item_gen_cor, item_freq, time) %>%
  filter(site_code == "mad", !is.na(item_freq), time=="day") #or we could take into account night samples too ?!

data_full2 %>% filter(grp6=="Cephalochordata", site_code=="mad")

data_raw %>% filter(item_raw =="Cephalochordates", site_code=="mad") %>% select(item_freq, item_raw, fish_sp, time)

#Get the data according to grp6
data_mad_grp <- data_mad %>% left_join(grp_full2[,c("item_raw","grp6","grp1","grp3")], all.x=TRUE)
data_mad_grp <- unique(data_mad_grp)

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

#But i would need all families and grp6 items for comparison .... 
n <- unique(data_full2$grp6)
n <- n[order(n)]
m <- unique(data_full2$family) 
m<- m[order(m)] #alphabetic order 

mad_ISmatrix2 <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))

#Find which are in common
cols <- colnames(mad_ISmatrix2)[colnames(mad_ISmatrix2) %in% colnames(mad_ISmatrix_std)]
rows <- rownames(mad_ISmatrix2)[rownames(mad_ISmatrix2) %in% rownames(mad_ISmatrix_std)]

mad_ISmatrix2[rows, cols] <- mad_ISmatrix_std[rows, cols]
colSums(mad_ISmatrix2)
rownames(mad_ISmatrix2)

mad_ISmatrix2 <- mad_ISmatrix2[-39,]

#Colors
col.fish <- data.frame(ifelse(colSums(mad_ISmatrix2)!= 0,"black","grey60")) #families with values = 0, grey color
colnames(col.fish) <- "color"
col.fish$color <- as.character(col.fish$color)

col.grp6 <- unique(data.frame(data_full2$grp6)) 
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

##Carefull diff from the presence/absence matrix as some items with no frequence took into account, which is not the case with quantitative matrix (e.g. Cephalochordata)



  #####Hawai - Item freq####
data_haw <- data_raw %>% select(site_code, class, family_cor, fish_sp, nb_sample, nb_guts, item_raw, item_raw, item_phylum, item_class, item_ord_cor, item_fam_cor, item_freq) %>%
  filter(!is.na(item_freq),site_code == "haw") 
  

#Get the data according to grp6
data_haw_grp <- data_haw %>% left_join(grp_full2[,c("item_raw","grp6","grp1","grp3")], all.x=TRUE)
data_haw_grp <- unique(data_haw_grp)

#Check item for which no grp6 --> rename 
data_haw_grp %>% filter(is.na(grp6)) %>% dplyr::count(item_raw)

data_haw_grp %>% filter(item_raw =="Polychaeta_errant_frags") %>% select(fish_sp)

data_haw_grp %>% filter(fish_sp =="Chaetodon fremblii")

data_haw_grp <- data_haw_grp %>% dplyr::mutate(grp6 = replace(grp6, item_raw == "Polychaeta_errant_frags", "Annelida"),
                                               grp6 = replace(grp6, item_raw == "Polychaetes heads", "Annelida"))
                                               

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

haw_ISmatrix_std <- apply(haw_ISmatrix, 2, function(x) x/sum(x)) #standardized matrix, colSums = 1
colSums(haw_ISmatrix_std)

#But i would need all families and grp6 items for comparison .... 
n <- unique(data_full2$grp6)
n <- n[order(n)]
m <- unique(data_full2$family) 
m<- m[order(m)] #alphabetic order 

haw_ISmatrix2 <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))

#Find which are in common
cols <- colnames(haw_ISmatrix2)[colnames(haw_ISmatrix2) %in% colnames(haw_ISmatrix_std)]
rows <- rownames(haw_ISmatrix2)[rownames(haw_ISmatrix2) %in% rownames(haw_ISmatrix_std)]

haw_ISmatrix2[rows, cols] <- haw_ISmatrix_std[rows, cols]
colSums(haw_ISmatrix2)
rownames(haw_ISmatrix2)

haw_ISmatrix2 <- haw_ISmatrix2[-39,]

#Colors
col.fish <- data.frame(ifelse(colSums(haw_ISmatrix2)!= 0,"black","grey60")) #families with values = 0, grey color
colnames(col.fish) <- "color"
col.fish$color <- as.character(col.fish$color)

col.grp6 <- unique(data.frame(data_full2$grp6)) 
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

data_full2 %>% filter(site_code =="haw", family=="Belonidae")
data_raw %>% filter(site_code =="haw", family_cor=="Belonidae")
data_haw %>% filter(family_cor=="Belonidae") %>% select(nb_guts, item_raw, item_freq)


  #####New Caledonia - Item freq####
library(readr)
data_clean_nc <- read_csv("data/data_clean_nc.csv")
names(data_clean_nc)
data_nca <- data_clean_nc %>% select(site_code, class, Family, nom, nb_sample, nb_guts, item_raw, item_raw, item_phylum, item_class, item_ord, item_family, freq_item) %>%
  filter(!is.na(freq_item)) %>%
  dplyr::rename(item_freq = freq_item, family_cor = Family, fish_sp = nom)


#Get the data according to grp6
data_nca_grp <- data_nca %>% left_join(grp_full2[,c("item_raw","grp6","grp1","grp3")], all.x=TRUE)
data_nca_grp <- unique(data_nca_grp)

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

#But i would need all families and grp6 items for comparison .... 
n <- unique(data_full2$grp6)
n <- n[order(n)]
m <- unique(data_full2$family) 
m<- m[order(m)] #alphabetic order 

nca_ISmatrix2 <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))

#Find which are in common
cols <- colnames(nca_ISmatrix2)[colnames(nca_ISmatrix2) %in% colnames(nca_ISmatrix_std)]
rows <- rownames(nca_ISmatrix2)[rownames(nca_ISmatrix2) %in% rownames(nca_ISmatrix_std)]

nca_ISmatrix2[rows, cols] <- nca_ISmatrix_std[rows, cols]
colSums(nca_ISmatrix2)
rownames(nca_ISmatrix2)

nca_ISmatrix2 <- nca_ISmatrix2[-39,]

#Colors
col.fish <- data.frame(ifelse(colSums(nca_ISmatrix2)!= 0,"black","grey60")) #families with values = 0, grey color
colnames(col.fish) <- "color"
col.fish$color <- as.character(col.fish$color)

col.grp6 <- unique(data.frame(data_full2$grp6)) 
col.grp6 <- data.frame(col.grp6[!is.na(col.grp6[1])])
names(col.grp6)[1] <- "taxon"

colfunc4 <- colorRampPalette(c("darkorchid4","slateblue4","turquoise1","lightseagreen","gold","coral1","lightpink2"))

col.grp6$colors <- colfunc4(38)
col.grp6$colors <- as.character(col.grp6$colors)

box.colors <- data.frame(ifelse(rowSums(nca_ISmatrix2)!= 0,col.grp6$colors,"grey60")) #if sum of rows = 0 (item_class), grey color
colnames(box.colors) <- "color"
box.colors$color <- as.character(box.colors$color)

#plot
plotweb(nca_ISmatrix2,method="normal", empty = FALSE, arrow="no",text.rot=90, col.interaction = "grey80",
        col.low = box.colors$color, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.10, ybig = 1.3, bor.col.low = box.colors$color, bor.col.high = col.fish$color)

#Now with only the families in common for better comparison of the networks






  