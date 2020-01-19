#########################################
#Family level matrices and network plots#
#########################################

library(tidyverse)
library(plyr)

#Create a subset for each region. data_ISfull_grp_final --> data without single and doubletones
data_vir <- data_ISfull_grp_final2 %>% filter(site_code == "vir")
data_mari <- data_ISfull_grp_final2 %>% filter(site_code == "mari")
data_haw <- data_ISfull_grp_final2 %>% filter(site_code == "haw")
data_nca <- data_ISfull_grp_final2 %>% filter(site_code == "nca")
data_mad <- data_ISfull_grp_final2 %>% filter(site_code == "mad")
data_jap <- data_ISfull_grp_final2 %>% filter(site_code == "jap")

####1. Mean IS per families#### Using grp6 (most comparable prey category)

####Virgin Islands####
#Need to "sum up" the % volume according to the categorie of grp6 : if 7% of shrimp and 3% of crab = 10% Decapoda
data_vir_fam_sum <- data_vir  %>%
  plyr::ddply(.(family_cor, fish_sp, grp6), summarise, sum = sum(item_volper))

nb_spbyfam <- data_vir_fam_sum %>% group_by(family_cor) %>%
  dplyr::summarise(n = n_distinct(fish_sp))

data_vir_fam_sum <- data_vir_fam_sum %>% dplyr::left_join(nb_spbyfam[,c("family_cor", "n")], by="family_cor")

data_vir_fam_sum <-data_vir_fam_sum %>% group_by(family_cor, grp6) %>%
  dplyr::mutate(mean_fam = sum(sum)/n) #mean per family = volper / number of species/families

#Keep only family_cor rows
data_vir_fam_sum <- data_vir_fam_sum %>% select(family_cor, grp6, mean_fam) %>% unique(.)


####Marshall Islands####
#Get the mean item_freq by group and family
data_mari_fam_sum <- data_mari  %>%
  plyr::ddply(.(family_cor, fish_sp, grp6), summarise, mean = mean(item_freq))

nb_spbyfam <- data_mari_fam_sum %>% group_by(family_cor) %>%
  dplyr::summarise(n = n_distinct(fish_sp))

data_mari_fam_sum <- data_mari_fam_sum %>% dplyr::left_join(nb_spbyfam[,c("family_cor", "n")], by="family_cor")

data_mari_fam_sum <-data_mari_fam_sum %>% group_by(family_cor, grp6) %>%
  dplyr::mutate(mean_fam = sum(mean)/n) #item freq by family_cor is the sum of item_freq divided by the nb of species 

data_mari_fam_sum <- data_mari_fam_sum %>% select(family_cor, grp6, mean_fam) %>% unique(.)


####Madagascar####
#Get the mean item_freq by group and family
data_mad_fam_sum <- data_mad  %>%
  plyr::ddply(.(family_cor, fish_sp, grp6), summarise, mean = mean(item_freq)) #mean by group6 for each species

nb_spbyfam <- data_mad_fam_sum %>% group_by(family_cor) %>%
  dplyr::summarise(n = n_distinct(fish_sp))

data_mad_fam_sum <- data_mad_fam_sum %>% dplyr::left_join(nb_spbyfam[,c("family_cor", "n")], by="family_cor")

data_mad_fam_sum <-data_mad_fam_sum %>% group_by(family_cor, grp6) %>%
  dplyr::mutate(mean_fam = sum(mean)/n) #item freq by family_cor is the sum of item_freq divided by the nb of species 

data_mad_fam_sum <- data_mad_fam_sum %>% select(family_cor, grp6, mean_fam) %>% unique(.)

####Hawai####
#Get the mean item_freq by group and family
data_haw_fam_sum <- data_haw  %>%
  plyr::ddply(.(family_cor, fish_sp, grp6), summarise, mean = mean(item_freq)) #mean by group6 for each species

nb_spbyfam <- data_haw_fam_sum %>% group_by(family_cor) %>%
  dplyr::summarise(n = n_distinct(fish_sp))

data_haw_fam_sum <- data_haw_fam_sum %>% dplyr::left_join(nb_spbyfam[,c("family_cor", "n")], by="family_cor")

data_haw_fam_sum <-data_haw_fam_sum %>% group_by(family_cor, grp6) %>%
  dplyr::mutate(mean_fam = sum(mean)/n) #item freq by family_cor is the sum of item_freq divided by the nb of species 

data_haw_fam_sum <- data_haw_fam_sum %>% select(family_cor, grp6, mean_fam) %>% unique(.)

####New Caledonia####
#Get the mean item_freq by group and family
data_nca_fam_sum <- data_nca  %>%
  plyr::ddply(.(family_cor, fish_sp, grp6), summarise, mean = mean(item_freq)) #mean by group6 for each species

nb_spbyfam <- data_nca_fam_sum %>% group_by(family_cor) %>%
  dplyr::summarise(n = n_distinct(fish_sp))

data_nca_fam_sum <- data_nca_fam_sum %>% dplyr::left_join(nb_spbyfam[,c("family_cor", "n")], by="family_cor")

data_nca_fam_sum <-data_nca_fam_sum %>% group_by(family_cor, grp6) %>%
  dplyr::mutate(mean_fam = sum(mean)/n) #item freq by family_cor is the sum of item_freq divided by the nb of species 

data_nca_fam_sum <- data_nca_fam_sum %>% select(family_cor, grp6, mean_fam) %>% unique(.)

####Japan####
#Get the mean item_freq by group and family. !!!! For JAP, filter so remove all item_freq NA
data_jap_fam_sum <- data_jap  %>% filter(!is.na(item_freq)) %>% 
  plyr::ddply(.(family_cor, fish_sp, grp6), summarise, mean = mean(item_freq)) #mean by group6 for each species

nb_spbyfam <- data_jap_fam_sum %>% group_by(family_cor) %>%
  dplyr::summarise(n = n_distinct(fish_sp))

data_jap_fam_sum <- data_jap_fam_sum %>% dplyr::left_join(nb_spbyfam[,c("family_cor", "n")], by="family_cor")

data_jap_fam_sum <-data_jap_fam_sum %>% group_by(family_cor, grp6) %>%
  dplyr::mutate(mean_fam = sum(mean)/n) #item freq by family_cor is the sum of item_freq divided by the nb of species 

data_jap_fam_sum <- data_jap_fam_sum %>% select(family_cor, grp6, mean_fam) %>% unique(.)


####2. Matrices ####

#Create nodes 
n <- unique(data_ISfull_grp_final2$grp6)
n <- n[order(n)] %>%
  as.character(.)
m <- unique(data_ISfull_grp_final2$family_cor) #84 family_cor
m <- m[order(m)] #alphabetic order 

####Virgin Islands####
#Now need to have a matrix with the % per family_cor
vir_ISmatrix_fam <- reshape2::acast(data_vir_fam_sum, grp6 ~ family_cor, value.var = "mean_fam") 
vir_ISmatrix_fam[is.na(vir_ISmatrix_fam)] <- 0  

vir_ISmatrix_fam_std <- apply(vir_ISmatrix_fam, 2, function(x) x/sum(x)) #standardized matrix, colSums = 1
colSums(vir_ISmatrix_fam_std)

vir_ISmatrix_fam2 <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))

#Find which are in common
cols <- colnames(vir_ISmatrix_fam2)[colnames(vir_ISmatrix_fam2) %in% colnames(vir_ISmatrix_fam_std)]
rows <- rownames(vir_ISmatrix_fam2)[rownames(vir_ISmatrix_fam2) %in% rownames(vir_ISmatrix_fam_std)]

vir_ISmatrix_fam2[rows, cols] <- vir_ISmatrix_fam_std[rows, cols] #Matrix with all item grps and fish families

#Colors
#Set fish families boxes colors (upper boxes)
col.fish <- data.frame(ifelse(colSums(vir_ISmatrix_fam2)!= 0,"black","grey60")) #families with values = 0, grey color
colnames(col.fish) <- "color"
col.fish$color <- as.character(col.fish$color)

#Prey colors, same for all of the 5 networks
col.grp6 <- unique(data.frame(data_ISfull_grp_final$grp6)) 
col.grp6 <- data.frame(col.grp6[!is.na(col.grp6[1])])
names(col.grp6)[1] <- "taxon"

colfunc4 <- colorRampPalette(c("darkorchid4", "steelblue","turquoise1","seagreen3", "gold","orange","brown2","lightpink2")) #,"slateblue4", 
#colfunc4 <- colorRampPalette(c("darkorchid4","slateblue4","turquoise1","lightseagreen","gold","coral1","lightpink2"))
plot(rep(1,31),col=(colfunc4(31)),pch=15,cex=3) #plot to see the colours

col.grp6$colors <- colfunc4(31)
col.grp6$colors <- as.character(col.grp6$colors)

#Set prey boxes colors (lower boxes)
box.colors <- data.frame(ifelse(rowSums(vir_ISmatrix_fam2)!= 0, col.grp6$colors,"grey60")) #if sum of rows = 0 (item_class), grey color
colnames(box.colors) <- "color"
box.colors$color <- as.character(box.colors$color)

#plot
plotweb(vir_ISmatrix_fam2, method ="normal", empty = FALSE, arrow="no",text.rot=90, col.interaction = "grey80",
        col.low = box.colors$color, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.10, ybig = 1.3, bor.col.low = box.colors$color, bor.col.high = col.fish$color)

####Marshall Islands####
#Now need to have a matrix with the % per family_cor
mari_ISmatrix_fam <- reshape2::acast(data_mari_fam_sum, grp6 ~ family_cor, value.var = "mean_fam") 
mari_ISmatrix_fam[is.na(mari_ISmatrix_fam)] <- 0  

mari_ISmatrix_fam_std <- apply(mari_ISmatrix_fam, 2, function(x) x/sum(x)) #standardized matrix, colSums = 1
colSums(mari_ISmatrix_fam_std)

mari_ISmatrix_fam2 <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))

#Find which are in common
cols <- colnames(mari_ISmatrix_fam2)[colnames(mari_ISmatrix_fam2) %in% colnames(mari_ISmatrix_fam_std)]
rows <- rownames(mari_ISmatrix_fam2)[rownames(mari_ISmatrix_fam2) %in% rownames(mari_ISmatrix_fam_std)]

mari_ISmatrix_fam2[rows, cols] <- mari_ISmatrix_fam_std[rows, cols] #Matrix with all item grps and fish families

#Colors
col.fish <- data.frame(ifelse(colSums(mari_ISmatrix_fam2)!= 0,"black","grey60")) #families with values = 0, grey color
colnames(col.fish) <- "color"
col.fish$color <- as.character(col.fish$color)

box.colors <- data.frame(ifelse(rowSums(mari_ISmatrix_fam2)!= 0,col.grp6$colors,"grey60")) #if sum of rows = 0 (item_class), grey color
colnames(box.colors) <- "color"
box.colors$color <- as.character(box.colors$color)

#plot
plotweb(mari_ISmatrix_fam2, method="normal", empty = FALSE, arrow="no",text.rot=90, col.interaction = "grey80",
        col.low = box.colors$color, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.10, ybig = 1.3, bor.col.low = box.colors$color, bor.col.high = col.fish$color)

####Madagascar####
#Now need to have a matrix with the % per family_cor
mad_ISmatrix_fam <- reshape2::acast(data_mad_fam_sum, grp6 ~ family_cor, value.var = "mean_fam") 
mad_ISmatrix_fam[is.na(mad_ISmatrix_fam)] <- 0  

mad_ISmatrix_fam_std <- apply(mad_ISmatrix_fam, 2, function(x) x/sum(x)) #standardized matrix, colSums = 1
colSums(mad_ISmatrix_fam_std)

mad_ISmatrix_fam2 <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))

#Find which are in common
cols <- colnames(mad_ISmatrix_fam2)[colnames(mad_ISmatrix_fam2) %in% colnames(mad_ISmatrix_fam_std)]
rows <- rownames(mad_ISmatrix_fam2)[rownames(mad_ISmatrix_fam2) %in% rownames(mad_ISmatrix_fam_std)]

mad_ISmatrix_fam2[rows, cols] <- mad_ISmatrix_fam_std[rows, cols] #Matrix with all item grps and fish families

#Colors
col.fish <- data.frame(ifelse(colSums(mad_ISmatrix_fam2)!= 0,"black","grey60")) #families with values = 0, grey color
colnames(col.fish) <- "color"
col.fish$color <- as.character(col.fish$color)

box.colors <- data.frame(ifelse(rowSums(mad_ISmatrix_fam2)!= 0,col.grp6$colors,"grey60")) #if sum of rows = 0 (item_class), grey color
colnames(box.colors) <- "color"
box.colors$color <- as.character(box.colors$color)

#plot
plotweb(mad_ISmatrix_fam2,method="normal", empty = FALSE, arrow="no",text.rot=90, col.interaction = "grey80",
        col.low = box.colors$color, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.10, ybig = 1.3, bor.col.low = box.colors$color, bor.col.high = col.fish$color)

####Hawai####
#Now need to have a matrix with the % per family_cor
haw_ISmatrix_fam <- reshape2::acast(data_haw_fam_sum, grp6 ~ family_cor, value.var = "mean_fam") 
haw_ISmatrix_fam[is.na(haw_ISmatrix_fam)] <- 0  

haw_ISmatrix_fam_std <- apply(haw_ISmatrix_fam, 2, function(x) x/sum(x)) #standardized matrix, colSums = 1
colSums(haw_ISmatrix_fam_std)

haw_ISmatrix_fam2 <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))

#Find which are in common
cols <- colnames(haw_ISmatrix_fam2)[colnames(haw_ISmatrix_fam2) %in% colnames(haw_ISmatrix_fam_std)]
rows <- rownames(haw_ISmatrix_fam2)[rownames(haw_ISmatrix_fam2) %in% rownames(haw_ISmatrix_fam_std)]

haw_ISmatrix_fam2[rows, cols] <- haw_ISmatrix_fam_std[rows, cols] #Matrix with all item grps and fish families


#Colors
col.fish <- data.frame(ifelse(colSums(haw_ISmatrix_fam2)!= 0,"black","grey60")) #families with values = 0, grey color
colnames(col.fish) <- "color"
col.fish$color <- as.character(col.fish$color)

box.colors <- data.frame(ifelse(rowSums(haw_ISmatrix_fam2)!= 0,col.grp6$colors,"grey60")) #if sum of rows = 0 (item_class), grey color
colnames(box.colors) <- "color"
box.colors$color <- as.character(box.colors$color)

#plot
plotweb(haw_ISmatrix_fam2,method="normal", empty = FALSE, arrow="no",text.rot=90, col.interaction = "grey80",
        col.low = box.colors$color, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.10, ybig = 1.3, bor.col.low = box.colors$color, bor.col.high = col.fish$color)

####New Caledonia####
#Now need to have a matrix with the % per family_cor
nca_ISmatrix_fam <- reshape2::acast(data_nca_fam_sum, grp6 ~ family_cor, value.var = "mean_fam") 
nca_ISmatrix_fam[is.na(nca_ISmatrix_fam)] <- 0  

nca_ISmatrix_fam_std <- apply(nca_ISmatrix_fam, 2, function(x) x/sum(x)) #standardized matrix, colSums = 1
colSums(nca_ISmatrix_fam_std)

nca_ISmatrix_fam2 <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))

#Find which are in common
cols <- colnames(nca_ISmatrix_fam2)[colnames(nca_ISmatrix_fam2) %in% colnames(nca_ISmatrix_fam_std)]
rows <- rownames(nca_ISmatrix_fam2)[rownames(nca_ISmatrix_fam2) %in% rownames(nca_ISmatrix_fam_std)]

nca_ISmatrix_fam2[rows, cols] <- nca_ISmatrix_fam_std[rows, cols] #Matrix with all item grps and fish families


#Colors
col.fish <- data.frame(ifelse(colSums(nca_ISmatrix_fam2)!= 0,"black","grey60")) #families with values = 0, grey color
colnames(col.fish) <- "color"
col.fish$color <- as.character(col.fish$color)

box.colors <- data.frame(ifelse(rowSums(nca_ISmatrix_fam2)!= 0,col.grp6$colors,"grey60")) #if sum of rows = 0 (item_class), grey color
colnames(box.colors) <- "color"
box.colors$color <- as.character(box.colors$color)

#plot
plotweb(nca_ISmatrix_fam2,method="normal", empty = FALSE, arrow="no",text.rot=90, col.interaction = "grey80",
        col.low = box.colors$color, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.10, ybig = 1.3, bor.col.low = box.colors$color, bor.col.high = col.fish$color)


####Japan####
#Now need to have a matrix with the % per family_cor
jap_ISmatrix_fam <- reshape2::acast(data_jap_fam_sum, grp6 ~ family_cor, value.var = "mean_fam") 
jap_ISmatrix_fam[is.na(jap_ISmatrix_fam)] <- 0  

jap_ISmatrix_fam_std <- apply(jap_ISmatrix_fam, 2, function(x) x/sum(x)) #standardized matrix, colSums = 1
colSums(jap_ISmatrix_fam_std)

jap_ISmatrix_fam2 <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))

#Find which are in common
cols <- colnames(jap_ISmatrix_fam2)[colnames(jap_ISmatrix_fam2) %in% colnames(jap_ISmatrix_fam_std)]
rows <- rownames(jap_ISmatrix_fam2)[rownames(jap_ISmatrix_fam2) %in% rownames(jap_ISmatrix_fam_std)]

jap_ISmatrix_fam2[rows, cols] <- jap_ISmatrix_fam_std[rows, cols] #Matrix with all item grps and fish families

#Colors
#Set fish families boxes colors (upper boxes)
col.fish <- data.frame(ifelse(colSums(jap_ISmatrix_fam2)!= 0,"black","grey60")) #families with values = 0, grey color
colnames(col.fish) <- "color"
col.fish$color <- as.character(col.fish$color)

#Prey colors, same for all of the 5 networks
col.grp6 <- unique(data.frame(data_ISfull_grp_final$grp6)) 
col.grp6 <- data.frame(col.grp6[!is.na(col.grp6[1])])
names(col.grp6)[1] <- "taxon"

colfunc4 <- colorRampPalette(c("darkorchid4", "steelblue","turquoise1","seagreen3", "gold","orange","brown2","lightpink2")) #,"slateblue4", 
#colfunc4 <- colorRampPalette(c("darkorchid4","slateblue4","turquoise1","lightseagreen","gold","coral1","lightpink2"))
plot(rep(1,31),col=(colfunc4(31)),pch=15,cex=3) #plot to see the colours

col.grp6$colors <- colfunc4(31)
col.grp6$colors <- as.character(col.grp6$colors)

#Set prey boxes colors (lower boxes)
box.colors <- data.frame(ifelse(rowSums(jap_ISmatrix_fam2)!= 0, col.grp6$colors,"grey60")) #if sum of rows = 0 (item_class), grey color
colnames(box.colors) <- "color"
box.colors$color <- as.character(box.colors$color)

#plot
plotweb(jap_ISmatrix_fam2, method ="normal", empty = FALSE, arrow="no",text.rot=90, col.interaction = "grey80",
        col.low = box.colors$color, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.10, ybig = 1.3, bor.col.low = box.colors$color, bor.col.high = col.fish$color)


####3. Export clean matrices####
write.csv(vir_ISmatrix_fam2, "data/vir_ISmatrix_fam2.csv")
write.csv(mari_ISmatrix_fam2, "data/mari_ISmatrix_fam2.csv")
write.csv(nca_ISmatrix_fam2, "data/nca_ISmatrix_fam2.csv")
write.csv(mad_ISmatrix_fam2, "data/mad_ISmatrix_fam2.csv")  
write.csv(haw_ISmatrix_fam2, "data/haw_ISmatrix_fam2.csv")
write.csv(jap_ISmatrix_fam2, "data/jap_ISmatrix_fam2.csv")
