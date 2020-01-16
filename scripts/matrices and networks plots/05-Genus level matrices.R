################
#Genus matrices#
################


library(tidyverse)
library(plyr)

#full clean data set
data_ISfull <- read.csv("data/data_ISfull.csv") %>% separate(., fish_sp, into=c("genus","sp")) %>%
  mutate(fish_sp = paste(genus,sp, sep=" "))

#Need to create a genus column for each of the clean data sets
data_nca_grp2 <- data_nca_grp %>% separate(., fish_sp, into=c("genus","sp")) %>%
  mutate(fish_sp = paste(genus,sp, sep=" "))
data_vir_grp2 <- data_vir_grp %>% separate(., fish_sp, into=c("genus","sp")) %>%
  mutate(fish_sp = paste(genus,sp, sep=" "))
data_mari_grp2 <- data_mari_grp %>% separate(., fish_sp, into=c("genus","sp"))  %>%
  mutate(fish_sp = paste(genus,sp, sep=" "))
data_haw_grp2 <- data_haw_grp %>% separate(., fish_sp, into=c("genus","sp")) %>%
  mutate(fish_sp = paste(genus,sp, sep=" "))
data_mad_grp2 <- data_mad_grp %>% separate(., fish_sp, into=c("genus","sp")) %>%
  mutate(fish_sp = paste(genus,sp, sep=" "))


#Check number of genus commun to all 5 sites
intersect_all <- function(a,b,...){
  Reduce(intersect, list(a,b,...))
}
gen_com <- intersect_all(data_nca_grp2$genus, data_vir_grp2$genus, data_mari_grp2$genus, data_mad_grp2$genus, data_haw_grp2$genus) #Just 5 genus commun to all locations 


    ####1. Mean IS per genus####

####Virgin Islands####
#Need to "sum up" the % volume according to the categorie of grp6 : if 7% of shrimp and 3% of crab = 10% Decapoda
data_vir_grp2$item_volper <- as.numeric(data_vir_grp2$item_volper)
data_vir_gen_sum <- data_vir_grp2  %>%
  plyr::ddply(.(genus, fish_sp, grp6), summarise, sum = sum(item_volper))

#group_by(family_cor, grp6) %>%
#dplyr::mutate(mean_fam = mean(sum))

nb_spbygen <- data_vir_gen_sum %>% group_by(genus) %>%
  dplyr::summarise(n = n_distinct(fish_sp))

data_vir_gen_sum <- data_vir_gen_sum %>% dplyr::left_join(nb_spbygen[,c("genus", "n")], by="genus")

data_vir_gen_sum <-data_vir_gen_sum %>% group_by(genus, grp6) %>%
  dplyr::mutate(mean_gen = sum(sum)/n)

#Keep only genus rows
data_vir_sum_genus <- data_vir_gen_sum %>% select(genus, grp6, mean_gen)
data_vir_sum_genus <- unique(data_vir_sum_genus)


####Marshall Islands####
#Get the mean item_freq by group and family
data_mari_gen_sum <- data_mari_grp2  %>%
  plyr::ddply(.(genus, fish_sp, grp6), summarise, mean = mean(item_freq))

nb_spbygen <- data_mari_gen_sum %>% group_by(genus) %>%
  dplyr::summarise(n = n_distinct(fish_sp))

data_mari_gen_sum <- data_mari_gen_sum %>% dplyr::left_join(nb_spbygen[,c("genus", "n")], by="genus")

data_mari_gen_sum <-data_mari_gen_sum %>% group_by(genus, grp6) %>%
  dplyr::mutate(mean_gen = sum(mean)/n) #item freq by genus is the sum of item_freq divided by the nb of species 

data_mari_sum_genus <- data_mari_gen_sum %>% select(genus, grp6, mean_gen)
data_mari_sum_genus <- unique(data_mari_sum_genus)

####Madagascar####
#Get the mean item_freq by group and family
data_mad_gen_sum <- data_mad_grp2  %>%
  plyr::ddply(.(genus, fish_sp, grp6), summarise, mean = mean(item_freq)) #mean by group6 for each species

nb_spbygen <- data_mad_gen_sum %>% group_by(genus) %>%
  dplyr::summarise(n = n_distinct(fish_sp))

data_mad_gen_sum <- data_mad_gen_sum %>% dplyr::left_join(nb_spbygen[,c("genus", "n")], by="genus")

data_mad_gen_sum <-data_mad_gen_sum %>% group_by(genus, grp6) %>%
  dplyr::mutate(mean_gen = sum(mean)/n) #item freq by genus is the sum of item_freq divided by the nb of species 

data_mad_sum_genus <- data_mad_gen_sum %>% select(genus, grp6, mean_gen)
data_mad_sum_genus <- unique(data_mad_sum_genus)


####Hawai####
#Get the mean item_freq by group and family
data_haw_gen_sum <- data_haw_grp2  %>%
  plyr::ddply(.(genus, fish_sp, grp6), summarise, mean = mean(item_freq)) #mean by group6 for each species

nb_spbygen <- data_haw_gen_sum %>% group_by(genus) %>%
  dplyr::summarise(n = n_distinct(fish_sp))

data_haw_gen_sum <- data_haw_gen_sum %>% dplyr::left_join(nb_spbygen[,c("genus", "n")], by="genus")

data_haw_gen_sum <-data_haw_gen_sum %>% group_by(genus, grp6) %>%
  dplyr::mutate(mean_gen = sum(mean)/n) #item freq by genus is the sum of item_freq divided by the nb of species 

data_haw_sum_genus <- data_haw_gen_sum %>% select(genus, grp6, mean_gen)
data_haw_sum_genus <- unique(data_haw_sum_genus)

####New Caledonia####
#Get the mean item_freq by group and family
data_nca_gen_sum <- data_nca_grp2  %>%
  plyr::ddply(.(genus, fish_sp, grp6), summarise, mean = mean(item_freq)) #mean by group6 for each species

nb_spbygen <- data_nca_gen_sum %>% group_by(genus) %>%
  dplyr::summarise(n = n_distinct(fish_sp))

data_nca_gen_sum <- data_nca_gen_sum %>% dplyr::left_join(nb_spbygen[,c("genus", "n")], by="genus")

data_nca_gen_sum <-data_nca_gen_sum %>% group_by(genus, grp6) %>%
  dplyr::mutate(mean_gen = sum(mean)/n) #item freq by genus is the sum of item_freq divided by the nb of species 

data_nca_sum_genus <- data_nca_gen_sum %>% select(genus, grp6, mean_gen)
data_nca_sum_genus <- unique(data_nca_sum_genus)




    ####2. Matrices ####

#Create nodes 
n <- unique(data_ISfull$grp6)
n <- n[order(n)] %>%
  as.character(.)
m <- unique(data_ISfull$genus) #268 genus
m <- m[order(m)] #alphabetic order 

####Virgin Islands####
#Now need to have a matrix with the % per genus
vir_ISmatrix_gen <- reshape2::acast(data_vir_sum_genus, grp6 ~ genus, value.var = "mean_gen") 
vir_ISmatrix_gen[is.na(vir_ISmatrix_gen)] <- 0  

vir_ISmatrix_gen_std <- apply(vir_ISmatrix_gen, 2, function(x) x/sum(x)) #standardized matrix, colSums = 1
colSums(vir_ISmatrix_gen_std)

vir_ISmatrix_gen2 <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))

#Find which are in common
cols <- colnames(vir_ISmatrix_gen2)[colnames(vir_ISmatrix_gen2) %in% colnames(vir_ISmatrix_gen_std)]
rows <- rownames(vir_ISmatrix_gen2)[rownames(vir_ISmatrix_gen2) %in% rownames(vir_ISmatrix_gen_std)]

vir_ISmatrix_gen2[rows, cols] <- vir_ISmatrix_gen_std[rows, cols] #Matrix with all item grps and fish families
colSums(vir_ISmatrix_gen2)
colnames(vir_ISmatrix_gen2)
rownames(vir_ISmatrix_gen2)

####Marshall Islands####
#Now need to have a matrix with the % per genus
mari_ISmatrix_gen <- reshape2::acast(data_mari_sum_genus, grp6 ~ genus, value.var = "mean_gen") 
mari_ISmatrix_gen[is.na(mari_ISmatrix_gen)] <- 0  

mari_ISmatrix_gen_std <- apply(mari_ISmatrix_gen, 2, function(x) x/sum(x)) #standardized matrix, colSums = 1
colSums(mari_ISmatrix_gen_std)

mari_ISmatrix_gen2 <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))

#Find which are in common
cols <- colnames(mari_ISmatrix_gen2)[colnames(mari_ISmatrix_gen2) %in% colnames(mari_ISmatrix_gen_std)]
rows <- rownames(mari_ISmatrix_gen2)[rownames(mari_ISmatrix_gen2) %in% rownames(mari_ISmatrix_gen_std)]

mari_ISmatrix_gen2[rows, cols] <- mari_ISmatrix_gen_std[rows, cols] #Matrix with all item grps and fish families
colSums(mari_ISmatrix_gen2)
colnames(mari_ISmatrix_gen2)
rownames(mari_ISmatrix_gen2)

####Madagascar####
#Now need to have a matrix with the % per genus
mad_ISmatrix_gen <- reshape2::acast(data_mad_sum_genus, grp6 ~ genus, value.var = "mean_gen") 
mad_ISmatrix_gen[is.na(mad_ISmatrix_gen)] <- 0  

mad_ISmatrix_gen_std <- apply(mad_ISmatrix_gen, 2, function(x) x/sum(x)) #standardized matrix, colSums = 1
colSums(mad_ISmatrix_gen_std)

mad_ISmatrix_gen2 <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))

#Find which are in common
cols <- colnames(mad_ISmatrix_gen2)[colnames(mad_ISmatrix_gen2) %in% colnames(mad_ISmatrix_gen_std)]
rows <- rownames(mad_ISmatrix_gen2)[rownames(mad_ISmatrix_gen2) %in% rownames(mad_ISmatrix_gen_std)]

mad_ISmatrix_gen2[rows, cols] <- mad_ISmatrix_gen_std[rows, cols] #Matrix with all item grps and fish families
colSums(mad_ISmatrix_gen2)
colnames(mad_ISmatrix_gen2)
rownames(mad_ISmatrix_gen2)

####Hawai####
#Now need to have a matrix with the % per genus
haw_ISmatrix_gen <- reshape2::acast(data_haw_sum_genus, grp6 ~ genus, value.var = "mean_gen") 
haw_ISmatrix_gen[is.na(haw_ISmatrix_gen)] <- 0  

haw_ISmatrix_gen_std <- apply(haw_ISmatrix_gen, 2, function(x) x/sum(x)) #standardized matrix, colSums = 1
colSums(haw_ISmatrix_gen_std)

haw_ISmatrix_gen2 <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))

#Find which are in common
cols <- colnames(haw_ISmatrix_gen2)[colnames(haw_ISmatrix_gen2) %in% colnames(haw_ISmatrix_gen_std)]
rows <- rownames(haw_ISmatrix_gen2)[rownames(haw_ISmatrix_gen2) %in% rownames(haw_ISmatrix_gen_std)]

haw_ISmatrix_gen2[rows, cols] <- haw_ISmatrix_gen_std[rows, cols] #Matrix with all item grps and fish families
colSums(haw_ISmatrix_gen2)
colnames(haw_ISmatrix_gen2)
rownames(haw_ISmatrix_gen2)

####New Caledonia####
#Now need to have a matrix with the % per genus
nca_ISmatrix_gen <- reshape2::acast(data_nca_sum_genus, grp6 ~ genus, value.var = "mean_gen") 
nca_ISmatrix_gen[is.na(nca_ISmatrix_gen)] <- 0  

nca_ISmatrix_gen_std <- apply(nca_ISmatrix_gen, 2, function(x) x/sum(x)) #standardized matrix, colSums = 1
colSums(nca_ISmatrix_gen_std)

nca_ISmatrix_gen2 <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))

#Find which are in common
cols <- colnames(nca_ISmatrix_gen2)[colnames(nca_ISmatrix_gen2) %in% colnames(nca_ISmatrix_gen_std)]
rows <- rownames(nca_ISmatrix_gen2)[rownames(nca_ISmatrix_gen2) %in% rownames(nca_ISmatrix_gen_std)]

nca_ISmatrix_gen2[rows, cols] <- nca_ISmatrix_gen_std[rows, cols] #Matrix with all item grps and fish families
colSums(nca_ISmatrix_gen2)
colnames(nca_ISmatrix_gen2)
rownames(nca_ISmatrix_gen2)

  ####3. Export clean matrices####
write.csv(vir_ISmatrix_gen2, "data/vir_ISmatrix_gen2.csv")
write.csv(mari_ISmatrix_gen2, "data/mari_ISmatrix_gen2.csv")
write.csv(nca_ISmatrix_gen2, "data/nca_ISmatrix_gen2.csv")
write.csv(mad_ISmatrix_gen2, "data/mad_ISmatrix_gen2.csv")  
write.csv(haw_ISmatrix_gen2, "data/haw_ISmatrix_gen2.csv")