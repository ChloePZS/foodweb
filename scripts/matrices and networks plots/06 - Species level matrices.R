####################
#Matrices _ species#
####################

library(tidyverse)
library(plyr)

#Create a subset for each region. data_ISfull_grp_final --> data without single and doubletones
data_vir <- data_ISfull_grp_final2 %>% filter(site_code == "vir")
data_mari <- data_ISfull_grp_final2 %>% filter(site_code == "mari")
data_haw <- data_ISfull_grp_final2 %>% filter(site_code == "haw")
data_nca <- data_ISfull_grp_final2 %>% filter(site_code == "nca")
data_mad <- data_ISfull_grp_final2 %>% filter(site_code == "mad")
data_jap <- data_ISfull_grp_final2 %>% filter(site_code == "jap")
#Check number of species commun to all 5 sites
#intersect_all <- function(a,b,...){
  #Reduce(intersect, list(a,b,...))
#}
#intersect_all(data_nca$fish_sp, data_vir$fish_sp, data_mari$fish_sp, data_mad$fish_sp, data_haw$fish_sp)#no species in commun to all 5 sites


    #1. Info by species#####
#Choose here of matrices for gpr6 (most comparable), grp1 (most detailed) or prey classes

#Virgin Islands
#Need to "sum up" the % volume according to the categorie of item ....: if 7% of shrimp and 3% of crab = 10% Decapoda
data_vir_sp_sum <- data_vir  %>%
  plyr::ddply(.(fish_sp, grp6), summarise, sum = sum(item_volper))

#Marshall Islands#
#Get the mean item_freq by group and family
data_mari_sp_sum <- data_mari%>%
  plyr::ddply(.(fish_sp, grp6), summarise, mean = mean(item_freq))

#Hawaii#
#Get the mean item_freq by group and family
data_haw_sp_sum <- data_haw %>%
  plyr::ddply(.(fish_sp, grp6), summarise, mean = mean(item_freq))

#New Caledonia#
#Get the mean item_freq by group and family
data_nca_sp_sum <- data_nca %>%
  plyr::ddply(.(fish_sp, grp6), summarise, mean = mean(item_freq))

#Madagascar#
#Get the mean item_freq by group and family
data_mad_sp_sum <- data_mad %>%
  plyr::ddply(.(fish_sp, grp6), summarise, mean = mean(item_freq))

#Japan#
#Get the mean item_freq by group and family
data_jap_sp_sum <- data_jap %>% filter(!is.na(item_freq)) %>%
  plyr::ddply(.(fish_sp, grp6), summarise, mean = mean(item_freq))


    #2. Matrices ####
#Create nodes 
n <- unique(data_ISfull_grp_final2$grp6)
n <- n[order(n)] %>%
  as.character(.)
m <- unique(data_ISfull_grp_final2$fish_sp) #688 species
m <- m[order(m)] #alphabetic order 


####Virgin Islands####
vir_ISmatrix_sp <- reshape2::acast(data_vir_sp_sum, grp6 ~ fish_sp, value.var = "sum") 
vir_ISmatrix_sp[is.na(vir_ISmatrix_sp)] <- 0  

vir_ISmatrix_sp_std <- apply(vir_ISmatrix_sp, 2, function(x) x/sum(x)) #standardized matrix, colSums = 1
colSums(vir_ISmatrix_sp_std)

#Create matrice with all nodes from the 5 regions
vir_ISmatrix_sp2 <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m)) 
#Find which are in common
cols <- colnames(vir_ISmatrix_sp2)[colnames(vir_ISmatrix_sp2) %in% colnames(vir_ISmatrix_sp_std)]
rows <- rownames(vir_ISmatrix_sp2)[rownames(vir_ISmatrix_sp2) %in% rownames(vir_ISmatrix_sp_std)]

vir_ISmatrix_sp2[rows, cols] <- vir_ISmatrix_sp_std[rows, cols] #Matrix with all item grps and fish families
colSums(vir_ISmatrix_sp2)


####Marshall Islands####
mari_ISmatrix_sp <- reshape2::acast(data_mari_sp_sum, grp6 ~ fish_sp, value.var = "mean") 
mari_ISmatrix_sp[is.na(mari_ISmatrix_sp)] <- 0  

mari_ISmatrix_sp_std <- apply(mari_ISmatrix_sp, 2, function(x) x/sum(x)) #standardized matrix, colSums = 1
colSums(mari_ISmatrix_sp_std)

mari_ISmatrix_sp2 <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))

#Find which are in common
cols <- colnames(mari_ISmatrix_sp2)[colnames(mari_ISmatrix_sp2) %in% colnames(mari_ISmatrix_sp_std)]
rows <- rownames(mari_ISmatrix_sp2)[rownames(mari_ISmatrix_sp2) %in% rownames(mari_ISmatrix_sp_std)]

mari_ISmatrix_sp2[rows, cols] <- mari_ISmatrix_sp_std[rows, cols] #Matrix with all item grps and fish families
colSums(mari_ISmatrix_sp2)


####Hawai####
haw_ISmatrix_sp <- reshape2::acast(data_haw_sp_sum, grp6 ~ fish_sp, value.var = "mean") 
haw_ISmatrix_sp[is.na(haw_ISmatrix_sp)] <- 0  

haw_ISmatrix_sp_std <- apply(haw_ISmatrix_sp, 2, function(x) x/sum(x)) #standardized matrix, colSums = 1
colSums(haw_ISmatrix_sp_std)

haw_ISmatrix_sp2 <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))

#Find which are in common
cols <- colnames(haw_ISmatrix_sp2)[colnames(haw_ISmatrix_sp2) %in% colnames(haw_ISmatrix_sp_std)]
rows <- rownames(haw_ISmatrix_sp2)[rownames(haw_ISmatrix_sp2) %in% rownames(haw_ISmatrix_sp_std)]

haw_ISmatrix_sp2[rows, cols] <- haw_ISmatrix_sp_std[rows, cols] #Matrix with all item grps and fish families
colSums(haw_ISmatrix_sp2)


####New Caledonia####
nca_ISmatrix_sp <- reshape2::acast(data_nca_sp_sum, grp6 ~ fish_sp, value.var = "mean") 
nca_ISmatrix_sp[is.na(nca_ISmatrix_sp)] <- 0  

nca_ISmatrix_sp_std <- apply(nca_ISmatrix_sp, 2, function(x) x/sum(x)) #standardized matrix, colSums = 1
colSums(nca_ISmatrix_sp_std)

nca_ISmatrix_sp2 <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))

#Find which are in common
cols <- colnames(nca_ISmatrix_sp2)[colnames(nca_ISmatrix_sp2) %in% colnames(nca_ISmatrix_sp_std)]
rows <- rownames(nca_ISmatrix_sp2)[rownames(nca_ISmatrix_sp2) %in% rownames(nca_ISmatrix_sp_std)]

nca_ISmatrix_sp2[rows, cols] <- nca_ISmatrix_sp_std[rows, cols] #Matrix with all item grps and fish families
colSums(nca_ISmatrix_sp2)


####Madagascar####
mad_ISmatrix_sp <- reshape2::acast(data_mad_sp_sum, grp6 ~ fish_sp, value.var = "mean") 
mad_ISmatrix_sp[is.na(mad_ISmatrix_sp)] <- 0  

mad_ISmatrix_sp_std <- apply(mad_ISmatrix_sp, 2, function(x) x/sum(x)) #standardized matrix, colSums = 1
colSums(mad_ISmatrix_sp_std)

mad_ISmatrix_sp2 <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m)) #matrice avec l'ensemble des noeuds

#Find which are in common
cols <- colnames(mad_ISmatrix_sp2)[colnames(mad_ISmatrix_sp2) %in% colnames(mad_ISmatrix_sp_std)]
rows <- rownames(mad_ISmatrix_sp2)[rownames(mad_ISmatrix_sp2) %in% rownames(mad_ISmatrix_sp_std)]

mad_ISmatrix_sp2[rows, cols] <- mad_ISmatrix_sp_std[rows, cols] #Matrix with all item grps and fish families
colSums(mad_ISmatrix_sp2)

####Japan####
jap_ISmatrix_sp <- reshape2::acast(data_jap_sp_sum, grp6 ~ fish_sp, value.var = "mean") 
jap_ISmatrix_sp[is.na(jap_ISmatrix_sp)] <- 0  

jap_ISmatrix_sp_std <- apply(jap_ISmatrix_sp, 2, function(x) x/sum(x)) #standardized matrix, colSums = 1
colSums(jap_ISmatrix_sp_std)

jap_ISmatrix_sp2 <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m)) #matrice avec l'ensemble des noeuds

#Find which are in common
cols <- colnames(jap_ISmatrix_sp2)[colnames(jap_ISmatrix_sp2) %in% colnames(jap_ISmatrix_sp_std)]
rows <- rownames(jap_ISmatrix_sp2)[rownames(jap_ISmatrix_sp2) %in% rownames(jap_ISmatrix_sp_std)]

jap_ISmatrix_sp2[rows, cols] <- jap_ISmatrix_sp_std[rows, cols] #Matrix with all item grps and fish families
colSums(jap_ISmatrix_sp2)


    #3. Export clean matrices####
#Change the name according to the prey category used
write.csv(vir_ISmatrix_sp_std, "data/vir_ISmatrix_sp_std_grp6.csv")
write.csv(mari_ISmatrix_sp_std, "data/mari_ISmatrix_sp_std_grp6.csv")
write.csv(nca_ISmatrix_sp_std, "data/nca_ISmatrix_sp_std_grp6.csv")
write.csv(mad_ISmatrix_sp_std, "data/mad_ISmatrix_sp_std_grp6.csv")  
write.csv(haw_ISmatrix_sp_std, "data/haw_ISmatrix_sp_std_grp6.csv")
write.csv(jap_ISmatrix_sp_std, "data/jap_ISmatrix_sp_std_grp6.csv")

#Class matrices without NA's
vir_ISmatrix_sp <- reshape2::acast(data_vir_sp_sum, item_class ~ fish_sp, value.var = "sum") 
vir_ISmatrix_sp[is.na(vir_ISmatrix_sp)] <- 0  
vir_ISmatrix_sp <- vir_ISmatrix_sp[-30,] #remove last row with prey items with class as "NA"
vir_ISmatrix_sp_std <- apply(vir_ISmatrix_sp, 2, function(x) x/sum(x))#standardized matrix, colSums = 1
vir_ISmatrix_sp_std <- vir_ISmatrix_sp_std[, colSums(is.na(vir_ISmatrix_sp_std)) != nrow(vir_ISmatrix_sp_std)] #remove columns with NA"

mari_ISmatrix_sp <- reshape2::acast(data_mari_sp_sum, item_class ~ fish_sp, value.var = "mean") 
mari_ISmatrix_sp[is.na(mari_ISmatrix_sp)] <- 0  
mari_ISmatrix_sp <- mari_ISmatrix_sp[-24,]
mari_ISmatrix_sp_std <- apply(mari_ISmatrix_sp, 2, function(x) x/sum(x)) #standardized matrix, colSums = 1
mari_ISmatrix_sp_std <-mari_ISmatrix_sp_std[, colSums(is.na(mari_ISmatrix_sp_std)) != nrow(mari_ISmatrix_sp_std)]


mad_ISmatrix_sp <- reshape2::acast(data_mad_sp_sum, item_class ~ fish_sp, value.var = "mean") 
mad_ISmatrix_sp[is.na(mad_ISmatrix_sp)] <- 0  
mad_ISmatrix_sp <- mad_ISmatrix_sp[-21,]
mad_ISmatrix_sp_std <- apply(mad_ISmatrix_sp, 2, function(x) x/sum(x)) #standardized matrix, colSums = 1
mad_ISmatrix_sp_std <-mad_ISmatrix_sp_std[, colSums(is.na(mad_ISmatrix_sp_std)) != nrow(mad_ISmatrix_sp_std)]


nca_ISmatrix_sp <- reshape2::acast(data_nca_sp_sum, item_class ~ fish_sp, value.var = "mean") 
nca_ISmatrix_sp[is.na(nca_ISmatrix_sp)] <- 0  
nca_ISmatrix_sp <- nca_ISmatrix_sp[-16,]
nca_ISmatrix_sp_std <- apply(nca_ISmatrix_sp, 2, function(x) x/sum(x)) #standardized matrix, colSums = 1
nca_ISmatrix_sp_std <-nca_ISmatrix_sp_std[, colSums(is.na(nca_ISmatrix_sp_std)) != nrow(nca_ISmatrix_sp_std)]


haw_ISmatrix_sp <- reshape2::acast(data_haw_sp_sum, item_class ~ fish_sp, value.var = "mean") 
haw_ISmatrix_sp[is.na(haw_ISmatrix_sp)] <- 0  
haw_ISmatrix_sp <- haw_ISmatrix_sp[-24,]
haw_ISmatrix_sp_std <- apply(haw_ISmatrix_sp, 2, function(x) x/sum(x)) #standardized matrix, colSums = 1
haw_ISmatrix_sp_std <-haw_ISmatrix_sp_std[, colSums(is.na(haw_ISmatrix_sp_std)) != nrow(haw_ISmatrix_sp_std)]


jap_ISmatrix_sp <- reshape2::acast(data_jap_sp_sum, item_class ~ fish_sp, value.var = "mean") 
jap_ISmatrix_sp[is.na(jap_ISmatrix_sp)] <- 0  
jap_ISmatrix_sp <- jap_ISmatrix_sp[-19,]
jap_ISmatrix_sp_std <- apply(jap_ISmatrix_sp, 2, function(x) x/sum(x)) #standardized matrix, colSums = 1
jap_ISmatrix_sp_std <-jap_ISmatrix_sp_std[, colSums(is.na(jap_ISmatrix_sp_std)) != nrow(jap_ISmatrix_sp_std)] #enlève colonnes dont somme différente nb de lines (cad sp où NA partout)

