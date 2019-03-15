##################
#Tripartite plots#
##################

library(tidyverse)
library(bipartite)

#Getting one dataset per site
haw <- data_clean2 %>% filter(site_code=="haw")
vir <- data_clean2 %>% filter(site_code=="vir")
mari <- data_clean2 %>% filter(site_code=="mari")
mad <- data_clean2 %>% filter(site_code=="mad")

#Check fish_sp as prey items

vir %>% filter(!is.na(item_fam_cor), item_class=="Actinopterygii") %>% count()

#Creating list of all fish families and fish prey items
fish.vir <- data_clean2 %>% filter(!is.na(item_fam_cor), item_class=="Actinopterygii", site_code=="vir")

n <- unique(vir$item_fam_cor[!is.na(vir$item_fam_cor) & vir$item_class=="Actinopterygii"]) #preys
n <- n[order(n)] #47 fish prey
m <- unique(vir$family_cor) #predators
m<- m[order(m)] #alphabetic order 

#Fish pred/prey matrix for Virgin Islands
x <- with(vir, table(vir$item_fam_cor[!is.na(vir$item_fam_cor) & vir$item_class=="Actinopterygii"], vir$family_cor[!is.na(vir$item_fam_cor) & vir$item_class=="Actinopterygii"]))
fish.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
fish.matrix[i1] <- x[i1]

dim(fish.matrix)

fish.matrix.std <- apply(fish.matrix, 2,function(x) x/sum(x))
fish.matrix.std[is.na(fish.matrix.std)] <- 0

#Fish pred/other preys matrix
vir.fishprey <- unique(vir$item_fam_cor[!is.na(vir$item_fam_cor) & vir$item_class=="Actinopterygii"])
unique(vir.fishprey)

vir.fish <- vir %>% filter(vir$family_cor %in% vir.fishprey)

n <- unique(vir.fish$grp3) #prey
n <- n[order(n)]
m <- unique(vir.fish$family_cor) #pred same as the preys from the fish matrix
m <- m[order(m)] #alphabetic order  

#Virgin Islands
x <- with(vir, table(vir.fish$grp3, vir.fish$family_cor))
vir.matrix <- matrix(0, ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
vir.matrix[i1] <- x[i1]

dim(vir.matrix)

vir.matrix.std <- apply(vir.matrix, 2,function(x) x/sum(x))

vir.matrix.std[is.na(vir.matrix.std)] <- 0 #replace the NA values by 0

rownames(vir.matrix.std) #look for item as NA

vir.matrix.std <- vir.matrix.std[-41,] #removing items as NA

plotweb2(fish.matrix, vir.matrix, method="normal", empty=TRUE,method2 = "normal", empty2 = TRUE)
