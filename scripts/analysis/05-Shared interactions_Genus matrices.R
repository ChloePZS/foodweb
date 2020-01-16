######################################
#Shared interactions - Genus matrices#
######################################

library(tidyverse)
library(vegan)
library(picante)
library(RColorBrewer)
library(wesanderson)
library(reshape2)
library(matrixStats)

data_ISfull <- read.csv("data/data_ISfull.csv") %>% separate(., fish_sp, into=c("genus","sp")) %>%
  mutate(fish_sp = paste(genus,sp, sep=" "))

data_ISfull %>% filter(genus =="Scarus")#Ok changed to labridae

#1. Import  final matrices
vir_ISmatrix_gen_gen2 <- read.csv("data/vir_ISmatrix_gen_gen2.csv", sep=",", row.names = 1) %>%
  as.matrix(.)
mari_ISmatrix_gen_gen2 <- read.csv("data/mari_ISmatrix_gen_gen2.csv", sep=",", row.names = 1) %>%
  as.matrix(.)
nca_ISmatrix_gen_gen2 <- read.csv("data/nca_ISmatrix_gen_gen2.csv", sep=",", row.names = 1) %>%
  as.matrix(.)
haw_ISmatrix_gen_gen2 <- read.csv("data/haw_ISmatrix_gen_gen2.csv", sep=",", row.names = 1) %>%
  as.matrix(.)
mad_ISmatrix_gen_gen2 <- read.csv("data/mad_ISmatrix_gen_gen2.csv", sep=",", row.names = 1) %>%
  as.matrix(.)


#2. Transform into qualitative matrices
pa_vir_gen <- decostand(vir_ISmatrix_gen_gen2, method="pa")
pa_mari_gen <- decostand(mari_ISmatrix_gen_gen2, method="pa")
pa_nca_gen <- decostand(nca_ISmatrix_gen_gen2, method="pa")
pa_haw_gen <- decostand(haw_ISmatrix_gen_gen2, method="pa")
pa_mad_gen <- decostand(mad_ISmatrix_gen_gen2, method="pa")

pa_sum_gen <- pa_vir_gen + pa_mari_gen + pa_nca_gen + pa_haw_gen + pa_mad_gen


#3. Calculate proportions of shared interactions over all potential interactions ####
chloe <- function(m1, m2, m3, m4, m5, Sum, n) {
  sum(m1[which(Sum>(n-1))],m2[which(Sum>(n-1))], m3[which(Sum>(n-1))], m4[which(Sum>(n-1))], m5[which(Sum>(n-1))]) / sum(sum(colSums(m1)), sum(colSums(m2)), sum(colSums(m3)), sum(colSums(m4)), sum(colSums(m5)))
}#eo function


chloe2 <- function(m1, m2, m3, m4, m5, Sum, n) {
  sum(m1[which(Sum>(n-1))],m2[which(Sum>(n-1))], m3[which(Sum>(n-1))], m4[which(Sum>(n-1))], m5[which(Sum>(n-1))]) / length(which(colMaxs(Sum) == n)) * length(which(rowMaxs(Sum) == n))
}#eo function




length(which(colMaxs(pa_sum_gen) == 5)) * length(which(rowMaxs(pa_sum_gen) == 5))

prop_int_gen <-data.frame(value = c(chloe(vir_ISmatrix_gen_gen2, nca_ISmatrix_gen_gen2, mari_ISmatrix_gen_gen2, mad_ISmatrix_gen_gen2, haw_ISmatrix_gen_gen2, Sum= pa_sum_gen, n=5),  
                              chloe(vir_ISmatrix_gen_gen2, nca_ISmatrix_gen_gen2, mari_ISmatrix_gen_gen2, mad_ISmatrix_gen_gen2, haw_ISmatrix_gen_gen2, Sum= pa_sum_gen, n=4)  ,
                              chloe(vir_ISmatrix_gen_gen2, nca_ISmatrix_gen_gen2, mari_ISmatrix_gen_gen2, mad_ISmatrix_gen_gen2, haw_ISmatrix_gen_gen2, Sum= pa_sum_gen, n=3),
                              chloe(vir_ISmatrix_gen_gen2, nca_ISmatrix_gen_gen2, mari_ISmatrix_gen_gen2, mad_ISmatrix_gen_gen2, haw_ISmatrix_gen_gen2, Sum= pa_sum_gen, n=2)),
                       nb_int = c("5","4","3","2"))

prop_int_gen2 <-data.frame(value = c(chloe2(vir_ISmatrix_gen_gen2, nca_ISmatrix_gen_gen2, mari_ISmatrix_gen_gen2, mad_ISmatrix_gen_gen2, haw_ISmatrix_gen_gen2, Sum= pa_sum_gen, n=5),  
                                    chloe2(vir_ISmatrix_gen_gen2, nca_ISmatrix_gen_gen2, mari_ISmatrix_gen_gen2, mad_ISmatrix_gen_gen2, haw_ISmatrix_gen_gen2, Sum= pa_sum_gen, n=4)  ,
                                    chloe2(vir_ISmatrix_gen_gen2, nca_ISmatrix_gen_gen2, mari_ISmatrix_gen_gen2, mad_ISmatrix_gen_gen2, haw_ISmatrix_gen_gen2, Sum= pa_sum_gen, n=3),
                                    chloe2(vir_ISmatrix_gen_gen2, nca_ISmatrix_gen_gen2, mari_ISmatrix_gen_gen2, mad_ISmatrix_gen_gen2, haw_ISmatrix_gen_gen2, Sum= pa_sum_gen, n=2)),
                          nb_int = c("5","4","3","2"))



#4. Null models####
#create randomized matrices
p <- 1000

rand_vir_gen <- lapply(1:p, function(x) randomizeMatrix(vir_ISmatrix_gen_gen2, null.model = "frequency"))
rand_mari_gen <- lapply(1:p, function(x) randomizeMatrix(mari_ISmatrix_gen_gen2, null.model = "frequency"))
rand_nca_gen <- lapply(1:p, function(x) randomizeMatrix(nca_ISmatrix_gen_gen2, null.model = "frequency"))
rand_mad_gen <- lapply(1:p, function(x) randomizeMatrix(mad_ISmatrix_gen_gen2, null.model = "frequency"))
rand_haw_gen <- lapply(1:p, function(x) randomizeMatrix(haw_ISmatrix_gen_gen2, null.model = "frequency"))

##With function chloe : proportions of interactions relative to the total number of genus
#Fonction to get the index for each of the p permutations
nm_interactions5_gen <- lapply(1:p, function (z) {
  
  chloe <- function(m1, m2, m3, m4, m5, Sum, n) {
    
    sum(m1[which(Sum>(n-1))],m2[which(Sum>(n-1))], m3[which(Sum>(n-1))], m4[which(Sum>(n-1))], m5[which(Sum>(n-1))]) / sum(sum(colSums(m1)), sum(colSums(m2)), sum(colSums(m3)), sum(colSums(m4)), sum(colSums(m5)))
  }#eo function
  
  chloe(rand_vir_gen[[z]], rand_nca_gen[[z]], rand_mad_gen[[z]], rand_mari_gen[[z]], rand_haw_gen[[z]] , Sum= pa_sum_gen, n=5)   
  
})#eo lapply


nm_interactions5_gen <- do.call(rbind, nm_interactions5_gen)

df_nm_interactions2_gen <- data.frame(nm_interactions2_gen) %>% dplyr::mutate(nb_int = 2) %>% dplyr::rename(value = nm_interactions2_gen)
df_nm_interactions3_gen <- data.frame(nm_interactions3_gen) %>% mutate(nb_int = 3) %>% dplyr::rename(value = nm_interactions3_gen)
df_nm_interactions4_gen <- data.frame(nm_interactions4_gen) %>% mutate(nb_int = 4) %>% dplyr::rename(value = nm_interactions4_gen)
df_nm_interactions5_gen <- data.frame(nm_interactions5_gen) %>% mutate(nb_int = 5) %>% dplyr::rename(value = nm_interactions5_gen)

rand_int_gen <- rbind(df_nm_interactions5_gen, df_nm_interactions4_gen, df_nm_interactions3_gen, df_nm_interactions2_gen) %>% mutate(nb_int = factor(nb_int, levels = c(2,3,4,5)))

#Good plots
ggplot(rand_int_gen, aes(x=nb_int, y=value, fill=nb_int)) +
  geom_boxplot(outlier.shape = NA) + 
  scale_fill_viridis_d(option ="D") +
  stat_summary(fun.y = mean, geom = "point", mapping = aes(x=nb_int, y=value), color="black", size = 2.5, shape=19) +
  geom_point(data = prop_int_gen, aes(x=nb_int, y=value), size=3.5, shape=8)+
  labs( x = "Number of shared interactions", y = "Proportion of interactions_genus_chloe") +
  theme_classic() +
  theme(legend.position = "none") 


#with chloe2 : proportions of interactions relative to the number of interactions potentially shared between the 5 regions
nm_interactions5_gen2 <- lapply(1:p, function (z) {
  
  chloe2 <- function(m1, m2, m3, m4, m5, Sum, n) {
    
    sum(m1[which(Sum>(n-1))],m2[which(Sum>(n-1))], m3[which(Sum>(n-1))], m4[which(Sum>(n-1))], m5[which(Sum>(n-1))]) / length(which(colMaxs(Sum) == n)) * length(which(rowMaxs(Sum) == n))
  }#eo function
  
  chloe2(rand_vir_gen[[z]], rand_nca_gen[[z]], rand_mad_gen[[z]], rand_mari_gen[[z]], rand_haw_gen[[z]] , Sum= pa_sum_gen, n=5)   
  
})#eo lapply


nm_interactions5_gen2 <- do.call(rbind, nm_interactions5_gen2)

df_nm_interactions2_gen2 <- data.frame(nm_interactions2_gen2) %>% dplyr::mutate(nb_int = 2) %>% dplyr::rename(value = nm_interactions2_gen2)
df_nm_interactions3_gen2 <- data.frame(nm_interactions3_gen2) %>% mutate(nb_int = 3) %>% dplyr::rename(value = nm_interactions3_gen2)
df_nm_interactions4_gen2 <- data.frame(nm_interactions4_gen2) %>% mutate(nb_int = 4) %>% dplyr::rename(value = nm_interactions4_gen2)
df_nm_interactions5_gen2 <- data.frame(nm_interactions5_gen2) %>% mutate(nb_int = 5) %>% dplyr::rename(value = nm_interactions5_gen2)

rand_int_gen2 <- rbind(df_nm_interactions5_gen2, df_nm_interactions4_gen2, df_nm_interactions3_gen2, df_nm_interactions2_gen2) %>% mutate(nb_int = factor(nb_int, levels = c(2,3,4,5)))

#Good plots
ggplot(rand_int_gen2, aes(x=nb_int, y=value, fill=nb_int)) +
  geom_boxplot(outlier.shape = NA) + 
  scale_fill_viridis_d(option ="D") +
  stat_summary(fun.y = mean, geom = "point", mapping = aes(x=nb_int, y=value), color="black", size = 2.5, shape=19) +
  geom_point(data = prop_int_gen2, aes(x=nb_int, y=value), size=3.5, shape=8)+
  labs( x = "Number of shared interactions", y = "Proportion of interactions_genus_chloe2") +
  theme_classic() +
  theme(legend.position = "none") 

length(which(colMaxs(pa_sum_gen) == 5)) * length(which(rowMaxs(pa_sum_gen) == 5))
length(which(colMaxs(pa_sum_gen) == 4)) * length(which(rowMaxs(pa_sum_gen) == 4))
length(which(colMaxs(pa_sum_gen) == 3)) * length(which(rowMaxs(pa_sum_gen) == 3))
length(which(colMaxs(pa_sum_gen) == 2)) * length(which(rowMaxs(pa_sum_gen) == 2))


length(pa_sum_gen[which(pa_sum_gen == 5)])/10184*100
length(pa_sum_gen[which(pa_sum_gen == 4)])/10184*100
length(pa_sum_gen[which(pa_sum_gen == 3)])/10184*100
length(pa_sum_gen[which(pa_sum_gen == 2)])/10184*100


dim(pa_sum_gen)
length(pa_sum_gen[which(pa_sum_gen != 0)])
sum(colSums(pa_sum_gen))
length(pa_sum_gen[which(pa_sum_gen == 0)])/10184*100

#Could proportions of interactions absent to all regions also relevant ? 

#Get list of shared interactions
genus_nb_int <- setNames(melt(pa_sum_gen),c("prey","genus","nb_int"))
gen_nb_int_haw <- setNames(melt(pa_haw_gen),c("prey","genus","haw"))
gen_nb_int_mari <- setNames(melt(pa_mari_gen),c("prey","genus","mari"))
gen_nb_int_nca <- setNames(melt(pa_nca_gen),c("prey","genus","nca"))
gen_nb_int_mad <- setNames(melt(pa_mad_gen),c("prey","genus","mad"))
gen_nb_int_vir <- setNames(melt(pa_vir_gen),c("prey","genus","vir"))


genus_nb_int <- genus_nb_int %>% left_join(gen_nb_int_haw) %>%
  left_join(gen_nb_int_mari) %>%
  left_join(gen_nb_int_nca) %>%
  left_join(gen_nb_int_mad) %>% 
  left_join(gen_nb_int_vir) %>%
  left_join(data_ISfull[,c("genus","family_cor")], by="genus") %>%
  unique(.) %>%
  filter(nb_int !=0)

write.csv(genus_nb_int, "genus_nb_int.csv")

genus_nb_int %>% group_by(nb_int) %>% tally(nb_int)

genus_nb_int %>% filter(genus=="Scarus")


#5. Correlation shared interactions/interaction strengh #####
#Get the matrices into long format
df_pa_sum_gen <- melt(pa_sum_gen) %>%
  rename(nb_int = value)

#Get the matrices into long format
df_mari_genIS <- melt(mari_ISmatrix_gen2)
#merge 
df_mariIS_gen2 <- left_join(df_mari_genIS, df_pa_sum_gen) %>% filter(nb_int != 0) %>% mutate(site = case_when(value != 0 ~ "mari")) 
df_mariIS_gen2$nb_int <- as.factor(df_mariIS_gen2$nb_int)

df_mariIS_gen2 %>% filter(nb_int == 5) %>% summarize(mean(value)) #check the mean 

#Get the matrices into long format
df_hawIS_gen <- melt(haw_ISmatrix_gen2)
#merge 
df_hawIS_gen2 <- left_join(df_hawIS_gen, df_pa_sum_gen) %>% filter(nb_int != 0) %>% mutate(site = case_when(value != 0 ~ "haw"))
df_hawIS_gen2$nb_int <- as.factor(df_hawIS_gen2$nb_int)

#Get the matrices into long format
df_madIS_gen <- melt(mad_ISmatrix_gen2)

#merge 
df_madIS_gen2 <- left_join(df_madIS_gen, df_pa_sum_gen) %>% filter(nb_int != 0) %>% mutate(site = case_when(value != 0 ~ "mad"))
df_madIS_gen2$nb_int <- as.factor(df_madIS_gen2$nb_int)

#Get the matrices into long format
df_ncaIS_gen <- melt(nca_ISmatrix_gen2)
#merge 
df_ncaIS_gen2 <- left_join(df_ncaIS_gen, df_pa_sum_gen) %>% filter(nb_int != 0) %>% mutate(site = case_when(value != 0 ~ "nca"))
df_ncaIS_gen2$nb_int <- as.factor(df_ncaIS_gen2$nb_int)

#Get the matrices into long format
df_virIS_gen <- melt(vir_ISmatrix_gen2)
#merge 
df_virIS_gen2 <- left_join(df_virIS_gen, df_pa_sum_gen) %>% filter (nb_int != 0) %>% mutate(site = case_when(value != 0 ~ "vir"))
df_virIS_gen2$nb_int <- as.factor(df_virIS_gen2$nb_int)


#full data set
df_allIS_gen <- rbind(df_hawIS_gen2, df_madIS_gen2, df_mariIS_gen2, df_ncaIS_gen2, df_virIS_gen2) 
df_allIS_gen$site <- as.factor(df_allIS_gen$site)
df_allIS_gen <- df_allIS_gen %>% mutate (site_name = case_when(site == "haw" ~ "Hawaï",
                                                       site == "mad" ~ "Madagascar",
                                                       site == "mari" ~ "Marshall Islands",
                                                       site == "nca"~ "New Caledonia",
                                                       site == "vir" ~ "West Indies"))


df_allIS$nb_int <- as.factor(df_allIS$nb_int)
#ggplot(data=subset(df_allIS, value !=0), aes(x = nb_int, y=value, color = nb_int)) + #Remove IS = 0, but
ggplot(data=subset(df_allIS_gen, !is.na(site)), aes(x = nb_int, y=value, color = nb_int)) +
  stat_summary(fun.y = mean, geom="point",size=2) + 
  #stat_summary(fun.data = mean_IC, geom ="errorbar", size=0.3) +
  stat_summary(fun.data = mean_se, geom ="errorbar", size=0.3, width=0.6) +
  stat_summary(fun.y=mean, geom="line", color="black", size=0.1, aes(group=1))+
  scale_color_viridis_d(option ="D") +
  labs(x = "Number of shared interactions", y="Interaction strength") + 
  facet_wrap(. ~ site_name)+
  theme(strip.text.x = element_text(size=10, face="bold"),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(colour = "black", 
                                   size=0.5, 
                                   lineend = "butt"),
        axis.line.y = element_line(colour = "black", 
                                   size=0.5)) +
  ggtitle("Genus level")+
  guides(color=FALSE)



mean_IC <- function(x){
  r <- c(mean(x), mean(x)-1.96*sd(x)/sqrt(length(x)), mean(x) + 1.96*sd(x)/sqrt(length(x)))
  names(r) <- c("y","ymin","ymax")
  r
}
  

         