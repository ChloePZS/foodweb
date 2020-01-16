########################
#Try accumulation curve#
########################

library(tidyverse)
library(readxl)
library(plyr)
library(bipartite)
library(reshape2)

#Example
library(vegan)
data("BCI")
plot(specaccum(BCI, method="exact"))

#Data prep
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

data_raw_grp <- left_join(data_raw, grp_full2[,c("item_raw","grp6")], all.x=TRUE) %>% 
  filter(time =="day"| !is.na(item_volper) | !is.na(item_freq))
data_raw_grp <- unique(data_raw_grp)


nb_links_guts_g6 <- data_ISfull_grp6 %>% group_by(fish_sp, site_code) %>% dplyr::summarise(n = n_distinct(grp6))

nb_links_guts_g6 <- left_join(nb_links_guts_g6, data_ISfull_grp6[,c("fish_sp","grp6","site_code","nb_sample", "nb_guts","nb_empty", "item_freq")], by=c("fish_sp","site_code"), all.x=TRUE)
nb_links_guts_g6 <- unique(nb_links_guts_g6)
nb_links_guts_g6$nb_empty <- as.numeric(nb_links_guts_g6$nb_empty)


nb_links_guts_g6 <- nb_links_guts_g6 %>% mutate(nb_guts = ifelse(is.na(nb_guts),nb_sample-nb_empty, nb_guts),
                                                nb_int = item_freq * nb_guts) #if nb_guts = NA then replace by nb_sample-nb_empty

#Try with Hawai 
nb_links_haw <- nb_links_guts_g6 %>% filter(site_code == "haw") %>% mutate(int = paste(fish_sp,grp6, sep="-"))

#Matrix with columns as all possible pairwise interaction, columns as census (here nb of individual sampled), data would be the number of interactions
nb_links_matrix_haw <- reshape2::acast(nb_links_haw,  nb_sample ~ int, value.var = "nb_int", mean) #with mean nb_int
nb_links_matrix_haw[is.na(nb_links_matrix_haw)] <- 0

sp1<-specaccum(nb_links_matrix_haw,"random") %>% plot(.) #"random" adds sites in random order
sp2<-specaccum(nb_links_matrix_haw,"collector") %>% plot(.) #Method "collector" adds sites in the order they happen to be in the data
sp3<-specaccum(nb_links_matrix_haw,"exact") %>% plot(.) #"exact" finds the expected (mean) species richness
sp4<-specaccum(nb_links_matrix_haw,"coleman") %>% plot(.) #"coleman" finds the expected richness following Coleman et al. 1982
sp5<-specaccum(nb_links_matrix_haw,"rarefaction") %>% plot(.)#"rarefaction" finds the mean when accumulating individuals instead of sites. 

sp1<-specaccum(nb_links_matrix_haw, "random")
sp2<-specaccum(nb_links_matrix_haw,"random")
tsp2 <- specaccum(t(nb_links_matrix_haw),"random")  %>% plot(.)
plot(sp, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue", xlab= "Number of sampled individuals", ylab="Number of interactions",main="Hawaï")
boxplot(sp2, col="yellow", add=TRUE, pch="+")


ggplot(data=nb_links_haw, aes(x=nb_sample, y=nb_int), na.rm=TRUE) + 
  geom_point(shape = 16, alpha = 0.4) +
  geom_smooth(method="lm",   # Add linear regression line
              se=TRUE) +     # Don't add shaded confidence region
  labs(x= "Number of individual sampled" , y="Number of trophic interactions/species - Grp6") #Here it seems only certain points are highlighted and used for the lm


#Try with Madagascar
nb_links_mad <- nb_links_guts_g6 %>% filter(site_code == "mad") %>% mutate(int = paste(fish_sp,grp6, sep="-"))

#Matrix with columns as all possible pairwise interaction, columns as census (here nb of individual sampled), data would be the number of interactions
nb_links_matrix_mad <- reshape2::acast(nb_links_mad, int ~ nb_sample, value.var = "nb_int", mean) #with mean nb_int
nb_links_matrix_mad[is.na(nb_links_matrix_mad)] <- 0

sp1<-specaccum(t(nb_links_matrix_mad),"random") %>% plot(.) #"random" adds sites in random order
sp2<-specaccum(nb_links_matrix_mad,"collector") %>% plot(.) #Method "collector" adds sites in the order they happen to be in the data
sp3<-specaccum(nb_links_matrix_mad,"exact") %>% plot(.) #"exact" finds the expected (mean) species richness
sp4<-specaccum(nb_links_matrix_mad,"coleman") %>% plot(.) #"coleman" finds the expected richness following Coleman et al. 1982
sp5<-specaccum(nb_links_matrix_mad,"rarefaction") %>% plot(.)#"rarefaction" finds the mean when accumulating individuals instead of sites. 

sp1<-specaccum(nb_links_matrix_mad, "random")
sp2<-specaccum(nb_links_matrix_mad,"random")
plot(sp2, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue", xlab="Number of sampled individuals", ylab="Number of disctint pairwise interactions",main="Madgascar")

ggplot(data=nb_links_mad, aes(x=nb_sample, y=n), na.rm=TRUE) + 
  geom_point(shape = 16, alpha = 0.4) +
  geom_smooth(method="lm",   # Add linear regression line
              se=TRUE) +     # Don't add shaded confidence region
  labs(x= "Number of individual sampled" , y="Number of trophic interactions/species - Grp6")


#Try with New Caledonia
nb_links_nca <- nb_links_guts_g6 %>% filter(site_code == "nca") %>% mutate(int = paste(fish_sp,grp6, sep="-"))

#Matrix with columns as all possible pairwise interaction, columns as census (here nb of individual sampled), data would be the number of interactions
nb_links_matrix_nca <- reshape2::acast(nb_links_nca, nb_sample ~ int, value.var = "nb_int", mean)
nb_links_matrix_nca[is.na(nb_links_matrix_nca)] <- 0

sp1<-specaccum(nb_links_matrix_nca,"random") %>% plot(.) #"random" adds sites in random order
sp2<-specaccum(nb_links_matrix_nca,"collector") %>% plot(.) #Method "collector" adds sites in the order they happen to be in the data
sp3<-specaccum(nb_links_matrix_nca,"exact") %>% plot(.) #"exact" finds the expected (mean) species richness
sp4<-specaccum(nb_links_matrix_nca,"coleman") %>% plot(.) #"coleman" finds the expected richness following Coleman et al. 1982
sp5<-specaccum(nb_links_matrix_nca,"rarefaction") %>% plot(.)#"rarefaction" finds the mean when accumulating individuals instead of sites. 

sp1<-specaccum(nb_links_matrix_nca,"random")
sp2<-specaccum(nb_links_matrix_nca,"random")
plot(sp2, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue", xlab="Number of sampled individuals", ylab="Number of disctint pairwise interactions",main="New Caledonia")
boxplot(sp2, col="yellow", add=TRUE, pch="+")

ggplot(data=nb_links_nca, aes(x=nb_sample, y=nb_int), na.rm=TRUE) + 
  geom_point(shape = 16, alpha = 0.4) +
  geom_smooth(method="lm",   # Add linear regression line
              se=TRUE) +     # Don't add shaded confidence region
  labs(x= "Number of individual sampled" , y="Number of distinct trophic interactions - Grp6")

nb_links_nca %>% filter(nb_sample > 2000) %>% select(nb_guts, nb_int, int)

#Try with Marshall Islands
nb_links_mari <- nb_links_guts_g6 %>% filter(site_code == "mari") %>% mutate(int = paste(fish_sp,grp6, sep="-"))

#Matrix with columns as all possible pairwise interaction, columns as census (here nb of individual sampled), data would be the number of interactions
nb_links_matrix_mari <- reshape2::acast(nb_links_mari, nb_sample ~ int, value.var = "nb_int", mean) 
nb_links_matrix_mari[is.na(nb_links_matrix_mari)] <- 0

sp1<-specaccum(nb_links_matrix_mari,"random") %>% plot(.) #"random" adds sites in random order
sp2<-specaccum(nb_links_matrix_mari,"collector") %>% plot(.) #Method "collector" adds sites in the order they happen to be in the data
sp3<-specaccum(nb_links_matrix_mari,"exact") %>% plot(.) #"exact" finds the expected (mean) species richness
sp4<-specaccum(nb_links_matrix_mari,"coleman") %>% plot(.) #"coleman" finds the expected richness following Coleman et al. 1982
sp5<-specaccum(nb_links_matrix_mari,"rarefaction") %>% plot(.)#"rarefaction" finds the mean when accumulating individuals instead of sites. 

sp1<-specaccum(nb_links_matrix_mari, "random")
sp2<-specaccum(nb_links_matrix_mari, "random")
plot(sp2, ci.type="poly", col="turquoise4", lwd=2, ci.lty=0, ci.col="lightgrey", xlab="Number of sampled individuals", ylab="Number of disctint pairwise interactions",main="Marshall Islands")
boxplot(sp2, col="grey", add=TRUE, pch="+")


ggplot(data=nb_links_mari, aes(x=nb_sample, y=n), na.rm=TRUE) + 
  geom_point(shape = 16, alpha = 0.4) +
  geom_smooth(method="lm",   # Add linear regression line
              se=TRUE) +     # Don't add shaded confidence region
  labs(x= "Number of individual sampled" , y="Number of trophic interactions/species - Grp6")


ggplot(data=nb_links_mari, aes(x=nb_sample, y=nb_int), na.rm=TRUE) + 
  geom_point(shape = 16, alpha = 0.4) +
  geom_smooth(method="lm",   # Add linear regression line
              se=TRUE) +     # Don't add shaded confidence region
  labs(x= "Number of individual sampled" , y="Number of distinct trophic interactions - Grp6")



###
ggplot(data=nb_links_guts_g6, aes(x=nb_sample, y=n, color=site_code), na.rm=TRUE) + 
  geom_point(shape = 16, alpha = 0.4) +
  xlim(0,300) +
  geom_smooth(method="lm",   # Add linear regression line
              se=TRUE) +     # Don't add shaded confidence region
  labs(x= "Number of individual sampled" , y="Number of trophic interactions per species - Grp6")


ggplot(data=nb_links_guts_g6, aes(x=nb_sample, y=nb_int, color=site_code), na.rm=TRUE) + 
  geom_point(shape = 16, alpha = 0.4) +
  xlim(0,300) +
  ylim(0,250) +
  geom_smooth(method="lm",   # Add linear regression line
              se=TRUE) +     # Don't add shaded confidence region
  labs(x= "Number of individual sampled" , y="Number distinct trophic interactions - Grp6")


hist(nb_links_guts_g6$n)
  