#Accumulation curve 2.0#

library(tidyverse)
library(vegan)

http://pedroj.github.io/interaccum/
  
#Example --> sampled sites as rows and species as columns
library(vegan)
data("BCI")
plot(specaccum(BCI, method="random"))

#Load data set of each region --> qualitative matrices though
data_vir <- data_ISfull_grp %>% filter(site_code == "vir")
data_mari <- data_ISfull_grp %>% filter(site_code == "mari")
data_haw <- data_ISfull_grp %>% filter(site_code == "haw")
data_nca <- data_ISfull_grp %>% filter(site_code == "nca")
data_mad <- data_ISfull_grp %>% filter(site_code == "mad")

#Could try to have the pairwise interaction as columns, samples (nb indiv) as rows and 0-1 value if interaction
#Problem is data pooled by species don't have diet for each individual sampled....

#Try with Vir
int_vir_mat <- data_vir %>% mutate(pairwise_int = paste(fish_sp,grp6, sep="-")) %>%
  group_by(fish_sp) %>%
  mutate(nb_int = n_distinct(grp6)) %>%
  reshape2::acast(., nb_sample ~ pairwise_int, value.var = "nb_int") #66 rows and 969 pairwise interactions
int_vir_mat[is.na(int_vir_mat)]  <- 0 
int_vir_mat[int_vir_mat != 0] <- 1

specaccum(int_vir_mat,"random") %>% plot(.)
specaccum(int_vir_mat,"collector") %>% plot(.)
specaccum(int_vir_mat,"exact") %>% plot(.)

#Try simple relation btw nb of pairwise interaction per species fct nb of individual sampled
nb_int_grp6 <- data_ISfull_grp %>% group_by(fish_sp, site_code) %>% dplyr::summarise(nb_int = n_distinct(grp6)) %>%
  left_join(., data_ISfull_grp[ ,c("fish_sp","site_code","nb_sample","nb_guts")], by=c("fish_sp","site_code")) %>%
  unique(.)

nb_int_grp6 <- nb_int_grp6 %>% group_by(fish_sp, site_code, nb_sample,nb_int) %>% #get the mean of nb_guts for species and site
  summarise_at(.vars = names(.)[5], .funs = c(mean.nb_guts = "mean")) 

ggplot(data = nb_int_grp6, aes(x=nb_sample, y=nb_int, color=site_code)) +
  geom_point(shape = 16, alpha = 0.4) +
  xlim(0,300) +
  geom_smooth(method="lm",   # Add linear regression line
              se=TRUE)



#### Possible only with New Caledonia data set --> data per individual

