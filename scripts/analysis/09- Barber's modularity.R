#####################
#Barber's modularity#
#####################

library(metacom)
library(tidyverse)

#determine Barber"s modularity
mod.mat <- Modularity(pa_vir_sp, method="curveball", sims=1,
                         scores=1, order=TRUE, nstarts=30) #sims = number of null distributions, nstarts = number of runs for the algorithme

#return results : modularity(Q), z-stat, p-val test, mean of null distributions and variance of the null distributions
mod.mat 

#1. Import qualitative matrices (species level using grp6)
pa_vir_sp <- read.csv(pa_vir_sp, "data/pa_vir_sp.csv", sep=",", row.names = 1) %>%
  as.matrix(.)
pa_mari_sp <- read.csv(pa_vir_sp, "data/pa_mari_sp.csv", sep=",", row.names = 1) %>%
  as.matrix(.)
pa_haw_sp <- read.csv(pa_vir_sp, "data/pa_haw_sp.csv", sep=",", row.names = 1) %>%
  as.matrix(.)
pa_mad_sp <- read.csv(pa_vir_sp, "data/pa_mad_sp.csv", sep=",", row.names = 1) %>%
  as.matrix(.)
pa_nca_sp <- read.csv(pa_vir_sp, "data/pa_nca_sp.csv", sep=",", row.names = 1) %>%
  as.matrix(.)

#2. Modularity calculation
mat <- list(pa_vir_sp, pa_mari_sp, pa_haw_sp, pa_mad_sp, pa_nca_sp)

mod <- lapply(mat, function(x){Modularity(x, method="curveball", sims=1,
                                               scores=1, order=TRUE, nstarts=30)}) 

mod.df <- do.call(rbind,mod) %>% as.data.frame() %>%
  add_column(., site = c("vir","mari","haw","mad","nca"))


#3. Null model ...

           