########################
#Trophic specialization#
########################

library(tidyverse)
library(plotrix)

  #1. Binary matrices
pa_vir_sp <- decostand(vir_ISmatrix_sp_std, method="pa")
pa_mari_sp <- decostand(mari_ISmatrix_sp_std, method="pa")
pa_nca_sp <- decostand(nca_ISmatrix_sp_std, method="pa")
pa_haw_sp <- decostand(haw_ISmatrix_sp_std, method="pa")
pa_mad_sp <- decostand(mad_ISmatrix_sp_std, method="pa")
pa_jap_sp <- decostand(jap_ISmatrix_sp_std, method="pa")


  #2. Binary matrices with thresholds
#0.5
vir_ISmatrix_sp_0.5 <- vir_ISmatrix_sp_std * (vir_ISmatrix_sp_std > 0.5) 
vir_pa_0.5 <- decostand(vir_ISmatrix_sp_0.5, method = "pa")

nca_ISmatrix_sp_0.5 <- nca_ISmatrix_sp_std * (nca_ISmatrix_sp_std > 0.5) 
nca_pa_0.5 <- decostand (nca_ISmatrix_sp_0.5, method = "pa")

mari_ISmatrix_sp_0.5 <- mari_ISmatrix_sp_std * (mari_ISmatrix_sp_std > 0.5) 
mari_pa_0.5 <- decostand (mari_ISmatrix_sp_0.5, method = "pa")

mad_ISmatrix_sp_0.5 <- mad_ISmatrix_sp_std * (mad_ISmatrix_sp_std > 0.5)
mad_pa_0.5 <- decostand (mad_ISmatrix_sp_0.5, method = "pa")

haw_ISmatrix_sp_0.5 <- haw_ISmatrix_sp_std * (haw_ISmatrix_sp_std > 0.5)
haw_pa_0.5 <- decostand (haw_ISmatrix_sp_0.5, method = "pa")

jap_ISmatrix_sp_0.5 <- jap_ISmatrix_sp_std * (jap_ISmatrix_sp_std > 0.5)
jap_pa_0.5 <- decostand (jap_ISmatrix_sp_0.5, method = "pa")

#0.25
vir_ISmatrix_sp_0.25 <- vir_ISmatrix_sp_std * (vir_ISmatrix_sp_std > 0.25) 
vir_pa_0.25 <- decostand(vir_ISmatrix_sp_0.25, method = "pa")

nca_ISmatrix_sp_0.25 <- nca_ISmatrix_sp_std * (nca_ISmatrix_sp_std > 0.25) 
nca_pa_0.25 <- decostand (nca_ISmatrix_sp_0.25, method = "pa")

mari_ISmatrix_sp_0.25 <- mari_ISmatrix_sp_std * (mari_ISmatrix_sp_std > 0.25) 
mari_pa_0.25 <- decostand (mari_ISmatrix_sp_0.25, method = "pa")

mad_ISmatrix_sp_0.25 <- mad_ISmatrix_sp_std * (mad_ISmatrix_sp_std > 0.25)
mad_pa_0.25 <- decostand (mad_ISmatrix_sp_0.25, method = "pa")

haw_ISmatrix_sp_0.25 <- haw_ISmatrix_sp_std * (haw_ISmatrix_sp_std > 0.25)
haw_pa_0.25 <- decostand (haw_ISmatrix_sp_0.25, method = "pa")

jap_ISmatrix_sp_0.25 <- jap_ISmatrix_sp_std * (jap_ISmatrix_sp_std > 0.25)
jap_pa_0.25 <- decostand (jap_ISmatrix_sp_0.25, method = "pa")

#% specialization 
prop_spe_vir <- apply(pa_vir_sp, 2, function(x) sum(x != 0))/nrow(pa_vir_sp)
mean(prop_spe_vir) ; sd(prop_spe_vir)
prop_spe_vir_0.25 <- apply(vir_pa_0.25, 2, function(x) sum(x != 0))/nrow(vir_pa_0.25)
mean(prop_spe_vir_0.25) ; sd(prop_spe_vir_0.25)
prop_spe_vir_0.5 <- apply(vir_pa_0.5, 2, function(x) sum(x != 0))/nrow(vir_pa_0.5)
mean(prop_spe_vir_0.5) ; sd(prop_spe_vir_0.5)

prop_spe_nca <- apply(pa_nca_sp, 2, function(x) sum(x != 0))/nrow(pa_nca_sp)
mean(prop_spe_nca) ; sd(prop_spe_nca)
prop_spe_nca_0.25 <- apply(nca_pa_0.25, 2, function(x) sum(x != 0))/nrow(nca_pa_0.25)
mean(prop_spe_nca_0.25) ; sd(prop_spe_nca_0.25)
prop_spe_nca_0.5 <- apply(nca_pa_0.5, 2, function(x) sum(x != 0))/nrow(nca_pa_0.5)
mean(prop_spe_nca_0.5) ; sd(prop_spe_nca_0.5)

prop_spe_mari <- apply(pa_mari_sp, 2, function(x) sum(x != 0))/nrow(pa_mari_sp)
mean(prop_spe_mari) ; sd(prop_spe_mari)
prop_spe_mari_0.25 <- apply(mari_pa_0.25, 2, function(x) sum(x != 0))/nrow(mari_pa_0.25)
mean(prop_spe_mari_0.25) ; sd(prop_spe_mari_0.25)
prop_spe_mari_0.5 <- apply(mari_pa_0.5, 2, function(x) sum(x != 0))/nrow(mari_pa_0.5)
mean(prop_spe_mari_0.5) ; sd(prop_spe_mari_0.5)

prop_spe_mad <- apply(pa_mad_sp, 2, function(x) sum(x != 0))/nrow(pa_mad_sp)
mean(prop_spe_mad) ; sd(prop_spe_mad)
prop_spe_mad_0.25 <- apply(mad_pa_0.25, 2, function(x) sum(x != 0))/nrow(mad_pa_0.25)
mean(prop_spe_mad_0.25) ; sd(prop_spe_mad_0.25)
prop_spe_mad_0.5 <- apply(mad_pa_0.5, 2, function(x) sum(x != 0))/nrow(mad_pa_0.5)
mean(prop_spe_mad_0.5) ; sd(prop_spe_mad_0.5)

prop_spe_haw <- apply(pa_haw_sp, 2, function(x) sum(x != 0))/nrow(pa_haw_sp)
mean(prop_spe_haw) ; sd(prop_spe_haw)
prop_spe_haw_0.25 <- apply(haw_pa_0.25, 2, function(x) sum(x != 0))/nrow(haw_pa_0.25)
mean(prop_spe_haw_0.25) ; sd(prop_spe_haw_0.25)
prop_spe_haw_0.5 <- apply(haw_pa_0.5, 2, function(x) sum(x != 0))/nrow(haw_pa_0.5)
mean(prop_spe_haw_0.5) ; sd(prop_spe_haw_0.5)

prop_spe_jap <- apply(pa_jap_sp, 2, function(x) sum(x != 0))/nrow(pa_jap_sp)
mean(prop_spe_jap) ; sd(prop_spe_jap)
prop_spe_jap_0.25 <- apply(jap_pa_0.25, 2, function(x) sum(x != 0))/nrow(jap_pa_0.25)
mean(prop_spe_jap_0.25) ; sd(prop_spe_jap_0.25)
prop_spe_jap_0.5 <- apply(jap_pa_0.5, 2, function(x) sum(x != 0))/nrow(jap_pa_0.5)
mean(prop_spe_jap_0.5) ; sd(prop_spe_jap_0.5)

#table

a_length <- c(length(prop_spe_vir), length(prop_spe_vir_0.25), length(prop_spe_vir_0.5),
         length(prop_spe_nca), length(prop_spe_nca_0.25), length(prop_spe_nca_0.5),
         length(prop_spe_mari), length(prop_spe_mari_0.25), length(prop_spe_mari_0.5),
         length(prop_spe_mad), length(prop_spe_mad_0.25), length(prop_spe_mad_0.5),
         length(prop_spe_haw), length(prop_spe_haw_0.25), length(prop_spe_haw_0.5),
         length(prop_spe_jap), length(prop_spe_jap_0.25), length(prop_spe_jap_0.5))


prop_spe_dat <- c(mean(prop_spe_vir), mean(prop_spe_vir_0.25), mean(prop_spe_vir_0.5),
                  mean(prop_spe_nca), mean(prop_spe_nca_0.25), mean(prop_spe_nca_0.5),
                  mean(prop_spe_mari), mean(prop_spe_mari_0.25), mean(prop_spe_mari_0.5),
                  mean(prop_spe_mad), mean(prop_spe_mad_0.25), mean(prop_spe_mad_0.5),
                  mean(prop_spe_haw), mean(prop_spe_haw_0.25), mean(prop_spe_haw_0.5),
                  mean(prop_spe_jap), mean(prop_spe_jap_0.25), mean(prop_spe_jap_0.5)) %>%
  as.data.frame() %>%
  rename(., mean = ".") %>%
  add_column(site = rep(c("vir","nca","mari","mad","haw","jap"), each = 3)) %>%
  add_column(thresh = rep(c("0","0.25","0.5"), times = 6)) %>%
  add_column(c(std.error(prop_spe_vir), std.error(prop_spe_vir_0.25), std.error(prop_spe_vir_0.5),
               std.error(prop_spe_nca), std.error(prop_spe_nca_0.25), std.error(prop_spe_nca_0.5),
               std.error(prop_spe_mari), std.error(prop_spe_mari_0.25), std.error(prop_spe_mari_0.5),
               std.error(prop_spe_mad), std.error(prop_spe_mad_0.25), std.error(prop_spe_mad_0.5),
               std.error(prop_spe_haw), std.error(prop_spe_haw_0.25), std.error(prop_spe_haw_0.5),
               std.error(prop_spe_jap), std.error(prop_spe_jap_0.25), std.error(prop_spe_jap_0.5))) %>%
 rename(., se = "c(...)" )


#single value 
prop_spe <- c(unname(prop_spe_vir), unname(prop_spe_nca), unname(prop_spe_mari), unname(prop_spe_mad), unname(prop_spe_haw), unname(prop_spe_jap)) #841
prop_spe_0.25 <- c(unname(prop_spe_vir_0.25), unname(prop_spe_nca_0.25), unname(prop_spe_mari_0.25), unname(prop_spe_mad_0.25), unname(prop_spe_haw_0.25), unname(prop_spe_jap_0.25)) #841
prop_spe_0.5 <- c(unname(prop_spe_vir_0.5), unname(prop_spe_nca_0.5), unname(prop_spe_mari_0.5), unname(prop_spe_mad_0.5), unname(prop_spe_haw_0.5), unname(prop_spe_jap_0.5)) #841
 
prop_spe_dat2 <- c(mean(prop_spe), mean(prop_spe_0.25), mean(prop_spe_0.5)) %>%
  as.data.frame() %>%
  rename(., mean = ".") %>%
  add_column(thresh = rep(c("0","0.25","0.5"), each = 1)) %>%
  add_column(c(std.error(prop_spe), std.error(prop_spe_0.25), std.error(prop_spe_0.5)))


boxplot(prop_spe_dat$mean[prop_spe_dat$thresh == "0"] ~ prop_spe_dat$site[prop_spe_dat$thresh == "0"])
