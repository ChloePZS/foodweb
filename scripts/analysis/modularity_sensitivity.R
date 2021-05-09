###################################
#Modularity - Sensitivity analysis#
###################################


library(tidyverse)
library(bipartite)
library(ggpubr)
library(ggplot2)

      #1. Observed modularity for the three prey categories ###

#1. Import  final matrices --> to change according to prey groupings (e.g. grp6, grp1, item_class)
vir_ISmatrix_sp_std <- read.csv("data/vir_ISmatrix_sp_std_item_class.csv", sep=",", row.names = 1) %>%
  as.matrix(.)
mari_ISmatrix_sp_std <- read.csv("data/mari_ISmatrix_sp_std_item_class.csv", sep=",", row.names = 1) %>%
  as.matrix(.)
nca_ISmatrix_sp_std<- read.csv("data/nca_ISmatrix_sp_std_item_class.csv", sep=",", row.names = 1) %>%
  as.matrix(.)
haw_ISmatrix_sp_std <- read.csv("data/haw_ISmatrix_sp_std_item_class.csv", sep=",", row.names = 1) %>%
  as.matrix(.)
mad_ISmatrix_sp_std <- read.csv("data/mad_ISmatrix_sp_std_item_class.csv", sep=",", row.names = 1) %>%
  as.matrix(.)
jap_ISmatrix_sp_std <- read.csv("data/jap_ISmatrix_sp_std_item_class.csv", sep=",", row.names = 1) %>%
  as.matrix(.)

#2. Transform into qualitative matrices
pa_vir_sp <- decostand(vir_ISmatrix_sp_std, method="pa")
pa_mari_sp <- decostand(mari_ISmatrix_sp_std, method="pa")
pa_nca_sp <- decostand(nca_ISmatrix_sp_std, method="pa")
pa_haw_sp <- decostand(haw_ISmatrix_sp_std, method="pa")
pa_mad_sp <- decostand(mad_ISmatrix_sp_std, method="pa")
pa_jap_sp <- decostand(jap_ISmatrix_sp_std, method="pa")


#3. Modularity measure ####
#Qualitative webs
#Reptiition to have a mean and SD for modularity - Example with West Indies and grp1

# mod_vir_sp_grp1 <- parallel::mclapply(1:100, function(x) computeModules(pa_vir_sp), mc.cores = 40)
# mod_vir_grp1 <- sapply(mod_vir_sp_grp1, function(x) x@likelihood)
# mean(mod_vir_grp1); sd(mod_vir_grp1)
# 
# nb_mod_vir <- sapply(mod_vir_sp_grp1, function(x) dim(x@modules)[1]-1) 
# 
# #Hawai
# mod_haw_sp_grp1 <- parallel::mclapply(1:100, function(x) computeModules(pa_haw_sp),mc.cores = 40)
# mod_haw_grp1 <- sapply(mod_haw_sp_grp1, function(x) x@likelihood)
# mean(mod_haw_grp1); sd(mod_haw_grp1)
# 
# nb_mod_haw <- sapply(mod_haw_sp_grp1, function(x) dim(x@modules)[1]-1) 
# mean(nb_mod_haw); sd(nb_mod_haw)


#Run algorithme once
obs_mod_vir_grp1 <- computeModules(pa_vir_sp)
obs_mod_mari_grp1 <- computeModules(pa_mari_sp)
obs_mod_mad_grp1 <- computeModules(pa_mad_sp)
obs_mod_jap_grp1 <- computeModules(pa_jap_sp)
obs_mod_nca_grp1 <- computeModules(pa_nca_sp)
obs_mod_haw_grp1 <- computeModules(pa_haw_sp)

obs_mod_grp1 <- c(obs_mod_mad_grp1@likelihood, obs_mod_nca_grp1@likelihood, obs_mod_mari_grp1@likelihood, obs_mod_haw_grp1@likelihood, obs_mod_vir_grp1@likelihood,obs_mod_jap_grp1@likelihood) %>%
  as.data.frame() %>%
  dplyr::rename(., mod =".") %>%
  add_column(site = c("mad","nca","mari","haw","vir","jap")) %>%
  add_column(obs = "grp1")

#Prey class grouping
#single algorithme
obs_mod_vir_class <- computeModules(pa_vir_sp)
obs_mod_mari_class <- computeModules(pa_mari_sp)
obs_mod_mad_class <- computeModules(pa_mad_sp)
obs_mod_jap_class <- computeModules(pa_jap_sp)
obs_mod_nca_class <- computeModules(pa_nca_sp)
obs_mod_haw_class <- computeModules(pa_haw_sp)

obs_mod_class <- c(obs_mod_mad_class@likelihood, obs_mod_nca_class@likelihood, obs_mod_mari_class@likelihood, obs_mod_haw_class@likelihood, obs_mod_vir_class@likelihood,obs_mod_jap_class@likelihood) %>%
  as.data.frame() %>%
  dplyr::rename(., mod =".") %>%
  add_column(site = c("mad","nca","mari","haw","vir","jap")) %>%
  add_column(obs = "class")

#Grp6 prey category
obs_mod_vir_grp6 <- computeModules(pa_vir_sp)
obs_mod_mari_grp6 <- computeModules(pa_mari_sp)
obs_mod_mad_grp6 <- computeModules(pa_mad_sp)
obs_mod_jap_grp6 <- computeModules(pa_jap_sp)
obs_mod_nca_grp6 <- computeModules(pa_nca_sp)
obs_mod_haw_grp6 <- computeModules(pa_haw_sp)

obs_mod_grp6 <- c(obs_mod_mad_grp6@likelihood, obs_mod_nca_grp6@likelihood, obs_mod_mari_grp6@likelihood, obs_mod_haw_grp6@likelihood, obs_mod_vir_grp6@likelihood,obs_mod_jap_grp6@likelihood) %>%
  as.data.frame() %>%
  dplyr::rename(., mod =".") %>%
  add_column(site = c("mad","nca","mari","haw","vir","jap")) %>%
  add_column(obs = "grp6")


obs_mod_grp <- rbind(obs_mod_grp6, obs_mod_grp1, obs_mod_class) %>%
  mutate_if(is.character, as.factor) %>%
  add_column(dat = "Observed values")


site.lab <- c("Hawaii","Madagascar","Marshall Islands","New Caledonia","Virgin Islands","Okinawa")
names(site.lab) <-c("haw","mad","mari","nca","vir","jap")



    #2. Modularity of random matrices 
#Using curveball algortithme (Giovanni Strona et al., )
#Null model 
curve_ball<-function(m){
  RC=dim(m)
  R=RC[1]
  C=RC[2]
  hp=list()
  for (row in 1:dim(m)[1]) {hp[[row]]=(which(m[row,]==1))}
  l_hp=length(hp)
  for (rep in 1:(5*l_hp)){
    AB=sample(1:l_hp,2)
    a=hp[[AB[1]]]
    b=hp[[AB[2]]]
    ab=intersect(a,b)
    l_ab=length(ab)
    l_a=length(a)
    l_b=length(b)
    if ((l_ab %in% c(l_a,l_b))==F){
      tot=setdiff(c(a,b),ab)
      l_tot=length(tot)
      tot=sample(tot, l_tot, replace = FALSE, prob = NULL)
      L=l_a-l_ab
      hp[[AB[1]]] = c(ab,tot[1:L])
      hp[[AB[2]]] = c(ab,tot[(L+1):l_tot])}
    
  }
  rm=matrix(0,R,C)
  for (row in 1:R){rm[row,hp[[row]]]=1}
  rm
}

#1. Creating random food webs - Careful to check which matrices had been uploaded (i.e grp1, grp6 or classes) !!
p <- 100
rand_vir_sp3 <- lapply(1:p, function(x) curve_ball(pa_vir_sp))
rand_mari_sp3 <- lapply(1:p, function(x) curve_ball(pa_mari_sp))
rand_nca_sp3 <- lapply(1:p, function(x) curve_ball(pa_nca_sp))
rand_mad_sp3 <- lapply(1:p, function(x) curve_ball(pa_mad_sp))
rand_haw_sp3 <- lapply(1:p, function(x) curve_ball(pa_haw_sp))
rand_jap_sp3 <- lapply(1:p, function(x) curve_ball(pa_jap_sp))

#Null modularity grp6
mod_mari_sp_null3_grp6 <- unlist(sapply(rand_mari_sp3, computeModules))
mod_mad_sp_null3_grp6 <- unlist(sapply(rand_mad_sp3, computeModules))
mod_haw_sp_null3_grp6 <- unlist(sapply(rand_haw_sp3, computeModules))
mod_nca_sp_null3_grp6 <- unlist(sapply(rand_nca_sp3, computeModules))
mod_vir_sp_null3_grp6 <- unlist(sapply(rand_vir_sp3, computeModules))
mod_jap_sp_null3_grp6 <- unlist(sapply(rand_jap_sp3, computeModules))


mod_null3_grp6 <- c(sapply(mod_mad_sp_null3_grp6, function(x) x@likelihood),
                    sapply(mod_nca_sp_null3_grp6, function(x) x@likelihood),
                    sapply(mod_mari_sp_null3_grp6, function(x) x@likelihood),
                    sapply(mod_haw_sp_null3_grp6, function(x) x@likelihood),
                    sapply(mod_vir_sp_null3_grp6, function(x) x@likelihood),
                    sapply(mod_jap_sp_null3_grp6, function(x) x@likelihood)) %>%
  as.data.frame() %>%
  rename(., mod =".") %>%
  add_column(site = rep(c("mad","nca","mari","haw","vir","jap"), each=100)) %>%
  add_column(null = "curveball") %>%
  add_column(obs = "grp6")

#Null modularity grp1
mod_mari_sp_null3_grp1 <- unlist(sapply(rand_mari_sp3, computeModules))
mod_mad_sp_null3_grp1 <- unlist(sapply(rand_mad_sp3, computeModules))
mod_haw_sp_null3_grp1 <- unlist(sapply(rand_haw_sp3, computeModules))
mod_nca_sp_null3_grp1 <- unlist(sapply(rand_nca_sp3, computeModules))
mod_vir_sp_null3_grp1 <- unlist(sapply(rand_vir_sp3, computeModules)) 
mod_jap_sp_null3_grp1 <- unlist(sapply(rand_jap_sp3, computeModules))


mod_null3_grp1 <- c(sapply(mod_mad_sp_null3_grp1, function(x) x@likelihood),
               sapply(mod_nca_sp_null3_grp1, function(x) x@likelihood),
               sapply(mod_mari_sp_null3_grp1, function(x) x@likelihood),
               sapply(mod_haw_sp_null3_grp1, function(x) x@likelihood),
               sapply(mod_vir_sp_null3_grp1, function(x) x@likelihood),
               sapply(mod_jap_sp_null3_grp1, function(x) x@likelihood)) %>%
  as.data.frame() %>%
  rename(., mod =".") %>%
  add_column(site = rep(c("mad","nca","mari","haw","vir","jap"), each=100)) %>%
  add_column(null = "curveball") %>%
  add_column(obs = "grp1")


#Null modularity item_class
mod_mari_sp_null3_class <- unlist(sapply(rand_mari_sp3, computeModules))
mod_mad_sp_null3_class <- unlist(sapply(rand_mad_sp3, computeModules))
mod_haw_sp_null3_class <- unlist(sapply(rand_haw_sp3, computeModules))
mod_nca_sp_null3_class <- unlist(sapply(rand_nca_sp3, computeModules))
mod_vir_sp_null3_class <- unlist(sapply(rand_vir_sp3, computeModules))
mod_jap_sp_null3_class <- unlist(sapply(rand_jap_sp3, computeModules))


mod_null3_class <- c(sapply(mod_mad_sp_null3_class, function(x) x@likelihood),
                    sapply(mod_nca_sp_null3_class, function(x) x@likelihood),
                    sapply(mod_mari_sp_null3_class, function(x) x@likelihood),
                    sapply(mod_haw_sp_null3_class, function(x) x@likelihood),
                    sapply(mod_vir_sp_null3_class, function(x) x@likelihood),
                    sapply(mod_jap_sp_null3_class, function(x) x@likelihood)) %>%
  as.data.frame() %>%
  rename(., mod =".") %>%
  add_column(site = rep(c("mad","nca","mari","haw","vir","jap"), each=100)) %>%
  add_column(null = "curveball") %>%
  add_column(obs = "class")


#2. Preparing the DF with mod for all 3 groupings
mod_null3_grp <- rbind(mod_null3_grp6, mod_null3_grp1, mod_null3_class) %>%
  add_column(dat = "Null distributions")

#Combine DF
mod_full <- plyr::rbind.fill(obs_mod_grp, mod_null3_grp)

obs_mod_mean$site <- factor(obs_mod_mean$site, levels = c("haw","mad","mari","nca","jap","vir"))
mod_null3_grp$site <- factor(mod_null3_grp$site, levels = c("haw","mad","mari","nca","jap","vir"))
mod_full$site <- factor(mod_full$site, levels = c("haw","mad","mari","nca","jap","vir"))

  
#3. Plot
#preparing the plot
quantiles_95 <- function(x) {
  r <- c(mean(x), quantile(x, probs=c(0.05)),quantile(x, probs=c(0.95)))
  names(r) <- c("y","ymin","ymax")
  r
}

obs_lab <- c(class = "Prey class",
         grp6 = "Main grouping",
         grp1 = "Alternative grouping")

#plot
mod_plot2 <- ggplot(data=mod_null3_grp, aes(x=factor(site, levels = c("haw","mad","mari","nca","jap","vir")), y=mod, fill=site)) +
  geom_point(data=obs_mod_grp, size=2.5, aes(shape="Observed values", color ="Observed values"))+
  stat_summary(fun.data = quantiles_95, geom ="errorbar", width=0.08) +
  stat_summary(fun.y = mean, geom="point",size=2.5, aes(shape = "Null distributions", color = "Null distributions" )) + 
  scale_shape_manual(name = NULL, values = c(19,19))+
  scale_color_manual(name = NULL, values = c("grey60","black"))+
  scale_x_discrete(breaks=c("haw","mad","mari","nca","vir","jap"),
                   labels=c("Hawaii", "Madagascar", "Marshall Islands","New Caledonia","West Indies","Okinawa"))+
  facet_grid(obs ~ ., scales="free", switch ="y", labeller = labeller(obs = obs_lab))+
  labs(y = "Modularity\n", x="") + 
  theme_test() +
  theme(strip.text.y = element_text(size=11),
        legend.text = element_text(colour="black", size=10),
        axis.line.x = element_line(colour = "black",size=0.5, lineend = "butt"),
        axis.line.y = element_line(colour = "black", size=0.5),
        axis.title = element_text(size=13.5),
        axis.text = element_text(size=12, colour = "black")) +
  guides(fill=FALSE) +
  theme(legend.position = c(0.1,0.95),
        legend.box = NULL)

mod_plot2


#save plot
ggsave("figures/modularity_supp.png", plot = mod_plot2, units="in", width=10, height=11, dpi=300)



    #2. Interaction strength threshold --> need quantitative matrices ####
#Quantitative webs (spw --> w for weighted) with grp6 
mod_vir_spw <- lapply(1:30, function(x) computeModules(vir_ISmatrix_sp_std))
mod_vir_spww <- sapply(mod_vir_spw, function(x) x@likelihood)
nb_mod_vir <- sapply(mod_vir_spw, function(x) dim(x@modules)[1]-1) 

mod_haw_spw <- lapply(1:30, function(x) computeModules(haw_ISmatrix_sp_std))
mod_haw_spww <- sapply(mod_haw_spw, function(x) x@likelihood)
nb_mod_haw <- sapply(mod_haw_spw, function(x) dim(x@modules)[1]-1)

mod_mari_spw <- lapply(1:30, function(x) computeModules(mari_ISmatrix_sp_std))
mod_mari_spww <- sapply(mod_mari_spw, function(x) x@likelihood)
nb_mod_mari <- sapply(mod_mari_spw, function(x) dim(x@modules)[1]-1)

mod_nca_spw <- lapply(1:30, function(x) computeModules(nca_ISmatrix_sp_std))
mod_nca_spww <- sapply(mod_nca_spw, function(x) x@likelihood)
nb_mod_nca <- sapply(mod_nca_spw, function(x) dim(x@modules)[1]-1)  

mod_mad_spw <- lapply(1:30, function(x) computeModules(mad_ISmatrix_sp_std))
mod_mad_spww <- sapply(mod_mad_spw, function(x) x@likelihood)
nb_mod_mad <- sapply(mod_mad_spw, function(x) dim(x@modules)[1]-1) 

obs_mod_w <- c(mod_mad_spww, mod_nca_spww, mod_mari_spww, mod_haw_spww, mod_vir_spww) %>%
  as.data.frame() %>%
  rename(., mod =".") %>%
  add_column(site = rep(c("mad","nca","mari","haw","vir"), each = 30)) %>%
  add_column(obs = "0")

nb_mod_w<- c(nb_mod_mad, nb_mod_nca, nb_mod_mari, nb_mod_haw, nb_mod_vir) %>%
  as.data.frame() %>%
  rename(., mod =".") %>%
  add_column(site = rep(c("mad","nca","mari","haw","vir"), each = 30)) %>%
  add_column(obs = "0")

#Threshold --> keep only links with weights superior to 0.5
vir_ISmatrix_sp_0.5 <- vir_ISmatrix_sp_std * (vir_ISmatrix_sp_std > 0.5) #128 sp & 9 preys
nca_ISmatrix_sp_0.5 <- nca_ISmatrix_sp_std * (nca_ISmatrix_sp_std > 0.5) #122 & 9
mari_ISmatrix_sp_0.5 <- mari_ISmatrix_sp_std * (mari_ISmatrix_sp_std > 0.5) #70 & 7
mad_ISmatrix_sp_0.5 <- mad_ISmatrix_sp_std * (mad_ISmatrix_sp_std > 0.5)# 31 & 12 
haw_ISmatrix_sp_0.5 <- haw_ISmatrix_sp_std * (haw_ISmatrix_sp_std > 0.5)# 19 & 7 sp

#Recalculate modularity
mod_vir_spw0.5 <- lapply(1:30, function(x) computeModules(vir_ISmatrix_sp_0.5))
mod_vir_spww0.5 <- sapply(mod_vir_spw0.5, function(x) x@likelihood)
nb_mod_vir0.5 <- sapply(mod_vir_spw0.5, function(x) dim(x@modules)[1]-1) 

mod_haw_spw0.5 <- lapply(1:30, function(x) computeModules(haw_ISmatrix_sp_0.5))
mod_haw_spww0.5 <- sapply(mod_haw_spw0.5, function(x) x@likelihood)
nb_mod_haw0.5 <- sapply(mod_haw_spw0.5, function(x) dim(x@modules)[1]-1)

mod_mari_spw0.5 <- lapply(1:30, function(x) computeModules(mari_ISmatrix_sp_0.5))
mod_mari_spww0.5 <- sapply(mod_mari_spw0.5, function(x) x@likelihood)
nb_mod_mari0.5 <- sapply(mod_mari_spw0.5, function(x) dim(x@modules)[1]-1)

mod_nca_spw0.5 <- lapply(1:30, function(x) computeModules(nca_ISmatrix_sp_0.5))
mod_nca_spww0.5 <- sapply(mod_nca_spw0.5, function(x) x@likelihood)
nb_mod_nca0.5 <- sapply(mod_nca_spw0.5, function(x) dim(x@modules)[1]-1)  

mod_mad_spw0.5 <- lapply(1:30, function(x) computeModules(mad_ISmatrix_sp_0.5))
mod_mad_spww0.5 <- sapply(mod_mad_spw0.5, function(x) x@likelihood)
nb_mod_mad0.5 <- sapply(mod_mad_spw0.5, function(x) dim(x@modules)[1]-1) 

obs_mod_w0.5 <- c(mod_mad_spww0.5, mod_nca_spww0.5, mod_mari_spww0.5, mod_haw_spww0.5, mod_vir_spww0.5) %>%
  as.data.frame() %>%
  rename(., mod =".") %>%
  add_column(site = rep(c("mad","nca","mari","haw","vir"), each = 30)) %>%
  add_column(obs = "0.5")

nb_mod_w0.5 <- c(nb_mod_mad0.5, nb_mod_nca0.5, nb_mod_mari0.5, nb_mod_haw0.5, nb_mod_vir0.5) %>%
  as.data.frame() %>%
  rename(., mod =".") %>%
  add_column(site = rep(c("mad","nca","mari","haw","vir"), each = 30)) %>%
  add_column(obs = "0.5")

#Threshold --> keep only links with weights superior to 0.25
vir_ISmatrix_sp_0.25 <- vir_ISmatrix_sp_std * (vir_ISmatrix_sp_std > 0.25)
nca_ISmatrix_sp_0.25 <- nca_ISmatrix_sp_std * (nca_ISmatrix_sp_std > 0.25)
mari_ISmatrix_sp_0.25 <- mari_ISmatrix_sp_std * (mari_ISmatrix_sp_std > 0.25)
mad_ISmatrix_sp_0.25 <- mad_ISmatrix_sp_std * (mad_ISmatrix_sp_std > 0.25)
haw_ISmatrix_sp_0.25 <- haw_ISmatrix_sp_std * (haw_ISmatrix_sp_std > 0.25)

#Recalculate modularity
mod_vir_spw0.25 <- lapply(1:30, function(x) computeModules(vir_ISmatrix_sp_0.25))
mod_vir_spww0.25 <- sapply(mod_vir_spw0.25, function(x) x@likelihood)
nb_mod_vir0.25 <- sapply(mod_vir_spw0.25, function(x) dim(x@modules)[1]-1) 

mod_haw_spw0.25 <- lapply(1:30, function(x) computeModules(haw_ISmatrix_sp_0.25))
mod_haw_spww0.25 <- sapply(mod_haw_spw0.25, function(x) x@likelihood)
nb_mod_haw0.25 <- sapply(mod_haw_spw0.25, function(x) dim(x@modules)[1]-1)

mod_mari_spw0.25 <- lapply(1:30, function(x) computeModules(mari_ISmatrix_sp_0.25))
mod_mari_spww0.25 <- sapply(mod_mari_spw0.25, function(x) x@likelihood)
nb_mod_mari0.25 <- sapply(mod_mari_spw0.25, function(x) dim(x@modules)[1]-1)

mod_nca_spw0.25 <- lapply(1:30, function(x) computeModules(nca_ISmatrix_sp_0.25))
mod_nca_spww0.25 <- sapply(mod_nca_spw0.25, function(x) x@likelihood)
nb_mod_nca0.25 <- sapply(mod_nca_spw0.25, function(x) dim(x@modules)[1]-1)  

mod_mad_spw0.25 <- lapply(1:30, function(x) computeModules(mad_ISmatrix_sp_0.25))
mod_mad_spww0.25 <- sapply(mod_mad_spw0.25, function(x) x@likelihood)
nb_mod_mad0.25 <- sapply(mod_mad_spw0.25, function(x) dim(x@modules)[1]-1) 


obs_mod_w0.25 <- c(mod_mad_spww0.25, mod_nca_spww0.25, mod_mari_spww0.25, mod_haw_spww0.25, mod_vir_spww0.25) %>%
  as.data.frame() %>%
  rename(., mod =".") %>%
  add_column(site = rep(c("mad","nca","mari","haw","vir"), each = 30)) %>%
  add_column(obs = "0.25")

nb_mod_w0.25 <- c(nb_mod_mad0.25, nb_mod_nca0.25, nb_mod_mari0.25, nb_mod_haw0.25, nb_mod_vir0.25) %>%
  as.data.frame() %>%
  rename(., mod =".") %>%
  add_column(site = rep(c("mad","nca","mari","haw","vir"), each = 30)) %>%
  add_column(obs = "0.25")

#DF with all mod
obs_mod_ww <- rbind(obs_mod_w, obs_mod_w0.5, obs_mod_w0.25) %>%
  mutate_if(is.character, as.factor)

nb_mod_ww <- rbind(nb_mod_w, nb_mod_w0.5, nb_mod_w0.25) %>%
  mutate_if(is.character, as.factor)

#Plot
ggplot(data = obs_mod_ww, aes(x = obs, y = mod))+
  #stat_summary(fun.data = quantiles_95, geom="boxplot", color="black")  +
  geom_boxplot() +
  #stat_summary(fun.data = mean_cl_boot, size=0.7, width = 0.1, linetype="solid", geom="errorbar", color="red")  +
  facet_wrap(. ~ site, labeller = labeller(site = site.labs))  +
  labs (y = "Modularity\n", x = "\nInteraction strength threshold") +
  theme_test() +
  theme(strip.text.y = element_text(size=11),
        axis.line.x = element_line(colour = "black",size=0.5, lineend = "butt"),
        axis.line.y = element_line(colour = "black", size=0.5),
        axis.title = element_text(size=13.5),
        axis.text = element_text(size=12, colour = "black")) 

lapply(split(obs_mod_ww, obs_mod_ww$site), function(d) { kruskal.test(mod ~ obs, data=d) }) #Diff between interaction threshold
#Increasing modularity with increasing interaction strength

#Plot for nb modules
ggplot(data = , aes(x = obs, y = mod))+
  #stat_summary(fun.data = quantiles_95, geom="boxplot", color="black")  +
  geom_boxplot() +
  #stat_summary(fun.data = mean_cl_boot, size=0.7, width = 0.1, linetype="solid", geom="errorbar", color="red")  +
  facet_wrap(. ~ site, labeller = labeller(site = site.labs))  +
  labs (y = "Modularity\n", x = "\nInteraction strength threshold") +
  theme_test() +
  theme(strip.text.y = element_text(size=11),
        axis.line.x = element_line(colour = "black",size=0.5, lineend = "butt"),
        axis.line.y = element_line(colour = "black", size=0.5),
        axis.title = element_text(size=13.5),
        axis.text = element_text(size=12, colour = "black")) 


ggarrange(plotlist = c(plotModuleWeb(mod_vir_spw[[1]]),
          plotModuleWeb(mod_vir_spw0.25[[1]]),
          plotModuleWeb(mod_vir_spw0.5[[1]])), nrow =2, ncol =2)

