#################################
#Null Model Vale - Species level# Networks matrix
#################################

library(tidyverse)
library(bipartite)
library(vegan)
library(picante)
library(RColorBrewer)
library(wesanderson)


  #1.  Import matrix

vir_ISmatrix_sp2 <- read.csv("data/vir_ISmatrix_sp2.csv", sep=",", row.names = 1) %>%
  as.matrix(.)
mari_ISmatrix_sp2 <- read.csv("data/mari_ISmatrix_sp2.csv", sep=",", row.names = 1) %>%
  as.matrix(.)
nca_ISmatrix_sp2 <- read.csv("data/nca_ISmatrix_sp2.csv", sep=",", row.names = 1) %>%
  as.matrix(.)
haw_ISmatrix_sp2 <- read.csv("data/haw_ISmatrix_sp2.csv", sep=",", row.names = 1) %>%
  as.matrix(.)
mad_ISmatrix_sp2 <- read.csv("data/mad_ISmatrix_sp2.csv", sep=",", row.names = 1) %>%
  as.matrix(.)


  #2.   Null model
chloe_null.model <- function(x, perm) {
  
  column.index <- 1:dim(x)[2]
  
  lapply(1:perm, function (y) {
    
    a <- x[,sample(column.index, size= dim(x)[2],  replace = T)]
    colnames (a) <- colnames(x)
    
    a
    
  }) 
  
}


chloe_null_vir_sp <- chloe_null.model(vir_ISmatrix_sp2, perm= 100)  
chloe_null_mari_sp <- chloe_null.model(mari_ISmatrix_sp2, perm= 100)  
chloe_null_nca_sp <- chloe_null.model(nca_ISmatrix_sp2, perm= 100)  
chloe_null_mad_sp <- chloe_null.model(mad_ISmatrix_sp2, perm= 100)  
chloe_null_haw_sp <- chloe_null.model(haw_ISmatrix_sp2, perm= 100)  
  

####2. Basic indices --> Connectance + WNODF####

obs_vir_sp <- unlist(networklevel(vir_ISmatrix_sp2, empty.web = TRUE, index=c("number of species","connectance","weighted connectance","weighted NODF",
                                                                              "linkage density","generality","vulnerability",
                                                                              "links per species","mean number of shared partners")))
obs_vir_sp <-  data.frame(t(obs_vir_sp)) %>%
  mutate(site = "vir",
         value = "obs") 

obs_mari_sp <- unlist(networklevel(mari_ISmatrix_sp2, empty.web = TRUE, index=c("number of species","connectance","weighted connectance","weighted NODF",
                                                                                "linkage density","generality","vulnerability",
                                                                                "links per species","mean number of shared partners")))
obs_mari_sp <-  data.frame(t(obs_mari_sp)) %>%
  mutate(site = "mari",
         value = "obs")

obs_nca_sp <- unlist(networklevel(nca_ISmatrix_sp2, empty.web = TRUE, index=c("number of species","connectance","weighted connectance","weighted NODF",
                                                                              "linkage density","generality","vulnerability",
                                                                              "links per species","mean number of shared partners")))
obs_nca_sp <-  data.frame(t(obs_nca_sp)) %>%
  mutate(site = "nca",
         value = "obs")


obs_mad_sp <- unlist(networklevel(mad_ISmatrix_sp2, empty.web = TRUE, index=c("number of species","connectance","weighted connectance","weighted NODF",
                                                                              "linkage density","generality","vulnerability",
                                                                              "links per species","mean number of shared partners")))
obs_mad_sp <-  data.frame(t(obs_mad_sp)) %>%
  mutate(site = "mad",
         value = "obs")

obs_haw_sp <- unlist(networklevel(haw_ISmatrix_sp2, empty.web = TRUE, index=c("number of species","connectance","weighted connectance","weighted NODF",
                                                                              "linkage density","generality","vulnerability",
                                                                              "links per species","mean number of shared partners")))
obs_haw_sp <-  data.frame(t(obs_haw_sp)) %>%
  mutate(site = "haw",
         value = "obs")

obs_full_sp <- plyr::rbind.fill(obs_mad_sp, obs_nca_sp, obs_mari_sp, obs_haw_sp, obs_vir_sp)


#Null models
net_chloe_vir_sp <- unlist(sapply(chloe_null_vir_sp, networklevel, weighted=TRUE, empty.web = TRUE, index=c("connectance","weighted connectance","weighted NODF", "generality","vulnerability", "linkage density")))
net_chloe_mari_sp <- unlist(sapply(chloe_null_mari_sp, networklevel, weighted=TRUE, empty.web = TRUE, index=c("connectance","weighted connectance","weighted NODF", "generality","vulnerability", "linkage density")))
net_chloe_nca_sp <- unlist(sapply(chloe_null_nca_sp, networklevel, weighted=TRUE, empty.web = TRUE, index=c("connectance","weighted connectance","weighted NODF", "generality","vulnerability", "linkage density")))
net_chloe_mad_sp <- unlist(sapply(chloe_null_mad_sp, networklevel, weighted=TRUE, empty.web = TRUE, index=c("connectance","weighted connectance","weighted NODF", "generality","vulnerability", "linkage density")))
net_chloe_haw_sp <- unlist(sapply(chloe_null_haw_sp, networklevel, weighted=TRUE, empty.web = TRUE, index=c("connectance","weighted connectance","weighted NODF", "generality","vulnerability", "linkage density")))


#Dataframe with random distributions
df_null_sp_haw <- t(net_chloe_haw_sp) %>% data.frame(.)
df_null_sp_haw$site <- "haw"
df_null_sp_haw$value <- "null"

df_null_sp_mari <- t(net_chloe_mari_sp) %>% data.frame(.) 
df_null_sp_mari$site <- "mari"
df_null_sp_mari$value <- "null"

df_null_sp_mad <- t(net_chloe_mad_sp) %>% data.frame(.) 
df_null_sp_mad$site <- "mad"
df_null_sp_mad$value <- "null"

df_null_sp_nca <- t(net_chloe_nca_sp) %>% data.frame(.) 
df_null_sp_nca$site <- "nca"
df_null_sp_nca$value <- "null"

df_null_sp_vir <- t(net_chloe_vir_sp) %>% data.frame(.) 
df_null_sp_vir$site <- "vir"
df_null_sp_vir$value <- "null"

chloe_null_sp_full <- plyr::rbind.fill(df_null_sp_mad, df_null_sp_nca, df_null_sp_mari, df_null_sp_haw, df_null_sp_vir) 
#Same as observed ! Make sense for the unweighted metrics but even for nestedness


####3. Modularity####
mod_vir_sp <- computeModules(vir_ISmatrix_sp2, method="Beckett", forceLPA = FALSE) #DIRT algorithm from Becket 2016
mod_mari_sp <- computeModules(mari_ISmatrix_sp2, method="Beckett", forceLPA = FALSE)
mod_mad_sp <- computeModules(mad_ISmatrix_sp2, method="Beckett", forceLPA = FALSE)
mod_nca_sp <- computeModules(nca_ISmatrix_sp2, method="Beckett", forceLPA = FALSE)
mod_haw_sp <- computeModules(haw_ISmatrix_sp2, method="Beckett", forceLPA = FALSE)

obs_full_sp$modularity <- c(mod_mad_sp@likelihood, mod_nca_sp@likelihood, mod_mari_sp@likelihood, mod_haw_sp@likelihood, mod_vir_sp@likelihood)

plotModuleWeb(mod_vir_sp)

#Null modularity
mod_mari_sp_chloe <- unlist(sapply(chloe_null_mari_sp, computeModules, method="Beckett",forceLPA = TRUE))
mod_mad_sp_chloe <- unlist(sapply(chloe_null_mad_sp, computeModules, method="Beckett", forceLPA = TRUE))
mod_haw_sp_chloe <- unlist(sapply(chloe_null_haw_sp, computeModules, method="Beckett", forceLPA = TRUE))
mod_nca_sp_chloe <- unlist(sapply(chloe_null_nca_sp, computeModules, method="Beckett", forceLPA = TRUE))
mod_vir_sp_chloe <- unlist(sapply(chloe_null_vir_sp, computeModules, method="Beckett", forceLPA = TRUE))

chloe_null_sp_full$modularity <- c(sapply(mod_mad_sp_chloe, function(x) x@likelihood),
                             sapply(mod_nca_sp_chloe, function(x) x@likelihood),
                             sapply(mod_mari_sp_chloe, function(x) x@likelihood),
                             sapply(mod_haw_sp_chloe, function(x) x@likelihood),
                             sapply(mod_vir_sp_chloe, function(x) x@likelihood))

identical(chloe_null_haw_sp[[2]], chloe_null_haw_sp[[98]])


####4. Robustness####
#To random extinction
ex_ran_vir_sp <- second.extinct(vir_ISmatrix_sp2, participant = "lower", method="random", nrep=100, details=FALSE)
robustness(ex_ran_vir_sp)
slope.bipartite(ex_ran_vir_sp)

ex_ran_mari_sp <- second.extinct(mari_ISmatrix_sp2, participant = "lower", method="random", nrep=100, details=FALSE)
robustness(ex_ran_mari_sp)
slope.bipartite(ex_ran_mari_sp)

ex_ran_haw_sp <- second.extinct(haw_ISmatrix_sp2, participant = "lower", method="random", nrep=100, details=FALSE)
robustness(ex_ran_haw_sp)
slope.bipartite(ex_ran_haw_sp)

ex_ran_mad_sp <- second.extinct(mad_ISmatrix_sp2, participant = "lower", method="random", nrep=100, details=FALSE)
robustness(ex_ran_mad_sp)
slope.bipartite(ex_ran_mad_sp)

ex_ran_nca_sp <- second.extinct(nca_ISmatrix_sp2, participant = "lower", method="random", nrep=100, details=FALSE)
robustness(ex_ran_nca_sp)
slope.bipartite(ex_ran_nca_sp)

cumsum(ex_ran_nca_sp[,3]) #nombre cumulé d'espèces éteintes  
cumsum(ex_ran_nca_sp[,3])/628 #proportions d'espèces éteintes ou 1-cumsum(ex_ran_nca_sp[,3])/628

628-cumsum(ex_ran_nca_sp[,3]) #nombre cumulé d'espèces encore en vie
(628-cumsum(ex_ran_nca_sp[,3]))/628 #proportions d'espèces en vie

#How the slope.bipartite curve create
plot(ex_ran_nca_sp[,1]/38, (38-cumsum(ex_ran_nca_sp[,2]))/38) #for lower level extinction, same curve than the slope.bipartite but WHY using the 2nd column and over 38 nodes ?!
plot(ex_ran_nca_sp[,1]/38, (628-cumsum(ex_ran_nca_sp[,3]))/628) #for lower level extinction, that should be the right curve


ex_ran_nca_sp <- second.extinct(nca_ISmatrix_sp2, participant = "higher", method="random", nrep=100, details=FALSE)
slope.bipartite(ex_ran_nca_sp)
plot(ex_ran_nca_sp[,1]/628, (38-cumsum(ex_ran_nca_sp[,2]))/38) #for higher level extinction, okay here uses the 2 columns coz wanna prop of lower extinct


obs_full_sp$robustness.ran <- c(robustness(ex_ran_mad_sp), robustness(ex_ran_nca_sp), robustness(ex_ran_mari_sp), robustness(ex_ran_haw_sp), robustness(ex_ran_vir_sp))

#Robustness null robels
rob_mari_sp_chloe <- lapply(chloe_null_mari_sp, second.extinct, participant = "lower", method="random", nrep=10, details=FALSE)
rob_mad_sp_chloe <- lapply(chloe_null_mad_sp, second.extinct, participant = "lower", method="random", nrep=10, details=FALSE)
rob_haw_sp_chloe <- lapply(chloe_null_haw_sp, second.extinct, participant = "lower", method="random", nrep=10, details=FALSE)
rob_nca_sp_chloe <- lapply(chloe_null_nca_sp, second.extinct, participant = "lower", method="random", nrep=10, details=FALSE)
rob_vir_sp_chloe <- lapply(chloe_null_vir_sp, second.extinct, participant = "lower", method="random", nrep=10, details=FALSE)

chloe_null_sp_full$robustness.ran <- c(sapply(rob_mad_sp_chloe, function(x) robustness(x)),
                                   sapply(rob_nca_sp_chloe, function(x) robustness(x)),
                                   sapply(rob_mari_sp_chloe, function(x) robustness(x)),
                                   sapply(rob_haw_sp_chloe, function(x) robustness(x)),
                                   sapply(rob_vir_sp_chloe, function(x) robustness(x)))
unique(chloe_null_sp_full$site)


unique(null_sp_full$site)

#Try to show curve for null distribution
rob_nca_sp_null[[3]] #only extinctions for higher level, so why not the case for the observed matrix
slope.bipartite(rob_nca_sp_null[[3]]) 
robustness(rob_nca_sp_null[[3]])
#Example plot 
plot(rob_nca_sp_null[[1]][,1]/38, (628 - cumsum(rob_nca_sp_null[[1]][,3]))/628, type="l", xlab = "Proportion of primary extinctions - Prey",
     ylab= "Proportions of secon Predator diversity", ylim=c(0,0.6)) #okay and here that's thats the third column used
lines(ex_ran_nca_sp[,1]/38, (628-cumsum(ex_ran_nca_sp[,3]))/628, type="l", col="red") #observed curve
lines(ex_ran_nca_sp[,1]/38, (38-cumsum(ex_ran_nca_sp[,2]))/38, type="l", col="blue") #observed curve rom slope.bipartite


#Tranform into long format
obs_full_sp_long <- gather(obs_full_sp, key = "metric",value ="metric_val", -site, -value)
null_sp_full_long <- gather (chloe_null_sp_full, key = "metric", value = "metric_val", -site, -value)


####5. Plots####
#Complexity
ggplot(data=subset(null_sp_full_long, metric %in% c("connectance", "weighted.connectance", "generality.HL","vulnerability.LL")), aes(x=site, y=metric_val, fill=site)) +
  stat_summary(fun.data = mean_IC, geom ="errorbar", size=0.4, color="red") +
  geom_boxplot(data=subset(null_sp_full_long, metric %in% c("connectance", "weighted.connectance", "generality.HL","vulnerability.LL")), outlier.shape = NA) +
  scale_fill_manual(values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  geom_point(data= subset(obs_full_sp_long, metric %in% c("connectance", "weighted.connectance", "generality.HL","vulnerability.LL")), size=2, shape=8)+
  facet_grid(metric ~ ., scales="free", switch ="y")+
  ggtitle("Complexity and Asymmetry_sp") +
  labs( y = "Value", x="Sites") + 
  theme(legend.position = "none") 


#Structure
ggplot(data=subset(null_sp_full_long, metric %in% c("modularity", "weighted.NODF","robustness.ran")), aes(x=site, y=metric_val, fill=site)) +
  #stat_summary(fun.data = mean_IC, geom ="errorbar", size=0.4, color="red") +
  geom_boxplot(data=subset(null_sp_full_long, metric %in% c("modularity", "weighted.NODF","robustness.ran")), outlier.shape = NA) +
  scale_fill_manual(values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  geom_point(data= subset(obs_full_sp_long, metric %in% c("modularity", "weighted.NODF","robustness.ran")), size=2, shape=8)+
  facet_grid(metric ~ ., scales="free", switch ="y")+
  ggtitle("Structure and Robustness_sp_chloe_null")+
  labs( y = "Value", x="Sites") + 
  theme(legend.position = "none") 



mean_IC <- function(x){
  r <- c(mean(x), mean(x)-1.96*sd(x)/sqrt(length(x)), mean(x) +1.96*sd(x)/sqrt(length(x)))
  names(r) <- c("y","ymin","ymax")
  r
}


source("    /chloe_null_model.R")


x <- vir_ISmatrix2


chloe_null.model <- function(x, perm) {
  
  column.index <- 1:dim(x)[2]
  
  lapply(1:perm, function (y) {
    
    a <- x[,sample(column.index, size= dim(x)[2], replace = T)]
    colnames (a) <- colnames(x)
    
    a
    
  }) 
  
}


rand_vir_sp <- chloe_null.model(vir_ISmatrix_sp2, perm= 100)  


