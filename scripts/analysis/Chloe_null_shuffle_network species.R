###############################
#Network metrics species level# Suffling columns of randomized matrices 
###############################

library(tidyverse)
library(bipartite)
library(vegan)
library(picante)
library(RColorBrewer)
library(wesanderson)

#1. Import  final matrices
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


#create randomized matrices just for 100 matrices to lower calcul time
p <- 100

rand_vir_sp <- lapply(1:p, function(x) randomizeMatrix(vir_ISmatrix_sp2, null.model = "frequency"))
rand_mari_sp <- lapply(1:p, function(x) randomizeMatrix(mari_ISmatrix_sp2, null.model = "frequency"))
rand_nca_sp <- lapply(1:p, function(x) randomizeMatrix(nca_ISmatrix_sp2, null.model = "frequency"))
rand_mad_sp <- lapply(1:p, function(x) randomizeMatrix(mad_ISmatrix_sp2, null.model = "frequency"))
rand_haw_sp <- lapply(1:p, function(x) randomizeMatrix(haw_ISmatrix_sp2, null.model = "frequency"))


colSums(rand_vir_sp[[1]])
rowSums(rand_vir_sp[[1]])
length(which(vir_ISmatrix_sp2[,7] != 0))
length(which(rand_vir_sp[[9]][,7] != 0))


rand_vir_sp2 <- shuffle_colums(rand_vir_sp) 
rand_mari_sp2 <- shuffle_colums(rand_mari_sp) 
rand_nca_sp2 <- shuffle_colums(rand_nca_sp) 
rand_mad_sp2 <- shuffle_colums(rand_mad_sp) 
rand_haw_sp2 <- shuffle_colums(rand_haw_sp) 


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
null_vir_sp2 <- unlist(sapply(rand_vir_sp2, networklevel, weighted=TRUE, empty.web = TRUE, index=c("connectance","weighted connectance","weighted NODF","generality","vulnerability")))
null_mari_sp2 <- unlist(sapply(rand_mari_sp2, networklevel, weighted=TRUE, empty.web = TRUE, index=c("connectance","weighted connectance","weighted NODF","generality","vulnerability")))
null_nca_sp2 <- unlist(sapply(rand_nca_sp2, networklevel, weighted=TRUE, empty.web = TRUE, index=c("connectance","weighted connectance","weighted NODF","generality","vulnerability")))
null_mad_sp2 <- unlist(sapply(rand_mad_sp2, networklevel, weighted=TRUE, empty.web = TRUE, index=c("connectance","weighted connectance","weighted NODF","generality","vulnerability")))
null_haw_sp2 <- unlist(sapply(rand_haw_sp2, networklevel, weighted=TRUE, empty.web = TRUE, index=c("connectance","weighted connectance","weighted NODF","generality","vulnerability")))


#Dataframe with random distributions
df_null_sp2_haw <- t(null_haw_sp2) %>% data.frame(.)
df_null_sp2_haw$site <- "haw"
df_null_sp2_haw$value <- "null"

df_null_sp2_mari <- t(null_mari_sp2) %>% data.frame(.) 
df_null_sp2_mari$site <- "mari"
df_null_sp2_mari$value <- "null"

df_null_sp2_mad <- t(null_mad_sp2) %>% data.frame(.) 
df_null_sp2_mad$site <- "mad"
df_null_sp2_mad$value <- "null"

df_null_sp2_nca <- t(null_nca_sp2) %>% data.frame(.) 
df_null_sp2_nca$site <- "nca"
df_null_sp2_nca$value <- "null"

df_null_sp2_vir <- t(null_vir_sp2) %>% data.frame(.) 
df_null_sp2_vir$site <- "vir"
df_null_sp2_vir$value <- "null"

null_sp2_full <- plyr::rbind.fill(df_null_sp2_mad, df_null_sp2_nca, df_null_sp2_mari, df_null_sp2_haw, df_null_sp2_vir) 


####3. Modularity####
mod_vir_sp <- computeModules(vir_ISmatrix_sp2, method="Beckett", forceLPA = FALSE) #DIRT algorithm from Becket 2016
mod_mari_sp <- computeModules(mari_ISmatrix_sp2, method="Beckett", forceLPA = FALSE)
mod_mad_sp <- computeModules(mad_ISmatrix_sp2, method="Beckett", forceLPA = FALSE)
mod_nca_sp <- computeModules(nca_ISmatrix_sp2, method="Beckett", forceLPA = FALSE)
mod_haw_sp <- computeModules(haw_ISmatrix_sp2, method="Beckett", forceLPA = FALSE)

obs_full_sp$modularity <- c(mod_mad_sp@likelihood, mod_nca_sp@likelihood, mod_mari_sp@likelihood, mod_haw_sp@likelihood, mod_vir_sp@likelihood)

plotModuleWeb(mod_vir_sp)

#Null modularity
mod_mari_sp2_null <- unlist(sapply(rand_mari_sp2, computeModules, method="Beckett",forceLPA = TRUE))
mod_mad_sp2_null <- unlist(sapply(rand_mad_sp2, computeModules, method="Beckett", forceLPA = TRUE))
mod_haw_sp2_null <- unlist(sapply(rand_haw_sp2, computeModules, method="Beckett", forceLPA = TRUE))
mod_nca_sp2_null <- unlist(sapply(rand_nca_sp2, computeModules, method="Beckett", forceLPA = TRUE))
mod_vir_sp2_null <- unlist(sapply(rand_vir_sp2, computeModules, method="Beckett", forceLPA = TRUE))

null_sp2_full$modularity <- c(sapply(mod_mad_sp2_null, function(x) x@likelihood),
                             sapply(mod_nca_sp2_null, function(x) x@likelihood),
                             sapply(mod_mari_sp2_null, function(x) x@likelihood),
                             sapply(mod_haw_sp2_null, function(x) x@likelihood),
                             sapply(mod_vir_sp2_null, function(x) x@likelihood))



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

obs_full_sp$robustness.ran <- c(robustness(ex_ran_mad_sp), robustness(ex_ran_nca_sp), robustness(ex_ran_mari_sp), robustness(ex_ran_haw_sp), robustness(ex_ran_vir_sp))

#Robustness null models
rob_vir_sp2_null <- lapply(rand_vir_sp2, second.extinct, participant = "lower", method="random", nrep=100, details=FALSE)
slope.bipartite(rob_vir_sp2_null[[2]])
rob_mari_sp2_null <- lapply(rand_mari_sp2, second.extinct, participant = "lower", method="random", nrep=100, details=FALSE)
rob_nca_sp2_null <- lapply(rand_nca_sp2, second.extinct, participant = "lower", method="random", nrep=100, details=FALSE)
rob_haw_sp2_null <- lapply(rand_haw_sp2, second.extinct, participant = "lower", method="random", nrep=100, details=FALSE)
rob_mad_sp2_null <- lapply(rand_mad_sp2, second.extinct, participant = "lower", method="random", nrep=100, details=FALSE)

unique(null_sp_full$site)


null_sp2_full$robustness.ran <- c(sapply(rob_mad_sp2_null, function(x) robustness(x)),
                                 sapply(rob_nca_sp2_null, function(x) robustness(x)),
                                 sapply(rob_mari_sp2_null, function(x) robustness(x)),
                                 sapply(rob_haw_sp2_null, function(x) robustness(x)),
                                 sapply(rob_vir_sp2_null, function(x) robustness(x)))


#Tranform into long format
obs_full_sp_long <- gather(obs_full_sp, key = "metric",value ="metric_val", -site, -value)
null_sp2_full_long <- gather (null_sp2_full, key = "metric", value = "metric_val", -site, -value)


####5. Plots####
#Complexity
ggplot(data=subset(null_sp2_full_long, metric %in% c("connectance", "weighted.connectance","generality.HL", "vulnerability.LL")), aes(x=site, y=metric_val, fill=site)) +
  #stat_summary(fun.data = mean_IC, geom ="errorbar", size=0.4, color="red") +
  geom_boxplot(data=subset(null_sp2_full_long, metric %in% c("connectance", "weighted.connectance","generality.HL", "vulnerability.LL")), outlier.shape = NA) +
  scale_fill_manual(values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  geom_point(data= subset(obs_full_sp_long, metric %in% c("connectance", "weighted.connectance","generality.HL", "vulnerability.LL")), size=2, shape=8)+
  facet_grid(metric ~ ., scales="free", switch ="y")+
  ggtitle("Complexity_sp") +
  labs( y = "Value", x="Sites") + 
  theme(legend.position = "none") 


#Structure
ggplot(data=subset(null_sp2_full_long, metric %in% c("modularity", "weighted.NODF","robustness.ran")), aes(x=site, y=metric_val, fill=site)) +
  #stat_summary(fun.data = mean_IC, geom ="errorbar", size=0.4, color="red") +
  geom_boxplot(data=subset(null_sp2_full_long, metric %in% c("modularity", "weighted.NODF","robustness.ran")), outlier.shape = NA) +
  scale_fill_manual(values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  geom_point(data= subset(obs_full_sp_long, metric %in% c("modularity", "weighted.NODF","robustness.ran")), size=2, shape=8)+
  facet_grid(metric ~ ., scales="free", switch ="y")+
  ggtitle("Structure and Robustness_sp")+
  labs( y = "Value", x="Sites") + 
  theme(legend.position = "none") 






