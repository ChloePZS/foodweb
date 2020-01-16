#################################
#Network structure | Genus level#
#################################

library(tidyverse)
library(bipartite)
library(vegan)
library(picante)
library(RColorBrewer)
library(wesanderson)

#1. Import  final matrices
vir_ISmatrix_gen2 <- read.csv("data/vir_ISmatrix_gen2.csv", sep=",", row.names = 1) %>%
  as.matrix(.)
mari_ISmatrix_gen2 <- read.csv("data/mari_ISmatrix_gen2.csv", sep=",", row.names = 1) %>%
  as.matrix(.)
nca_ISmatrix_gen2 <- read.csv("data/nca_ISmatrix_gen2.csv", sep=",", row.names = 1) %>%
  as.matrix(.)
haw_ISmatrix_gen2 <- read.csv("data/haw_ISmatrix_gen2.csv", sep=",", row.names = 1) %>%
  as.matrix(.)
mad_ISmatrix_gen2 <- read.csv("data/mad_ISmatrix_gen2.csv", sep=",", row.names = 1) %>%
  as.matrix(.)


#create randomized matrices just for 100 matrices to lower calcul time
p <- 100

rand_vir_gen <- lapply(1:p, function(x) randomizeMatrix(vir_ISmatrix_gen2, null.model = "frequency"))
rand_mari_gen <- lapply(1:p, function(x) randomizeMatrix(mari_ISmatrix_gen2, null.model = "frequency"))
rand_nca_gen <- lapply(1:p, function(x) randomizeMatrix(nca_ISmatrix_gen2, null.model = "frequency"))
rand_mad_gen <- lapply(1:p, function(x) randomizeMatrix(mad_ISmatrix_gen2, null.model = "frequency"))
rand_haw_gen <- lapply(1:p, function(x) randomizeMatrix(haw_ISmatrix_gen2, null.model = "frequency"))

colSums(rand_vir_gen[[1]])

####2. Basic indices --> Connectance + WNODF####

obs_vir_gen <- unlist(networklevel(vir_ISmatrix_gen2, empty.web = TRUE, index=c("number of species","connectance","weighted connectance","weighted NODF",
                                                                        "linkage density","generality","vulnerability",
                                                                        "links per species","mean number of shared partners")))
obs_vir_gen <-  data.frame(t(obs_vir_gen)) %>%
  mutate(site = "vir",
         value = "obs") 

obs_mari_gen <- unlist(networklevel(mari_ISmatrix_gen2, empty.web = TRUE, index=c("number of species","connectance","weighted connectance","weighted NODF",
                                                                                "linkage density","generality","vulnerability",
                                                                                "links per species","mean number of shared partners")))
obs_mari_gen <-  data.frame(t(obs_mari_gen)) %>%
  mutate(site = "mari",
         value = "obs")

obs_nca_gen <- unlist(networklevel(nca_ISmatrix_gen2, empty.web = TRUE, index=c("number of species","connectance","weighted connectance","weighted NODF",
                                                                                "linkage density","generality","vulnerability",
                                                                                "links per species","mean number of shared partners")))
obs_nca_gen <-  data.frame(t(obs_nca_gen)) %>%
  mutate(site = "nca",
         value = "obs")


obs_mad_gen <- unlist(networklevel(mad_ISmatrix_gen2, empty.web = TRUE, index=c("number of species","connectance","weighted connectance","weighted NODF",
                                                                                "linkage density","generality","vulnerability",
                                                                                "links per species","mean number of shared partners")))
obs_mad_gen <-  data.frame(t(obs_mad_gen)) %>%
  mutate(site = "mad",
         value = "obs")

obs_haw_gen <- unlist(networklevel(haw_ISmatrix_gen2, empty.web = TRUE, index=c("number of species","connectance","weighted connectance","weighted NODF",
                                                                                "linkage density","generality","vulnerability",
                                                                                "links per species","mean number of shared partners")))
obs_haw_gen <-  data.frame(t(obs_haw_gen)) %>%
  mutate(site = "haw",
         value = "obs")

obs_full_gen <- plyr::rbind.fill(obs_mad_gen, obs_nca_gen, obs_mari_gen, obs_haw_gen, obs_vir_gen)


#Null models
null_vir_gen <- unlist(sapply(rand_vir_gen, networklevel, weighted=TRUE, empty.web = TRUE, index=c("connectance","weighted connectance","weighted NODF")))
null_mari_gen <- unlist(sapply(rand_mari_gen, networklevel, weighted=TRUE, empty.web = TRUE, index=c("connectance","weighted connectance","weighted NODF")))
null_nca_gen <- unlist(sapply(rand_nca_gen, networklevel, weighted=TRUE, empty.web = TRUE, index=c("connectance","weighted connectance","weighted NODF")))
null_mad_gen <- unlist(sapply(rand_mad_gen, networklevel, weighted=TRUE, empty.web = TRUE, index=c("connectance","weighted connectance","weighted NODF")))
null_haw_gen <- unlist(sapply(rand_haw_gen, networklevel, weighted=TRUE, empty.web = TRUE, index=c("connectance","weighted connectance","weighted NODF")))


#Dataframe with random distributions
df_null_gen_haw <- t(null_haw_gen) %>% data.frame(.)
df_null_gen_haw$site <- "haw"
df_null_gen_haw$value <- "null"

df_null_gen_mari <- t(null_mari_gen) %>% data.frame(.) 
df_null_gen_mari$site <- "mari"
df_null_gen_mari$value <- "null"

df_null_gen_mad <- t(null_mad_gen) %>% data.frame(.) 
df_null_gen_mad$site <- "mad"
df_null_gen_mad$value <- "null"

df_null_gen_nca <- t(null_nca_gen) %>% data.frame(.) 
df_null_gen_nca$site <- "nca"
df_null_gen_nca$value <- "null"

df_null_gen_vir <- t(null_vir_gen) %>% data.frame(.) 
df_null_gen_vir$site <- "vir"
df_null_gen_vir$value <- "null"

null_gen_full <- plyr::rbind.fill(df_null_gen_mad, df_null_gen_nca, df_null_gen_mari, df_null_gen_haw, df_null_gen_vir) 



####3. Modularity####
mod_vir_gen <- computeModules(vir_ISmatrix_gen2, method="Beckett", forceLPA = FALSE) #DIRT algorithm from Becket 2016
mod_mari_gen <- computeModules(mari_ISmatrix_gen2, method="Beckett", forceLPA = FALSE)
mod_mad_gen <- computeModules(mad_ISmatrix_gen2, method="Beckett", forceLPA = FALSE)
mod_nca_gen <- computeModules(nca_ISmatrix_gen2, method="Beckett", forceLPA = FALSE)
mod_haw_gen <- computeModules(haw_ISmatrix_gen2, method="Beckett", forceLPA = FALSE)

obs_full_gen$modularity <- c(mod_mad_gen@likelihood, mod_nca_gen@likelihood, mod_mari_gen@likelihood, mod_haw_gen@likelihood, mod_vir_gen@likelihood)

plotModuleWeb(mod_vir_gen)

#Null modularity
mod_mari_gen_null <- unlist(sapply(rand_mari_gen, computeModules, method="Beckett",forceLPA = TRUE))
mod_mad_gen_null <- unlist(sapply(rand_mad_gen, computeModules, method="Beckett", forceLPA = TRUE))
mod_haw_gen_null <- unlist(sapply(rand_haw_gen, computeModules, method="Beckett", forceLPA = TRUE))
mod_nca_gen_null <- unlist(sapply(rand_nca_gen, computeModules, method="Beckett", forceLPA = TRUE))
mod_vir_gen_null <- unlist(sapply(rand_vir_gen, computeModules, method="Beckett", forceLPA = TRUE))

unique(null_gen_full$site)

null_gen_full$modularity <- c(sapply(mod_mad_gen_null, function(x) x@likelihood),
                              sapply(mod_nca_gen_null, function(x) x@likelihood),
                              sapply(mod_mari_gen_null, function(x) x@likelihood),
                              sapply(mod_haw_gen_null, function(x) x@likelihood),
                              sapply(mod_vir_gen_null, function(x) x@likelihood))

####4. Robustness####
#To random extinction
ex_ran_vir_gen <- second.extinct(vir_ISmatrix_gen2, participant = "lower", method="random", nrep=1000, details=FALSE)
robustness(ex_ran_vir_gen)
slope.bipartite(ex_ran_vir_gen)

ex_ran_mari_gen <- second.extinct(mari_ISmatrix_gen2, participant = "lower", method="random", nrep=1000, details=FALSE)
robustness(ex_ran_mari_gen)
slope.bipartite(ex_ran_mari_gen)

ex_ran_haw_gen <- second.extinct(haw_ISmatrix_gen2, participant = "lower", method="random", nrep=1000, details=FALSE)
robustness(ex_ran_haw_gen)
slope.bipartite(ex_ran_haw_gen)

ex_ran_mad_gen <- second.extinct(mad_ISmatrix_gen2, participant = "lower", method="random", nrep=1000, details=FALSE)
robustness(ex_ran_mad_gen)
slope.bipartite(ex_ran_mad_gen)

ex_ran_nca_gen <- second.extinct(nca_ISmatrix_gen2, participant = "lower", method="random", nrep=1000, details=FALSE)
robustness(ex_ran_nca_gen)
slope.bipartite(ex_ran_nca_gen)
                              
unique(obs_full_gen$site)

obs_full_gen$robustness.ran <- c(robustness(ex_ran_mad_gen), robustness(ex_ran_nca_gen), robustness(ex_ran_mari_gen), robustness(ex_ran_haw_gen), robustness(ex_ran_vir_gen))

#Robustness null models
rob_vir_gen_null <- lapply(rand_vir_gen, second.extinct, participant = "lower", method="random", nrep=1, details=FALSE)
rob_mari_gen_null <- lapply(rand_mari_gen, second.extinct, participant = "lower", method="random", nrep=1, details=FALSE)
rob_nca_gen_null <- lapply(rand_nca_gen, second.extinct, participant = "lower", method="random", nrep=1, details=FALSE)
rob_haw_gen_null <- lapply(rand_haw_gen, second.extinct, participant = "lower", method="random", nrep=1, details=FALSE)
rob_mad_gen_null <- lapply(rand_mad_gen, second.extinct, participant = "lower", method="random", nrep=1, details=FALSE)

null_gen_full$robustness.ran <- c(sapply(rob_mad_gen_null, function(x) robustness(x)),
                                  sapply(rob_nca_gen_null, function(x) robustness(x)),
                                  sapply(rob_mari_gen_null, function(x) robustness(x)),
                                  sapply(rob_haw_gen_null, function(x) robustness(x)),
                                  sapply(rob_vir_gen_null, function(x) robustness(x)))


#Tranform into long format
obs_full_gen_long <- gather(obs_full_gen, key = "metric",value ="metric_val", -site, -value)
null_gen_full_long <- gather (null_gen_full, key = "metric", value = "metric_val", -site, -value)
  
####5. Plots####
#Complexity
ggplot(data=subset(null_gen_full_long, metric %in% c("connectance", "weighted.connectance")), aes(x=site, y=metric_val, fill=site)) +
  #stat_summary(fun.data = mean_IC, geom ="errorbar", size=0.4, color="red") +
  geom_boxplot(data=subset(null_gen_full_long, metric %in% c("connectance", "weighted.connectance")), outlier.shape = NA) +
  scale_fill_manual(values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  geom_point(data= subset(obs_full_gen_long, metric %in% c("connectance", "weighted.connectance")), size=2, shape=8)+
  facet_grid(metric ~ ., scales="free", switch ="y")+
  ggtitle("Complexity_Genus") +
  labs( y = "Value", x="Sites")  +
  theme(legend.position = "none")


#Structure
ggplot(data=subset(null_gen_full_long, metric %in% c("modularity", "weighted.NODF","robustness.ran")), aes(x=site, y=metric_val, fill=site)) +
  #stat_summary(fun.data = mean_IC, geom ="errorbar", size=0.4, color="red") +
  geom_boxplot(data=subset(null_gen_full_long, metric %in% c("modularity", "weighted.NODF","robustness.ran")), outlier.shape = NA) +
  scale_fill_manual(values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  geom_point(data= subset(obs_full_gen_long, metric %in% c("modularity", "weighted.NODF","robustness.ran")), size=2, shape=8)+
  facet_grid(metric ~ ., scales="free", switch ="y")+
  ggtitle("Structure and Robustness_Genus") +
  labs( y = "Value", x="Sites") + 
  theme(legend.position = "none")

