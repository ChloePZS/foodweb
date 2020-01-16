###############################
#Network metrics species level#
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

#Check null models constrains
colSums(rand_vir_sp[[1]])
rowSums(rand_vir_sp[[1]])
vir1 <- rand_vir_sp[[1]]

#And keep weights so assign to different prey items
rand_vir_sp[[90]][,3][which(rand_vir_sp[[90]][,3] !=0)]
vir_ISmatrix_sp2[,3][which(vir_ISmatrix_sp2[,3] !=0)]

#Keep the number of prey/pred
length(which(rand_vir_sp[[9]][,7] != 0))
length(which(vir_ISmatrix_sp2 != 0)) #number of links
length(which(rand_vir_sp[[8]] != 0)) #number of links

length(which(colSums(vir_ISmatrix_sp2) != 0)) #nb of predators

#Check weights distribution
hist(vir_ISmatrix_sp2)
hist(rand_vir_sp[[90]])

hist(rowSums(mari_ISmatrix_sp2)) #more preys with weak interactions strength
hist(rowSums(rand_mari_sp[[5]])) #more preys with intermediate strengh , close to normal distribution

#unweighted G = nb links/nb of predators

colSums(vir_ISmatrix_sp2)
rowSums(vir_ISmatrix_sp2)

sum(vir_ISmatrix_sp2)
ncol(vir_ISmatrix_sp2)

####2. Basic indices --> Connectance + WNODF####

obs_vir_sp <- unlist(networklevel(vir_ISmatrix_sp2, empty.web = TRUE, index=c("number of species","connectance","weighted connectance","NODF","weighted NODF",
                                                                                "linkage density","generality","vulnerability",
                                                                                "links per species","mean number of shared partners","niche overlap")))
obs_vir_sp <-  data.frame(t(obs_vir_sp)) %>%
  mutate(site = "vir",
         value = "obs")
#when setting weighted FALSE, vulnerability measure decreases

obs_mari_sp <- unlist(networklevel(mari_ISmatrix_sp2, empty.web = TRUE, index=c("number of species","connectance","weighted connectance","NODF","weighted NODF",
                                                                                  "linkage density","generality","vulnerability",
                                                                                  "links per species","mean number of shared partners","niche overlap")))
obs_mari_sp <-  data.frame(t(obs_mari_sp)) %>%
  mutate(site = "mari",
         value = "obs")

obs_nca_sp <- unlist(networklevel(nca_ISmatrix_sp2, empty.web = TRUE, index=c("number of species","connectance","weighted connectance","NODF","weighted NODF",
                                                                                "linkage density","generality","vulnerability",
                                                                                "links per species","mean number of shared partners", "niche overlap")))
obs_nca_sp <-  data.frame(t(obs_nca_sp)) %>%
  mutate(site = "nca",
         value = "obs")


obs_mad_sp <- unlist(networklevel(mad_ISmatrix_sp2, empty.web = TRUE, index=c("number of species","connectance","weighted connectance","NODF","weighted NODF",
                                                                                "linkage density","generality","vulnerability",
                                                                                "links per species","mean number of shared partners","niche overlap")))
obs_mad_sp <-  data.frame(t(obs_mad_sp)) %>%
  mutate(site = "mad",
         value = "obs")

obs_haw_sp <- unlist(networklevel(haw_ISmatrix_sp2, empty.web = TRUE, index=c("number of species","connectance","weighted connectance","NODF","weighted NODF",
                                                                                "linkage density","generality","vulnerability",
                                                                                "links per species","mean number of shared partners","niche overlap")))
obs_haw_sp <-  data.frame(t(obs_haw_sp)) %>%
  mutate(site = "haw",
         value = "obs")

obs_full_sp <- plyr::rbind.fill(obs_mad_sp, obs_nca_sp, obs_mari_sp, obs_haw_sp, obs_vir_sp)



#Get qualitative generality and vulnerability measures
obs_full_sp$generality <-  c(length(which(mad_ISmatrix_sp2 != 0))/length(which(colSums(mad_ISmatrix_sp2) != 0)),
                             length(which(nca_ISmatrix_sp2 != 0))/length(which(colSums(nca_ISmatrix_sp2) != 0)),
                             length(which(mari_ISmatrix_sp2 != 0))/length(which(colSums(mari_ISmatrix_sp2) != 0)),
                             length(which(haw_ISmatrix_sp2 != 0))/length(which(colSums(haw_ISmatrix_sp2) != 0)),
                             length(which(vir_ISmatrix_sp2 != 0))/length(which(colSums(vir_ISmatrix_sp2) != 0)))

obs_full_sp$vulnerability <-  c(length(which(mad_ISmatrix_sp2 != 0))/length(which(rowSums(mad_ISmatrix_sp2) != 0)),
                             length(which(nca_ISmatrix_sp2 != 0))/length(which(rowSums(nca_ISmatrix_sp2) != 0)),
                             length(which(mari_ISmatrix_sp2 != 0))/length(which(rowSums(mari_ISmatrix_sp2) != 0)),
                             length(which(haw_ISmatrix_sp2 != 0))/length(which(rowSums(haw_ISmatrix_sp2) != 0)),
                             length(which(vir_ISmatrix_sp2 != 0))/length(which(rowSums(vir_ISmatrix_sp2) != 0)))


length(which(vir_ISmatrix_sp2 != 0)) #number of links
length(which(colSums(vir_ISmatrix_sp2) != 0)) #nb of predators
length(which(rowSums(vir_ISmatrix_sp2) != 0)) #nb of preys


#Degree distribution
degreedistr(vir_ISmatrix_sp2, plot.it = T) #seems to follow a truncated power law, would need to test that for each random matrices. So many sp with few links and few with many links
degreedistr(rand_vir_sp[[5]], plot.it = T) #degree distribution for the predatrs won't change coz nb of prey per pred fixed. But nb of links per prey would


#Nestedness contribution
nestedcontribution(vir_ISmatrix_sp2, nsimul = 10)


#Null models
null_vir_sp <- unlist(sapply(rand_vir_sp, networklevel, weighted=TRUE, empty.web = TRUE, index=c("connectance","weighted connectance","weighted NODF")))
null_mari_sp <- unlist(sapply(rand_mari_sp, networklevel, weighted=TRUE, empty.web = TRUE, index=c("connectance","weighted connectance","weighted NODF")))
null_nca_sp <- unlist(sapply(rand_nca_sp, networklevel, weighted=TRUE, empty.web = TRUE, index=c("connectance","weighted connectance","weighted NODF")))
null_mad_sp <- unlist(sapply(rand_mad_sp, networklevel, weighted=TRUE, empty.web = TRUE, index=c("connectance","weighted connectance","weighted NODF")))
null_haw_sp <- unlist(sapply(rand_haw_sp, networklevel, weighted=TRUE, empty.web = TRUE, index=c("connectance","weighted connectance","weighted NODF")))


#Dataframe with random distributions
df_null_sp_haw <- t(null_haw_sp) %>% data.frame(.)
df_null_sp_haw$site <- "haw"
df_null_sp_haw$value <- "null"

df_null_sp_mari <- t(null_mari_sp) %>% data.frame(.) 
df_null_sp_mari$site <- "mari"
df_null_sp_mari$value <- "null"

df_null_sp_mad <- t(null_mad_sp) %>% data.frame(.) 
df_null_sp_mad$site <- "mad"
df_null_sp_mad$value <- "null"

df_null_sp_nca <- t(null_nca_sp) %>% data.frame(.) 
df_null_sp_nca$site <- "nca"
df_null_sp_nca$value <- "null"

df_null_sp_vir <- t(null_vir_sp) %>% data.frame(.) 
df_null_sp_vir$site <- "vir"
df_null_sp_vir$value <- "null"

null_sp_full <- plyr::rbind.fill(df_null_sp_mad, df_null_sp_nca, df_null_sp_mari, df_null_sp_haw, df_null_sp_vir) 


####3. Modularity####
mod_vir_sp <- computeModules(vir_ISmatrix_sp2, method="Beckett", forceLPA = FALSE) #DIRT algorithm from Becket 2016
mod_mari_sp <- computeModules(mari_ISmatrix_sp2, method="Beckett", forceLPA = FALSE)
mod_mad_sp <- computeModules(mad_ISmatrix_sp2, method="Beckett", forceLPA = FALSE)
mod_nca_sp <- computeModules(nca_ISmatrix_sp2, method="Beckett", forceLPA = FALSE)
mod_haw_sp <- computeModules(haw_ISmatrix_sp2, method="Beckett", forceLPA = FALSE)

obs_full_sp$modularity <- c(mod_mad_sp@likelihood, mod_nca_sp@likelihood, mod_mari_sp@likelihood, mod_haw_sp@likelihood, mod_vir_sp@likelihood)

plotModuleWeb(mod_vir_sp)

#Null modularity
mod_mari_sp_null <- unlist(sapply(rand_mari_sp, computeModules, method="Beckett",forceLPA = TRUE))
mod_mad_sp_null <- unlist(sapply(rand_mad_sp, computeModules, method="Beckett", forceLPA = TRUE))
mod_haw_sp_null <- unlist(sapply(rand_haw_sp, computeModules, method="Beckett", forceLPA = TRUE))
mod_nca_sp_null <- unlist(sapply(rand_nca_sp, computeModules, method="Beckett", forceLPA = TRUE))
mod_vir_sp_null <- unlist(sapply(rand_vir_sp, computeModules, method="Beckett", forceLPA = TRUE))

null_sp_full$modularity <- c(sapply(mod_mad_sp_null, function(x) x@likelihood),
                              sapply(mod_nca_sp_null, function(x) x@likelihood),
                              sapply(mod_mari_sp_null, function(x) x@likelihood),
                              sapply(mod_haw_sp_null, function(x) x@likelihood),
                              sapply(mod_vir_sp_null, function(x) x@likelihood))




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
cumsum(ex_ran_nca_sp[,3])/628 #proportions d'espèces éteintes

628-cumsum(ex_ran_nca_sp[,3]) #nombre cumulé d'espèces encore en vie
(628-cumsum(ex_ran_nca_sp[,3]))/628 #proportions d'espèces en vie

#How the slope.bipartite curve create
plot(ex_ran_nca_sp[,1]/38, (38-cumsum(ex_ran_nca_sp[,2]))/38) #for lower level extinction, same curve than the slope.bipartite but WHY using the 2nd column and over 38 nodes ?!
plot(ex_ran_nca_sp[,1]/38, (628-cumsum(ex_ran_nca_sp[,3]))/628) #for lower level extinction, that should be the right curve


ex_ran_nca_sp <- second.extinct(nca_ISmatrix_sp2, participant = "higher", method="random", nrep=100, details=FALSE)
slope.bipartite(ex_ran_nca_sp)
plot(ex_ran_nca_sp[,1]/628, (38-cumsum(ex_ran_nca_sp[,2]))/38) #for higher level extinction, okay here uses the 2 columns coz wanna prop of lower extinct


obs_full_sp$robustness.ran <- c(robustness(ex_ran_mad_sp), robustness(ex_ran_nca_sp), robustness(ex_ran_mari_sp), robustness(ex_ran_haw_sp), robustness(ex_ran_vir_sp))

#Robustness null models
rob_vir_sp_null <- lapply(rand_vir_sp, second.extinct, participant = "lower", method="random", nrep=10, details=FALSE)
rob_mari_sp_null <- lapply(rand_mari_sp, second.extinct, participant = "lower", method="random", nrep=10, details=FALSE)
rob_nca_sp_null <- lapply(rand_nca_sp, second.extinct, participant = "lower", method="random", nrep=10, details=FALSE)
rob_haw_sp_null <- lapply(rand_haw_sp, second.extinct, participant = "lower", method="random", nrep=10, details=FALSE)
rob_mad_sp_null <- lapply(rand_mad_sp, second.extinct, participant = "lower", method="random", nrep=10, details=FALSE)

unique(null_sp_full$site)

#Try to show curve for null distribution
rob_nca_sp_null[[3]] #only extinctions for higher level, so why not the case for the observed matrix
slope.bipartite(rob_nca_sp_null[[3]]) 
robustness(rob_nca_sp_null[[3]])
#Example plot 
plot(rob_nca_sp_null[[1]][,1]/38, (628 - cumsum(rob_nca_sp_null[[1]][,3]))/628, type="l", xlab = "Proportion of primary extinctions - Prey",
     ylab= "Proportions of secon Predator diversity", ylim=c(0,0.6)) #okay and here that's thats the third column used
lines(ex_ran_nca_sp[,1]/38, (628-cumsum(ex_ran_nca_sp[,3]))/628, type="l", col="red") #observed curve
lines(ex_ran_nca_sp[,1]/38, (38-cumsum(ex_ran_nca_sp[,2]))/38, type="l", col="blue") #observed curve

#I need the mean extinction curve for each region

null_sp_full$robustness.ran <- c(sapply(rob_mad_sp_null, function(x) robustness(x)),
                                  sapply(rob_nca_sp_null, function(x) robustness(x)),
                                  sapply(rob_mari_sp_null, function(x) robustness(x)),
                                  sapply(rob_haw_sp_null, function(x) robustness(x)),
                                  sapply(rob_vir_sp_null, function(x) robustness(x)))


#Tranform into long format
obs_full_sp_long <- gather(obs_full_sp, key = "metric",value ="metric_val", -site, -value)
null_sp_full_long <- gather (null_sp_full, key = "metric", value = "metric_val", -site, -value)


####5. Plots####
#Complexity
ggplot(data=subset(null_sp_full_long, metric %in% c("connectance", "weighted.connectance")), aes(x=site, y=metric_val, fill=site)) +
  stat_summary(fun.data = mean_IC, geom ="errorbar", size=0.4, color="red") +
  geom_boxplot(data=subset(null_sp_full_long, metric %in% c("connectance", "weighted.connectance")), outlier.shape = NA) +
  scale_fill_manual(values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  geom_point(data= subset(obs_full_sp_long, metric %in% c("connectance", "weighted.connectance")), size=2, shape=8)+
  facet_grid(metric ~ ., scales="free", switch ="y")+
  ggtitle("Complexity_sp") +
  labs( y = "Value", x="Sites") + 
  theme(legend.position = "none") 


#Structure
ggplot(data=subset(null_sp_full_long, metric %in% c("modularity", "weighted.NODF","robustness.ran","robustness.ran2")), aes(x=site, y=metric_val, fill=site)) +
  stat_summary(fun.data = mean_se, geom ="errorbar", size=0.4, color="red") +
  geom_boxplot(data=subset(null_sp_full_long, metric %in% c("modularity", "weighted.NODF","robustness.ran","robustness.ran2")), outlier.shape = NA) +
  scale_fill_manual(values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  geom_point(data= subset(obs_full_sp_long, metric %in% c("modularity", "weighted.NODF","robustness.ran","robustness.ran2")), size=2, shape=8)+
  facet_grid(metric ~ ., scales="free", switch ="y")+
  ggtitle("Structure and Robustness_sp")+
  labs( y = "Value", x="Sites") + 
  theme(legend.position = "none") 


##Robustness without full set of nodes
#Try redo the boxplot with random distribution using the .std matrices without full sets of nodes
#1. To random extinction#### detais = FALSE
ex_ran_vir_sp <- second.extinct(vir_ISmatrix_sp_std, participant = "lower", method="random", nrep=1000, details=FALSE) #when set to FALSE, returns a list of the nrep repetitions

ex_ran_mari_sp <- second.extinct(mari_ISmatrix_sp_std, participant = "lower", method="random", nrep=1000, details=FALSE)

robustness(ex_ran_mari_sp)
slope.bipartite(ex_ran_mari_sp) #Can't work if details = FALSE

ex_ran_haw_sp <- second.extinct(haw_ISmatrix_sp_std, participant = "lower", method="random", nrep=1000, details=FALSE)

ex_ran_mad_sp <- second.extinct(mad_ISmatrix_sp_std, participant = "lower", method="random", nrep=1000, details=FALSE)

ex_ran_nca_sp <- second.extinct(nca_ISmatrix_sp_std, participant = "lower", method="random", nrep=1000, details=FALSE)

obs_full_sp$robustness.ran2 <- c(robustness(ex_ran_mad_sp), robustness(ex_ran_nca_sp), robustness(ex_ran_mari_sp), robustness(ex_ran_haw_sp), robustness(ex_ran_vir_sp))


#Robustness null models

#create randomized matrices just for 100 matrices to lower calcul time
p <- 100

rand_vir_sp <- lapply(1:p, function(x) randomizeMatrix(vir_ISmatrix_sp_std, null.model = "frequency"))
rand_mari_sp <- lapply(1:p, function(x) randomizeMatrix(mari_ISmatrix_sp_std, null.model = "frequency"))
rand_nca_sp <- lapply(1:p, function(x) randomizeMatrix(nca_ISmatrix_sp_std, null.model = "frequency"))
rand_mad_sp <- lapply(1:p, function(x) randomizeMatrix(mad_ISmatrix_sp_std, null.model = "frequency"))
rand_haw_sp <- lapply(1:p, function(x) randomizeMatrix(haw_ISmatrix_sp_std, null.model = "frequency"))

rob_vir_sp_null <- lapply(rand_vir_sp, second.extinct, participant = "lower", method="random", nrep=10, details=FALSE)
rob_mari_sp_null <- lapply(rand_mari_sp, second.extinct, participant = "lower", method="random", nrep=10, details=FALSE)
rob_nca_sp_null <- lapply(rand_nca_sp, second.extinct, participant = "lower", method="random", nrep=10, details=FALSE)
rob_haw_sp_null <- lapply(rand_haw_sp, second.extinct, participant = "lower", method="random", nrep=10, details=FALSE)
rob_mad_sp_null <- lapply(rand_mad_sp, second.extinct, participant = "lower", method="random", nrep=10, details=FALSE)


null_sp_full$robustness.ran2 <- c(sapply(rob_mad_sp_null, function(x) robustness(x)),
                                  sapply(rob_nca_sp_null, function(x) robustness(x)),
                                  sapply(rob_mari_sp_null, function(x) robustness(x)),
                                  sapply(rob_haw_sp_null, function(x) robustness(x)),
                                  sapply(rob_vir_sp_null, function(x) robustness(x)))


#Tranform into long format
obs_full_sp_long <- gather(obs_full_sp, key = "metric",value ="metric_val", -site, -value)
null_sp_full_long <- gather (null_sp_full, key = "metric", value = "metric_val", -site, -value)

ggplot(data=subset(null_sp_full_long, metric %in% c("modularity", "weighted.NODF","robustness.ran","robustness.ran2")), aes(x=site, y=metric_val, fill=site)) +
  #stat_summary(fun.data = mean_IC, geom ="errorbar", size=0.4, color="red") +
  geom_boxplot(data=subset(null_sp_full_long, metric %in% c("modularity", "weighted.NODF","robustness.ran","robustness.ran2")), outlier.shape = NA) +
  scale_fill_manual(values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  geom_point(data= subset(obs_full_sp_long, metric %in% c("modularity", "weighted.NODF","robustness.ran","robustness.ran2")), size=2, shape=8)+
  facet_grid(metric ~ ., scales="free", switch ="y")+
  ggtitle("Structure and Robustness_sp")+
  labs( y = "Value", x="Sites") + 
  theme(legend.position = "none") 




source("    /chloe_null_model.R")


x <- vir_ISmatrix2


chloe_null.model <- function(x, perm) {
  
                        column.index <- 1:dim(x)[2]
                        
                        lapply(1:perm, function (y) {
                          
                          a <- x[,sample(column.index, size= dim(x)[2])]
                          colnames (a) <- colnames(x)
                          
                          a
                          
                        }) 
  
}
  
  
rand_vir_sp <- chloe_null.model(vir_ISmatrix_sp2, perm= 100)  
  
  
