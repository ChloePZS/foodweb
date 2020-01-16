####################################
#Network metrics - Species - BINARY#
####################################

library(tidyverse)
library(picante)
library(bipartite)
library(vegan)
library(Hmisc)
library(RColorBrewer)

#Analysis to be done from matrices with three prey groupings

#1. Import  final matrices
vir_ISmatrix_sp_std <- read.csv("data/vir_ISmatrix_sp_std_grp6.csv", sep=",", row.names = 1) %>%
  as.matrix(.)
mari_ISmatrix_sp_std <- read.csv("data/mari_ISmatrix_sp_std_grp6.csv", sep=",", row.names = 1) %>%
  as.matrix(.)
nca_ISmatrix_sp_std <- read.csv("data/nca_ISmatrix_sp_std_grp6.csv", sep=",", row.names = 1) %>%
  as.matrix(.)
haw_ISmatrix_sp_std <- read.csv("data/haw_ISmatrix_sp_std_grp6.csv", sep=",", row.names = 1) %>%
  as.matrix(.)
mad_ISmatrix_sp_std <- read.csv("data/mad_ISmatrix_sp_std_grp6.csv", sep=",", row.names = 1) %>%
  as.matrix(.)

##2. Transform into qualitative matrices
pa_vir_sp <- decostand(vir_ISmatrix_sp_std, method="pa")
pa_mari_sp <- decostand(mari_ISmatrix_sp_std, method="pa")
pa_nca_sp <- decostand(nca_ISmatrix_sp_std, method="pa")
pa_haw_sp <- decostand(haw_ISmatrix_sp_std, method="pa")
pa_mad_sp <- decostand(mad_ISmatrix_sp_std, method="pa")

#Export binary matrices
write.csv(pa_vir_sp, "data/pa_vir_sp.csv")
write.csv(pa_mari_sp, "data/pa_mari_sp.csv")
write.csv(pa_nca_sp, "data/pa_nca_sp.csv")
write.csv(pa_haw_sp, "data/pa_haw_sp.csv")
write.csv(pa_mad_sp, "data/pa_mad_sp.csv")

  #3. create randomized matrices --> Keep colSums as observed
p <- 100

rand_vir_sp <- lapply(1:p, function(x) randomizeMatrix(pa_vir_sp, null.model = "frequency"))
rand_mari_sp <- lapply(1:p, function(x) randomizeMatrix(pa_mari_sp, null.model = "frequency"))
rand_nca_sp <- lapply(1:p, function(x) randomizeMatrix(pa_nca_sp, null.model = "frequency"))
rand_mad_sp <- lapply(1:p, function(x) randomizeMatrix(pa_mad_sp, null.model = "frequency"))
rand_haw_sp <- lapply(1:p, function(x) randomizeMatrix(pa_haw_sp, null.model = "frequency"))



obs_vir_sp <- unlist(networklevel(pa_vir_sp, empty.web = TRUE, weighted = FALSE, index=c("number of species","connectance","weighted connectance","NODF","weighted NODF",
                                                                              "linkage density","generality","vulnerability",
                                                                              "links per species","mean number of shared partners","niche overlap")))
obs_vir_sp <-  data.frame(t(obs_vir_sp)) %>%
  mutate(site = "vir",
         value = "obs")
#when setting weighted FALSE, vulnerability measure decreases

obs_mari_sp <- unlist(networklevel(pa_mari_sp, empty.web = TRUE, index=c("number of species","connectance","weighted connectance","NODF","weighted NODF",
                                                                                "linkage density","generality","vulnerability",
                                                                                "links per species","mean number of shared partners","niche overlap")))
obs_mari_sp <-  data.frame(t(obs_mari_sp)) %>%
  mutate(site = "mari",
         value = "obs")

obs_nca_sp <- unlist(networklevel(pa_nca_sp, empty.web = TRUE, index=c("number of species","connectance","weighted connectance","NODF","weighted NODF",
                                                                              "linkage density","generality","vulnerability",
                                                                              "links per species","mean number of shared partners", "niche overlap")))
obs_nca_sp <-  data.frame(t(obs_nca_sp)) %>%
  mutate(site = "nca",
         value = "obs")


obs_mad_sp <- unlist(networklevel(pa_mad_sp, empty.web = TRUE, index=c("number of species","connectance","weighted connectance","NODF","weighted NODF",
                                                                              "linkage density","generality","vulnerability",
                                                                              "links per species","mean number of shared partners","niche overlap")))
obs_mad_sp <-  data.frame(t(obs_mad_sp)) %>%
  mutate(site = "mad",
         value = "obs")

obs_haw_sp <- unlist(networklevel(pa_haw_sp, empty.web = TRUE, index=c("number of species","connectance","weighted connectance","NODF","weighted NODF",
                                                                              "linkage density","generality","vulnerability",
                                                                              "links per species","mean number of shared partners","niche overlap")))
obs_haw_sp <-  data.frame(t(obs_haw_sp)) %>%
  mutate(site = "haw",
         value = "obs")

obs_full_sp <- plyr::rbind.fill(obs_mad_sp, obs_nca_sp, obs_mari_sp, obs_haw_sp, obs_vir_sp)


#Null models
null_vir_sp <- unlist(sapply(rand_vir_sp, networklevel, weighted=FALSE, empty.web = TRUE, index=c("connectance","NODF")))
null_mari_sp <- unlist(sapply(rand_mari_sp, networklevel, weighted=FALSE, empty.web = TRUE, index=c("connectance","NODF")))
null_nca_sp <- unlist(sapply(rand_nca_sp, networklevel, weighted=FALSE, empty.web = TRUE, index=c("connectance","NODF")))
null_mad_sp <- unlist(sapply(rand_mad_sp, networklevel, weighted=FALSE, empty.web = TRUE, index=c("connectance","NODF")))
null_haw_sp <- unlist(sapply(rand_haw_sp, networklevel, weighted=FALSE, empty.web = TRUE, index=c("connectance","NODF")))


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
mod_vir_sp <- computeModules(pa_vir_sp, method="Beckett", forceLPA = FALSE) #DIRT algorithm from Becket 2016, but with binary networks should use Newman's modularity
mod_mari_sp <- computeModules(pa_mari_sp, method="Beckett", forceLPA = FALSE)
mod_mad_sp <- computeModules(pa_mad_sp, method="Beckett", forceLPA = FALSE)
mod_nca_sp <- computeModules(pa_nca_sp, method="Beckett", forceLPA = FALSE)
mod_haw_sp <- computeModules(pa_haw_sp, method="Beckett", forceLPA = FALSE)

obs_full_sp$modularity <- c(mod_mad_sp@likelihood, mod_nca_sp@likelihood, mod_mari_sp@likelihood, mod_haw_sp@likelihood, mod_vir_sp@likelihood)

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
ex_ran_vir_sp <- second.extinct(pa_vir_sp, participant = "lower", method="random", nrep=100, details=FALSE)
robustness(ex_ran_vir_sp)
slope.bipartite(ex_ran_vir_sp)

#Check robustness value
MESS::auc(x = cumsum(ex_ran_vir_sp[,2])/30, y = (191-cumsum(ex_ran_vir_sp[,3]))/191)
plot(x = cumsum(ex_ran_vir_sp[,2])/30, y = (191-cumsum(ex_ran_vir_sp[,3]))/191) # proportions of sp still alive en fonction de primary extinctions

ex_ran_mari_sp <- second.extinct(pa_mari_sp, participant = "lower", method="random", nrep=100, details=FALSE)
robustness(ex_ran_mari_sp)
slope.bipartite(ex_ran_mari_sp)

MESS::auc(x = cumsum(ex_ran_mari_sp[,2])/27, y = (139-cumsum(ex_ran_mari_sp[,3]))/139) #so it seems robustness value is a bit
plot(x = cumsum(ex_ran_mari_sp[,2])/27, y = (139-cumsum(ex_ran_mari_sp[,3]))/139)

ex_ran_haw_sp <- second.extinct(pa_haw_sp, participant = "lower", method="random", nrep=100, details=FALSE)
robustness(ex_ran_haw_sp)
slope.bipartite(ex_ran_haw_sp)

ex_ran_mad_sp <- second.extinct(pa_mad_sp, participant = "lower", method="random", nrep=100, details=FALSE)
robustness(ex_ran_mad_sp)
slope.bipartite(ex_ran_mad_sp)

ex_ran_nca_sp <- second.extinct(pa_nca_sp, participant = "lower", method="random", nrep=100, details=FALSE)
robustness(ex_ran_nca_sp)
slope.bipartite(ex_ran_nca_sp)


#ex_ran_nca_sp <- second.extinct(nca_ISmatrix_sp_std, participant = "higher", method="random", nrep=100, details=FALSE)
#slope.bipartite(ex_ran_nca_sp)
#plot(ex_ran_nca_sp[,1]/628, (38-cumsum(ex_ran_nca_sp[,2]))/38) #for higher level extinction, okay here uses the 2 columns coz wanna prop of lower extinct


obs_full_sp$robustness.ran <- c(robustness(ex_ran_mad_sp), robustness(ex_ran_nca_sp), robustness(ex_ran_mari_sp), robustness(ex_ran_haw_sp), robustness(ex_ran_vir_sp))

#Robustness null models
rob_vir_sp_null <- lapply(rand_vir_sp, second.extinct, participant = "lower", method="random", nrep=10, details=FALSE)
rob_mari_sp_null <- lapply(rand_mari_sp, second.extinct, participant = "lower", method="random", nrep=10, details=FALSE)
rob_nca_sp_null <- lapply(rand_nca_sp, second.extinct, participant = "lower", method="random", nrep=10, details=FALSE)
rob_haw_sp_null <- lapply(rand_haw_sp, second.extinct, participant = "lower", method="random", nrep=10, details=FALSE)
rob_mad_sp_null <- lapply(rand_mad_sp, second.extinct, participant = "lower", method="random", nrep=10, details=FALSE)

null_sp_full$robustness.ran <- c(sapply(rob_mad_sp_null, function(x) robustness(x)),
                                 sapply(rob_nca_sp_null, function(x) robustness(x)),
                                 sapply(rob_mari_sp_null, function(x) robustness(x)),
                                 sapply(rob_haw_sp_null, function(x) robustness(x)),
                                 sapply(rob_vir_sp_null, function(x) robustness(x)))

#Robustness degree connectivity
ex_deg_vir_sp <- second.extinct(pa_vir_sp, participant = "lower", method="degree", nrep=100, details=FALSE)
robustness(ex_deg_vir_sp)
slope.bipartite(ex_deg_vir_sp)

#Check robustness value
MESS::auc(x = cumsum(ex_deg_vir_sp[,2])/30, y = (191-cumsum(ex_deg_vir_sp[,3]))/191)
plot(x = cumsum(ex_deg_vir_sp[,2])/30, y = (191-cumsum(ex_deg_vir_sp[,3]))/191) # proportions of sp still alive en fonction de primary extinctions

ex_deg_mari_sp <- second.extinct(pa_mari_sp, participant = "lower", method="degree", nrep=100, details=FALSE)
robustness(ex_deg_mari_sp)
slope.bipartite(ex_deg_mari_sp)

MESS::auc(x = cumsum(ex_deg_mari_sp[,2])/27, y = (139-cumsum(ex_deg_mari_sp[,3]))/139) #so it seems robustness value is a bit
plot(x = cumsum(ex_deg_mari_sp[,2])/27, y = (139-cumsum(ex_deg_mari_sp[,3]))/139)

ex_deg_haw_sp <- second.extinct(pa_haw_sp, participant = "lower", method="degree", nrep=100, details=FALSE)
robustness(ex_deg_haw_sp)
slope.bipartite(ex_deg_haw_sp)

ex_deg_mad_sp <- second.extinct(pa_mad_sp, participant = "lower", method="degree", nrep=100, details=FALSE)
robustness(ex_deg_mad_sp)
slope.bipartite(ex_deg_mad_sp)

ex_deg_nca_sp <- second.extinct(pa_nca_sp, participant = "lower", method="degree", nrep=100, details=FALSE)
robustness(ex_deg_nca_sp)
slope.bipartite(ex_deg_nca_sp)

obs_full_sp$robustness.deg <- c(robustness(ex_deg_mad_sp), robustness(ex_deg_nca_sp), robustness(ex_deg_mari_sp), robustness(ex_deg_haw_sp), robustness(ex_deg_vir_sp))

#Robustness null models
deg_vir_sp_null <- lapply(rand_vir_sp, second.extinct, participant = "lower", method="degree", nrep=10, details=FALSE)
deg_mari_sp_null <- lapply(rand_mari_sp, second.extinct, participant = "lower", method="degree", nrep=10, details=FALSE)
deg_nca_sp_null <- lapply(rand_nca_sp, second.extinct, participant = "lower", method="degree", nrep=10, details=FALSE)
deg_haw_sp_null <- lapply(rand_haw_sp, second.extinct, participant = "lower", method="degree", nrep=10, details=FALSE)
deg_mad_sp_null <- lapply(rand_mad_sp, second.extinct, participant = "lower", method="degree", nrep=10, details=FALSE)

null_sp_full$robustness.deg <- c(sapply(deg_mad_sp_null, function(x) robustness(x)),
                                 sapply(deg_nca_sp_null, function(x) robustness(x)),
                                 sapply(deg_mari_sp_null, function(x) robustness(x)),
                                 sapply(deg_haw_sp_null, function(x) robustness(x)),
                                 sapply(deg_vir_sp_null, function(x) robustness(x)))




#Tranform into long format
obs_full_sp_long <- gather(obs_full_sp, key = "metric",value ="metric_val", -site, -value)
null_sp_full_long <- gather (null_sp_full, key = "metric", value = "metric_val", -site, -value)


####5. Plots####

metric.labs <- c("Modularity","Nestedness(NODF)","Robustness", "Robustness.deg")
names(metric.labs) <- c("modularity", "NODF","robustness.ran", "robustness.deg")

#Structure
ggplot(data=subset(null_sp_full_long, metric %in% c("modularity", "NODF","robustness.ran")), aes(x=site, y=metric_val, fill=site)) +
  stat_summary(fun.data = mean_cl_boot, size=0.7, linetype="solid", geom="errorbar", color="black")  +
  geom_boxplot(data=subset(null_sp_full_long, metric %in% c("modularity", "NODF","robustness.ran")), outlier.shape = NA) +
  scale_fill_manual(values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  geom_point(data= subset(obs_full_sp_long, metric %in% c("modularity", "NODF","robustness.ran")), size=2, shape=8)+
  facet_grid(metric ~ ., scales="free", switch ="y", labeller = labeller(metric = metric.labs))+
  scale_x_discrete(breaks=c("haw","mad","mari","nca","vir"),
                   labels=c("Hawaii", "Madagascar", "Marshall Islands","New Caledonia","West Indies"))+
  ggtitle("Structure and Robustness_sp_BINARY_Curveball Class")+
  labs( y = "Value", x="") + 
  theme_bw()+
  theme(legend.position = "none",
        strip.text.x = element_text(size=10, face="bold"),
        panel.background = element_rect(fill = 'white')) 

write.csv(obs_full_sp, "output/network_sp_class no NA.csv")


####With another null model

#3. create randomized matrices --> constrains of margin sums
p <- 100

rand_vir_sp <- lapply(1:p, function(x) randomizeMatrix(pa_vir_sp, null.model = "trialswap"))
rand_mari_sp <- lapply(1:p, function(x) randomizeMatrix(pa_mari_sp, null.model = "trialswap"))
rand_nca_sp <- lapply(1:p, function(x) randomizeMatrix(pa_nca_sp, null.model = "trialswap"))
rand_mad_sp <- lapply(1:p, function(x) randomizeMatrix(pa_mad_sp, null.model = "trialswap"))
rand_haw_sp <- lapply(1:p, function(x) randomizeMatrix(pa_haw_sp, null.model = "trialswap"))

rowSums(pa_vir_sp)
rowSums(rand_vir_sp[[4]])

colSums(pa_vir_sp)
colSums(rand_vir_sp[[4]])


#bipartit shuffle::web --> keep Connectance =  number of interactions and number of links, but does'nt keep margins, and therefore not degree distribution
rand_vir_sp <- shuffle.web(pa_vir_sp, N=100)
rand_mari_sp <- shuffle.web(pa_mari_sp, N=100)
rand_nca_sp <- shuffle.web(pa_nca_sp, N=100)
rand_mad_sp <- shuffle.web(pa_mad_sp, N=100)
rand_haw_sp <- shuffle.web(pa_haw_sp, N=100)


#And keep weights so assign to different prey items
rand_vir_sp[[90]][,3][which(rand_vir_sp[[90]][,3] !=0)]
pa_vir_sp[,3][which(pa_vir_sp[,3] !=0)]


#Try with curveball algorithme of Giovanni --> keep row and col Sums, connectance and degree distribution
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

c_vir <- curve_ball(pa_vir_sp)

colSums(pa_vir_sp)
colSums(c_vir)

#same number of preys/pred
rand_vir_sp[[40]][,3][which(rand_vir_sp[[40]][,3] !=0)]
pa_vir_sp[,3][which(pa_vir_sp[,3] !=0)]

p <- 100
rand_vir_sp <- lapply(1:p, function(x) curve_ball(pa_vir_sp))
rand_mari_sp <- lapply(1:p, function(x) curve_ball(pa_mari_sp))
rand_nca_sp <- lapply(1:p, function(x) curve_ball(pa_nca_sp))
rand_mad_sp <- lapply(1:p, function(x) curve_ball(pa_mad_sp))
rand_haw_sp <- lapply(1:p, function(x) curve_ball(pa_haw_sp))



#Check which species and prey are the most connecte
library(igraph)
w <- list(vir = pa_vir_sp, mari = pa_mari_sp, nca = pa_nca_sp, mad = pa_mad_sp, haw = pa_haw_sp)
networks <- betalink::prepare_networks(w, directed = FALSE)
nb_int_vir <- sort(lengths(as_adj_list(networks$vir)), decreasing=TRUE) %>% data.frame(.) #Haemulidae and Decapoda
nb_int_mari <- sort(lengths(as_adj_list(networks$mari)), decreasing=TRUE) %>% data.frame(.) #Actinopterygii + Decapoda and Labridae + Pomacentridae
nb_int_mad <- sort(lengths(as_adj_list(networks$mad)), decreasing=TRUE) %>% data.frame(.) #Decapoda + Monacanthidae
nb_int_nca <- sort(lengths(as_adj_list(networks$nca)), decreasing=TRUE) %>% data.frame(.) #Actinopterygii + Decapoda and Lethrinidae
nb_int_haw <- sort(lengths(as_adj_list(networks$haw)), decreasing=TRUE) %>% data.frame(.) #Decapoda + Pomacentridae




