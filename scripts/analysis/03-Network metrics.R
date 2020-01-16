#################
#Network metrics#
#################

library(tidyverse)
library(bipartite)
library(vegan)
library(picante)
library(RColorBrewer)
library(wesanderson)

#1. Import  final matrices
vir_ISmatrix2 <- read.csv("data/vir_ISmatrix2.csv", sep=",", row.names = 1) %>%
  as.matrix(.)
mari_ISmatrix2 <- read.csv("data/mari_ISmatrix2.csv", sep=",", row.names = 1) %>%
  as.matrix(.)
nca_ISmatrix2 <- read.csv("data/nca_ISmatrix2.csv", sep=",", row.names = 1) %>%
  as.matrix(.)
haw_ISmatrix2 <- read.csv("data/haw_ISmatrix2.csv", sep=",", row.names = 1) %>%
  as.matrix(.)
mad_ISmatrix2 <- read.csv("data/mad_ISmatrix2.csv", sep=",", row.names = 1) %>%
  as.matrix(.)

dim(vir_ISmatrix2)

#####2. Get network indices#####
vir_ISmatrix2[which(vir_ISmatrix2 !=0)] #number of realized links 406
length(which(colSums(vir_ISmatrix2)!=0)) + length(which(rowSums(vir_ISmatrix2) !=0)) #number of nodes with interactions 81

mad_ISmatrix2[which(mad_ISmatrix2 !=0)] #number of realized links 347
length(which(colSums(mad_ISmatrix2)!=0)) + length(which(rowSums(mad_ISmatrix2) !=0)) #number of nodes with interactions 66

obs_vir <- unlist(networklevel(vir_ISmatrix2, empty.web = TRUE, index=c("number of species","connectance","weighted connectance","weighted NODF",
                                                      "nestedness","NODF", "linkage density",
                                                      "generality","vulnerability","niche overlap",
                                                      "links per species","mean number of shared partners")))

obs_mari <- unlist(networklevel(mari_ISmatrix2, empty.web = TRUE, index=c("number of species","connectance","weighted connectance","weighted NODF",
                                                                        "nestedness","NODF", "linkage density",
                                                                        "generality","vulnerability","niche overlap",
                                                                        "links per species","mean number of shared partners")))

obs_mad <- unlist(networklevel(mad_ISmatrix2, empty.web = TRUE, index=c("number of species","connectance","weighted connectance","weighted NODF",
                                                                        "nestedness","NODF", "linkage density",
                                                                        "generality","vulnerability","niche overlap",
                                                                        "links per species","mean number of shared partners")))

obs_haw <- unlist(networklevel(haw_ISmatrix2, empty.web = TRUE, index=c("number of species","connectance","weighted connectance","weighted NODF",
                                                                        "nestedness","NODF", "linkage density",
                                                                        "generality","vulnerability","niche overlap",
                                                                        "links per species","mean number of shared partners")))

obs_nca <- unlist(networklevel(nca_ISmatrix2, empty.web = TRUE, index=c("number of species","connectance","weighted connectance","weighted NODF",
                                                                        "nestedness","NODF", "linkage density",
                                                                        "generality","vulnerability","niche overlap",
                                                                        "links per species","mean number of shared partners")))
obs_vir <- data.frame(t(obs_vir))
obs_vir$site <-"vir"
obs_vir$value <- "obs"

obs_mari <- data.frame(t(obs_mari))
obs_mari$site <-"mari"
obs_mari$value <- "obs"

obs_mad <- data.frame(t(obs_mad))
obs_mad$site <-"mad"
obs_mad$value <- "obs"

obs_haw <- data.frame(t(obs_haw))
obs_haw$site <-"haw"
obs_haw$value <- "obs"

obs_nca <- data.frame(t(obs_nca))
obs_nca$site <-"nca"
obs_nca$value <- "obs"

obs_vir_spe <- unlist(specieslevel(vir_ISmatrix2, index=c("species strength"))) #Bascompte 2006 vs betweenness
plot(obs_vir_spe)

#Comparison with null models bipartite::swap.web --> Margin totals
#swap_nca_100, swap_vir_100, swap_mari_100, swap_mad_100, swap_haw_100 are the 1000 matrices from swap.web
nulls_vir <- unlist(sapply(swap_vir_100, networklevel, weighted = TRUE, empty.web = TRUE, index=c("connectance","weighted connectance","weighted NODF",
                                                                                 "NODF", "linkage density", "links per species",
                                                                                 "generality","vulnerability","niche overlap")))

nulls_mari <- unlist(sapply(swap_mari_100, networklevel,weighted = TRUE,  empty.web = TRUE, index=c("connectance","weighted connectance","weighted NODF",
                                                                                  "NODF", "linkage density", "links per species",
                                                                                  "generality","vulnerability","niche overlap")))
nulls_mad <- unlist(sapply(swap_mad_100, networklevel, weighted = TRUE, empty.web = TRUE, index=c("connectance","weighted connectance","weighted NODF",
                                                                                   "NODF", "linkage density", "links per species",
                                                                                  "generality","vulnerability","niche overlap")))
nulls_haw <- unlist(sapply(swap_haw_100, networklevel, weighted = TRUE,  empty.web = TRUE, index=c("connectance","weighted connectance","weighted NODF",
                                                                                   "NODF", "linkage density", "links per species",
                                                                                   "generality","vulnerability","niche overlap")))
nulls_nca <- unlist(sapply(swap_nca_100, networklevel, weighted = TRUE, empty.web = TRUE, index=c("connectance","weighted connectance","weighted NODF",
                                                                                   "NODF", "linkage density", "links per species",
                                                                                   "generality","vulnerability","niche overlap")))
######Get mean and IC from null models for each region : not good for plots####
mean_nulls <- function(x){
  r <- c(mean(x), mean(x)+qnorm(0.975)*sd(x)/sqrt(length(x)), mean(x)-qnorm(0.975)*sd(x)/sqrt(length(x)), sd(x)/sqrt(length(x)))
  names(r) <- c("mean","IC95","IC5","se")
  r
}

mean_nulls(nulls_vir[4,])
mean_nulls(rand_nulls_vir[4,])
mean_nulls(r2d_nulls_vir[4,])

net_vir <- apply(nulls_vir,1,FUN= mean_nulls) %>%
  data.frame(.) %>%
  plyr::rbind.fill(., obs_vir) %>%
  rownames_to_column(., var = "value") %>%
  mutate (value = case_when(value == "1" ~ "Mean",
                            value =="2" ~ "IC95",
                            value == "3" ~ "IC5",
                            value == "4"~ "se",
                            value =="5" ~ "obs"))
net_vir$site <- "vir"


net_mari <- apply(nulls_mari,1,FUN= mean_nulls) %>%
  data.frame(.) %>%
  plyr::rbind.fill(., obs_mari) %>%
  rownames_to_column(., var = "value") %>%
  mutate (value = case_when(value == "1" ~ "Mean",
                            value =="2" ~ "IC95",
                            value == "3" ~ "IC5",
                            value == "4"~ "se",
                            value =="5" ~ "obs"))
net_mari$site <- "mari"

net_mad <- apply(nulls_mad,1,FUN= mean_nulls) %>%
  data.frame(.) %>%
  plyr::rbind.fill(., obs_mad) %>%
rownames_to_column(., var = "value") %>%
  mutate (value = case_when(value == "1" ~ "Mean",
                            value =="2" ~ "IC95",
                            value == "3" ~ "IC5",
                            value == "4"~ "se",
                            value =="5" ~ "obs"))
net_mad$site <- "mad"

net_haw <- apply(nulls_haw,1,FUN= mean_nulls) %>%
  data.frame(.) %>%
  plyr::rbind.fill(., obs_haw) %>%
rownames_to_column(., var = "value") %>%
  mutate (value = case_when(value == "1" ~ "Mean",
                            value =="2" ~ "IC95",
                            value == "3" ~ "IC5",
                            value == "4"~ "se",
                            value =="5" ~ "obs"))
net_haw$site <- "haw"

net_nca <- apply(nulls_nca,1,FUN= mean_nulls) %>%
  data.frame(.) %>%
  plyr::rbind.fill(., obs_nca) %>%
  rownames_to_column(., var = "value") %>%
  mutate (value = case_when(value == "1" ~ "Mean",
                                value =="2" ~ "IC95",
                                value == "3" ~ "IC5",
                                value == "4"~ "se",
                            value =="5" ~ "obs")) 
net_nca$site <- "nca"


Znest <- (25.83542172-mean(nulls_vir[4,]))/sd(nulls_vir[4,]) #Zscore to have a standardized value of the null models' metrics

#Need a dataframe with all 
net_metrics <- rbind(net_vir, net_nca, net_mari, net_mad, net_haw) %>%
  filter(value =="Mean" | value =="obs" | value =="IC95" | value == "IC5")


ggplot(net_metrics, aes(x=site , y=weighted.NODF, shape=value)) +
  geom_point() 


####Try another df format####
df_nulls_haw <- t(nulls_haw) %>% data.frame(.) 
df_nulls_haw$site <- "haw"
df_nulls_haw$value <- "null"

df_nulls_mari <- t(nulls_mari) %>% data.frame(.) 
df_nulls_mari$site <- "mari"
df_nulls_mari$value <- "null"

df_nulls_mad <- t(nulls_mad) %>% data.frame(.) 
df_nulls_mad$site <- "mad"
df_nulls_mad$value <- "null"

df_nulls_nca <- t(nulls_nca) %>% data.frame(.) 
df_nulls_nca$site <- "nca"
df_nulls_nca$value <- "null"


df_nulls_vir <- t(nulls_vir) %>% data.frame(.) 
df_nulls_vir$site <- "vir"
df_nulls_vir$value <- "null"

 
null_full <- plyr::rbind.fill(df_nulls_mad, df_nulls_nca, df_nulls_mari, df_nulls_haw, df_nulls_vir) 

null_full_long <- gather(null_full, key="metric",value ="metric_val", -site, -value)

obs_full <- plyr::rbind.fill(obs_mad, obs_nca, obs_mari, obs_haw, obs_vir)


null_full_long_plot <- null_full_long %>% filter(metric == "connectance" | metric == "weighted.connectance" | metric=="linkage.density"| metric=="generality.HL"| metric == "vulnerability.LL" | metric =="weighted.NODF")

obs_full_long <- gather(obs_full, key="metric",value ="metric_val", -site, -value)

obs_full_long$site <- factor(obs_full_long$site, levels=c("vir","mad","nca","mari","haw")) #specify levels for plot order

obs_full_long_plot <- obs_full_long %>% filter(metric == "connectance" | metric == "weighted.connectance" | metric=="linkage.density"| metric=="generality.HL"| metric == "vulnerability.LL" | metric =="weighted.NODF")


ggplot(null_full_long_plot, aes(x=factor(site) , y=metric_val, fill=site)) +
  #stat_summary(fun.y = mean, geom="point",size=2) + 
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  geom_point(data = obs_full_long_plot, size=2, shape=8) +
  #stat_summary(fun.y = mean_IC, geom="point",size=2) + 
  #stat_summary(fun.data = mean_IC, geom ="errorbar", size=0.4, color="red") +
  facet_grid(metric ~., scales="free_y", switch="y") +
  labs( x = "Regions", y = "Index values") + 
  ggtitle("swap.web null model")+
  theme(legend.position="none")


  ggplot(obs_full_long_plot, mapping = aes(x=factor(site), y=metric_val), group=metric) +
  geom_point(aes(shape=metric), size=2.5) +
  geom_line(aes(group=metric), size=0.2)
  
  
#Some plots relation between obs values
ggplot(data=obs_full, mapping = aes(x=connectance, y=weighted.NODF), shape=site) + 
    geom_point(aes(shape=site), size=2.5) +
    geom_line(size=0.2) +
    labs(y="WNODF", x="connectance")


ggplot(data=obs_full, mapping = aes(x=weighted.connectance, y=weighted.NODF), shape=site) + 
  geom_point(aes(shape=site), size=2.5) +
  geom_line(size=0.2) +
  labs(y="NODF", x="connectance.w") #less clear

ggplot(data=obs_full, mapping = aes(x=connectance, y=modularity), shape=site) + 
  geom_point(aes(shape=site), size=2.5) +
  geom_line(size=0.2) +
  labs(y="Modularity", x="connectance") #less clear


ggplot(data=obs_full, mapping = aes(x=robustness.ran, y=modularity), shape=site) + 
  geom_point(aes(shape=site), size=2.5) +
  geom_line(size=0.2) +
  labs(y="Modularity", x="Robustness") #less clear

ggplot(data=obs_full, mapping = aes(x=weighted.NODF, y=modularity), shape=site) + 
  geom_point(aes(shape=site), size=2.5) +
  geom_line(size=0.2) +
  labs(y="Modularity", x="Nestedness") #less clear

names(obs_full)


##Try facetting metrics for each null models    
####Picante randomizedMatrix --> contrains only on column sums####
rand_nulls_vir <- unlist(sapply(rand_vir, networklevel, weighted=TRUE, empty.web = TRUE, index=c("connectance","weighted connectance","weighted NODF",
                                                                                    "NODF", "linkage density", "links per species",
                                                                                    "generality","vulnerability","niche overlap")))

rand_nulls_mari <- unlist(sapply(rand_mari, networklevel, weighted = TRUE, empty.web = TRUE, index=c("connectance","weighted connectance","weighted NODF",
                                                                                  "NODF", "linkage density", "links per species",
                                                                                  "generality","vulnerability","niche overlap")))

rand_nulls_mad <- unlist(sapply(rand_mad, networklevel, weighted = TRUE, empty.web = TRUE, index=c("connectance","weighted connectance","weighted NODF",
                                                                                                     "NODF", "linkage density", "links per species",
                                                                                                     "generality","vulnerability","niche overlap"))) 

rand_nulls_haw <- unlist(sapply(rand_haw, networklevel, weighted = TRUE, empty.web = TRUE, index=c("connectance","weighted connectance","weighted NODF",
                                                                                                     "NODF", "linkage density", "links per species",
                                                                                                     "generality","vulnerability","niche overlap")))

rand_nulls_nca <- unlist(sapply(rand_nca, networklevel, weighted = TRUE, empty.web = TRUE, index=c("connectance","weighted connectance","weighted NODF",
                                                                                                   "NODF", "linkage density", "links per species",
                                                                                                   "generality","vulnerability","niche overlap")))
#Dataframe with random distributions
df_rand_haw <- t(rand_nulls_haw) %>% data.frame(.)
df_rand_haw$site <- "haw"
df_rand_haw$value <- "null"

df_rand_mari <- t(rand_nulls_mari) %>% data.frame(.) 
df_rand_mari$site <- "mari"
df_rand_mari$value <- "null"

df_rand_mad <- t(rand_nulls_mad) %>% data.frame(.) 
df_rand_mad$site <- "mad"
df_rand_mad$value <- "null"

df_rand_nca <- t(rand_nulls_nca) %>% data.frame(.) 
df_rand_nca$site <- "nca"
df_rand_nca$value <- "null"


df_rand_vir <- t(rand_nulls_vir) %>% data.frame(.) 
df_rand_vir$site <- "vir"
df_rand_vir$value <- "null"


rand_null_full <- plyr::rbind.fill(df_rand_mad, df_rand_nca, df_rand_mari, df_rand_haw, df_rand_vir)
rand_null_full_long_plot <- gather(rand_null_full, key="metric",value ="metric_val", -site, -value) %>% 
  filter(metric == "connectance" | metric == "weighted.connectance" | metric=="linkage.density"| metric=="generality.HL"| metric == "vulnerability.LL" | metric =="weighted.NODF")

ggplot(rand_null_full_long_plot, aes(x=factor(site) , y=metric_val, fill=site)) +
  #stat_summary(fun.y = mean, geom="point",size=2) + 
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  geom_point(data = obs_full_long_plot, size=2, shape=8) +
  #stat_summary(fun.y = mean_IC, geom="point",size=2) + 
  #stat_summary(fun.data = mean_IC, geom ="errorbar", size=0.4, color="red") +
  facet_grid(metric ~., scales="free_y", switch="y") +
  labs( x = "Regions", y = "Index values") + 
  ggtitle("picante_fequency null model")+
  theme(legend.position="none")     


#####bipartite::r2dtable --> constrains on margins totals so both total of rows and colums####
rowSums(r2d_vir_100[[867]]) ; colSums(r2d_vir_100[[867]])
rowSums(vir_ISmatrix2) ; colSums(vir_ISmatrix2)

r2d_nulls_vir <- unlist(sapply(r2d_vir_100, networklevel, weighted = TRUE, empty.web = TRUE, index=c("connectance","weighted connectance","weighted NODF",
                                                                                    "NODF", "linkage density", "links per species",
                                                                                    "generality","vulnerability","niche overlap")))


r2d_nulls_mari <- unlist(sapply(r2d_mari_100, networklevel, weighted = TRUE, empty.web = TRUE, index=c("connectance","weighted connectance","weighted NODF",
                                                                                                     "NODF", "linkage density", "links per species",
                                                                                                     "generality","vulnerability","niche overlap")))

r2d_nulls_mad <- unlist(sapply(r2d_mad_100, networklevel, weighted = TRUE, empty.web = TRUE, index=c("connectance","weighted connectance","weighted NODF",
                                                                                                     "NODF", "linkage density", "links per species",
                                                                                                     "generality","vulnerability","niche overlap")))

r2d_nulls_haw <- unlist(sapply(r2d_haw_100, networklevel, weighted = TRUE, empty.web = TRUE, index=c("connectance","weighted connectance","weighted NODF",
                                                                                                     "NODF", "linkage density", "links per species",
                                                                                                     "generality","vulnerability","niche overlap")))

r2d_nulls_nca <- unlist(sapply(r2d_nca_100, networklevel, weighted = TRUE, empty.web = TRUE, index=c("connectance","weighted connectance","weighted NODF",
                                                                                                     "NODF", "linkage density", "links per species",
                                                                                                     "generality","vulnerability","niche overlap")))                                                                                                  
                                
#Dataframe with random distributions
df_r2d_haw <- t(r2d_nulls_haw) %>% data.frame(.)
df_r2d_haw$site <- "haw"
df_r2d_haw$value <- "null"

df_r2d_mari <- t(r2d_nulls_mari) %>% data.frame(.) 
df_r2d_mari$site <- "mari"
df_r2d_mari$value <- "null"

df_r2d_mad <- t(r2d_nulls_mad) %>% data.frame(.) 
df_r2d_mad$site <- "mad"
df_r2d_mad$value <- "null"

df_r2d_nca <- t(r2d_nulls_nca) %>% data.frame(.) 
df_r2d_nca$site <- "nca"
df_r2d_nca$value <- "null"


df_r2d_vir <- t(r2d_nulls_vir) %>% data.frame(.) 
df_r2d_vir$site <- "vir"
df_r2d_vir$value <- "null"


r2d_null_full <- plyr::rbind.fill(df_r2d_mad, df_r2d_nca, df_r2d_mari, df_r2d_haw, df_r2d_vir)
r2d_null_full_long_plot <- gather(r2d_null_full, key="metric",value ="metric_val", -site, -value) %>% 
  filter(metric == "connectance" | metric == "weighted.connectance" | metric=="linkage.density"| metric=="generality.HL"| metric == "vulnerability.LL" | metric =="weighted.NODF")

ggplot(r2d_null_full_long_plot, aes(x=factor(site) , y=metric_val, fill=site)) +
  #stat_summary(fun.y = mean, geom="point",size=2) + 
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  geom_point(data = obs_full_long_plot, size=2, shape=8) +
  #stat_summary(fun.y = mean_IC, geom="point",size=2) + 
  #stat_summary(fun.data = mean_IC, geom ="errorbar", size=0.4, color="red") +
  facet_grid(metric ~., scales="free_y", switch="y") +
  labs( x = "Regions", y = "Index values") + 
  ggtitle("r2dtable null model")+
  theme(legend.position="none")     

                                                                                                                    
#Contribution to each species to nestedness, but convert into binary matrices
nest_cont <- nestedcontribution(vir_ISmatrix2, nsimul=10) #returns Zscore from comparisons with null model randomized interactions for each nodes

nest_cont_higher <- as.data.frame(nest_cont[1])
nest_cont_higher$node <- rownames(nest_cont[[1]])

nest_cont_higher[which(nest_cont_higher$nestedcontribution>0),]


#Degree disitribution
degreedistr(vir_ISmatrix2, plot.it = TRUE) #skewed towards weak links
degreedistr(nca_ISmatrix2, plot.it = TRUE)

######Modularity######
data("Safariland")
NOS(Safariland)
NOS(vir.matrix.fam) #just NA's.... seems to want count data 
NOS(vir_ISmatrix2) #even *100 doesn't work

#With Becket's algorithm
mod_vir <- computeModules(vir_ISmatrix2, method="Beckett", forceLPA = FALSE) #DIRT algorithm from Becket 2016
mod_vir_meta <- metaComputeModules(vir_ISmatrix2, N=30, method="Beckett") #run the algorithm several times and return the most modular
plotModuleWeb(mod_vir, plotModules = TRUE, rank = FALSE, weighted = TRUE, displayAlabels = TRUE, 
              displayBlabels = TRUE, labsize = 1, xlabel = "", ylabel = "",
              square.border = "white", fromDepth = 0, upToDepth = -1)
mod_vir_meta@likelihood #need @ to access the value

mod_vir@modules[-1, -c(1,2)]

mod_mari <- computeModules(mari_ISmatrix2, method="Beckett", forceLPA = FALSE) #DIRT algorithm from Becket 2016
mod_mari_meta <- metaComputeModules(mari_ISmatrix2, N=30, method="Beckett") #run the algorithm several times and return the most modular
plotModuleWeb(mod_mari_meta, plotModules = TRUE, rank = FALSE, weighted = TRUE, displayAlabels = TRUE, 
              displayBlabels = TRUE, labsize = 1, xlabel = "", ylabel = "",
              square.border = "white", fromDepth = 0, upToDepth = -1)
mod_mari_meta@likelihood #need @ to access the value

mod_mad <- computeModules(mad_ISmatrix2, method="Beckett", forceLPA = FALSE) #DIRT algorithm from Becket 2016
mod_mad_meta <- metaComputeModules(mad_ISmatrix2, N=30, method="Beckett") #run the algorithm several times and return the most modular
plotModuleWeb(mod_mad, plotModules = TRUE, rank = FALSE, weighted = TRUE, displayAlabels = TRUE, 
              displayBlabels = TRUE, labsize = 1, xlabel = "", ylabel = "",
              square.border = "white", fromDepth = 0, upToDepth = -1)
mod_mad@likelihood #need @ to access the value


mod_haw <- computeModules(haw_ISmatrix2, method="Beckett", forceLPA = FALSE) #DIRT algorithm from Becket 2016
mod_haw_meta <- metaComputeModules(haw_ISmatrix2, N=30, method="Beckett") #run the algorithm several times and return the most modular
plotModuleWeb(mod_haw_meta, plotModules = TRUE, rank = FALSE, weighted = TRUE, displayAlabels = TRUE, 
              displayBlabels = TRUE, labsize = 1, xlabel = "", ylabel = "",
              square.border = "white", fromDepth = 0, upToDepth = -1)
mod_haw_meta@likelihood #need @ to access the value

mod_nca <- computeModules(nca_ISmatrix2, method="Beckett", forceLPA = FALSE) #DIRT algorithm from Becket 2016
mod_nca_meta <- metaComputeModules(nca_ISmatrix2, N=30, method="Beckett") #run the algorithm several times and return the most modular
plotModuleWeb(mod_nca, plotModules = TRUE, rank = FALSE, weighted = TRUE, displayAlabels = TRUE, 
              displayBlabels = TRUE, labsize = 1, xlabel = "", ylabel = "",
              square.border = "white", fromDepth = 0, upToDepth = -1)
mod_nca@likelihood #need @ to access the value

#Create and add rows with modularity values
obs_mod <- data.frame(metric_val = c(mod_haw@likelihood, mod_vir@likelihood, mod_mari@likelihood, mod_nca@likelihood, mod_mad@likelihood),
                      site = c("haw","vir",'mari',"nca","mad"),
                      value = "obs",
                      metric = "modularity")
                      
obs_full_long_plot <- obs_full_long_plot %>% rbind(obs_full_long_plot, obs_mod)

#Modularity of null distributions
  #swap_web
mod_null_vir <- unlist(sapply(swap_vir_100, computeModules, method="Beckett", forceLPA = TRUE)) #LPA for faster algorithme
mod_null_vir[[1]]@likelihood
sapply(mod_null_vir, function(x) x@likelihood) #to extract the whole 1000 values

mod_null_mari <- unlist(sapply(swap_mari_100, computeModules, method="Beckett",forceLPA = TRUE))
mod_null_mad <- unlist(sapply(swap_mad_100, computeModules, method="Beckett", forceLPA = TRUE))
mod_null_haw <- unlist(sapply(swap_haw_100, computeModules, method="Beckett", forceLPA = TRUE))
mod_null_nca <- unlist(sapply(swap_nca_100, computeModules, method="Beckett", forceLPA = TRUE))

null_mod <- data.frame(metric_val = c(sapply(mod_null_haw, function(x) x@likelihood),
                                      sapply(mod_null_vir, function(x) x@likelihood), 
                                      sapply(mod_null_mari, function(x) x@likelihood),
                                      sapply(mod_null_nca, function(x) x@likelihood),
                                      sapply(mod_null_mad, function(x) x@likelihood)),
                      site = rep(c("haw","vir",'mari',"nca","mad"), each = 1000),
                      value = "null",
                      metric = "modularity")

plotModuleWeb(mod_null_haw[[1]])
plotModuleWeb(mod_null_haw[[865]])

#null_full_long_plot <- null_full_long_plot %>% rbind(null_full_long_plot, null_mod)
unique(obs_full_long_plot$metric)

#Plot
#full
ggplot(null_full_long_plot, aes(x=factor(site) , y=metric_val, fill=site)) +
  #stat_summary(fun.y = mean, geom="point",size=2) + 
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  geom_point(data = obs_full_long_plot, size=2, shape=8) +
  #stat_summary(fun.y = mean_IC, geom="point",size=2) + 
  #stat_summary(fun.data = mean_IC, geom ="errorbar", size=0.4, color="red") +
  facet_grid(metric ~., scales="free_y", switch="y") +
  labs( x = "Regions", y = "Index values") + 
  ggtitle("swap.web null model")+
  theme(legend.position="none")

#just modularity
ggplot(null_mod, aes(x=factor(site) , y=metric_val, fill=site)) +
  #stat_summary(fun.y = mean, geom="point",size=2, color="red") + 
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  geom_point(data = obs_mod, size=2, shape=8) +
  stat_summary(fun.data = mean_IC, geom ="errorbar", size=0.4, color="red") +
  facet_grid(metric ~., scales="free_y", switch="y") +
  labs( x = "Regions", y = "Index values") + 
  ggtitle("swap.web null model")+
  theme(legend.position="none")


plotModuleWeb(computeModules(Safariland, method="Beckett"))

  #rand_picante frequency
mod_rand_vir <- unlist(sapply(rand_vir, computeModules, method="Beckett", forceLPA = TRUE))
mod_rand_mari <- unlist(sapply(rand_mari, computeModules, method="Beckett", forceLPA = TRUE))
mod_rand_mad <- unlist(sapply(rand_mad, computeModules, method="Beckett", forceLPA = TRUE))
mod_rand_haw <- unlist(sapply(rand_haw, computeModules, method="Beckett", forceLPA = TRUE))
mod_rand_nca <- unlist(sapply(rand_nca, computeModules, method="Beckett", forceLPA = TRUE))

rand_mod <- data.frame(metric_val = c(sapply(mod_rand_haw, function(x) x@likelihood),
                                      sapply(mod_rand_vir, function(x) x@likelihood), 
                                      sapply(mod_rand_mari, function(x) x@likelihood),
                                      sapply(mod_rand_nca, function(x) x@likelihood),
                                      sapply(mod_rand_mad, function(x) x@likelihood)),
                       site = rep(c("haw","vir",'mari',"nca","mad"), each = 1000),
                       value = "null",
                       metric = "modularity")

plotModuleWeb(mod_rand_vir[[1]])

#rand_null_full_long_plot <- rand_null_full_long_plot %>% rbind(rand_null_full_long_plot, rand_mod)

ggplot(rand_mod, aes(x=factor(site) , y=metric_val, fill=site)) +
  #stat_summary(fun.y = mean, geom="point",size=2, color="red") + 
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  geom_point(data = obs_mod, size=2, shape=8) +
  stat_summary(fun.data = mean_IC, geom ="errorbar", size=0.4, color="red") +
  facet_grid(metric ~., scales="free_y", switch="y") +
  labs( x = "Regions", y = "Index values") + 
  ggtitle("rand_picante freq null model")+
  theme(legend.position="none")


  #r2dtable
colSums(r2d_vir_100[[1]]) 
rowSums(r2d_vir_100[[1]])
colSums(vir_ISmatrix2)
rowSums(vir_ISmatrix2)

mod_r2d_vir <- unlist(sapply(r2d_vir_100, computeModules, method="Beckett", forceLPA = TRUE))
mod_r2d_mari <- unlist(sapply(r2d_mari_100, computeModules, method="Beckett", forceLPA = TRUE))
mod_r2d_mad <- unlist(sapply(r2d_mad_100, computeModules, method="Beckett", forceLPA = TRUE))
mod_r2d_haw <- unlist(sapply(r2d_haw_100, computeModules, method="Beckett", forceLPA = TRUE))
mod_r2d_nca <- unlist(sapply(r2d_nca_100, computeModules, method="Beckett", forceLPA = TRUE))


r2d_mod <- data.frame(metric_val = c(sapply(mod_r2d_haw, function(x) x@likelihood),
                                      sapply(mod_r2d_vir, function(x) x@likelihood), 
                                      sapply(mod_r2d_mari, function(x) x@likelihood),
                                      sapply(mod_r2d_nca, function(x) x@likelihood),
                                      sapply(mod_r2d_mad, function(x) x@likelihood)),
                       site = rep(c("haw","vir",'mari',"nca","mad"), each = 1000),
                       value = "null",
                       metric = "modularity")

plotModuleWeb(mod_r2d_vir[[856]])

#r2d_null_full_long_plot <- r2d_null_full_long_plot %>% rbind(r2d_null_full_long_plot, r2d_mod)

ggplot(r2d_mod, aes(x=factor(site) , y=metric_val, fill=site)) +
  #stat_summary(fun.y = mean, geom="point",size=2, color="red") + 
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  geom_point(data = obs_mod, size=2, shape=8) +
  stat_summary(fun.data = mean_IC, geom ="errorbar", size=0.4, color="red") +
  facet_grid(metric ~., scales="free_y", switch="y") +
  labs( x = "Regions", y = "Index values") + 
  ggtitle("r2dtable null model")+
  theme(legend.position="none")

dev.off()


####Second extinction and robustness ##### 
#either random or from the more to the less connected. Extinctions of lower group
#Virgin
ex_deg_vir <-second.extinct(vir_ISmatrix2, participant = "lower", method="degree", nrep=1000, details=TRUE)
plot(ex_deg_vir)
plot(ex_deg_vir[,3] ~ ex_deg_vir[,1])
sum(ex_deg_vir[,3]) #84 familles
robustness(ex_deg_vir)
slope.bipartite(ex_deg_vir)

ex_ran_vir<-second.extinct(vir_ISmatrix2, participant = "lower", method="random", nrep=1000, details=FALSE)
plot(ex_ran_vir[,3] ~ ex_ran_vir[,1])
robustness(ex_ran_vir)
slope.bipartite(ex_ran_vir)

#Mari
ex_deg_mari <-second.extinct(mari_ISmatrix2, participant = "lower", method="degree", nrep=1000, details=TRUE)
plot(ex_deg_mari)
plot(ex_deg_mari[,3] ~ ex_deg_mari[,1])
robustness(ex_deg_mari)

ex_ran_mari<-second.extinct(mari_ISmatrix2, participant = "lower", method="random", nrep=1000, details=FALSE)
plot(ex_ran_mari[,3] ~ ex_ran_mari[,1])
robustness(ex_ran_mari)

#Mad
ex_deg_mad <-second.extinct(mad_ISmatrix2, participant = "lower", method="degree", nrep=1000, details=TRUE)
plot(ex_deg_mad)
plot(ex_deg_mad[,3] ~ ex_deg_mad[,1])
robustness(ex_deg_mad)

ex_ran_mad<-second.extinct(mad_ISmatrix2, participant = "lower", method="random", nrep=1000, details=FALSE)
plot(ex_ran_mad[,3] ~ ex_ran_mad[,1])
robustness(ex_ran_mad)

#Haw
ex_deg_haw <-second.extinct(haw_ISmatrix2, participant = "lower", method="degree", nrep=1000, details=TRUE)
plot(ex_deg_haw)
plot(ex_deg_haw[,3] ~ ex_deg_haw[,1])
robustness(ex_deg_haw)

ex_ran_haw <-second.extinct(haw_ISmatrix2, participant = "lower", method="random", nrep=1000, details=FALSE)
plot(ex_ran_haw[,3] ~ ex_ran_haw[,1])
robustness(ex_ran_haw)

#Nca
ex_deg_nca <-second.extinct(nca_ISmatrix2, participant = "lower", method="degree", nrep=1000, details=TRUE)
plot(ex_deg_nca)
plot(ex_deg_nca[,3] ~ ex_deg_nca[,1])
robustness(ex_deg_nca)

ex_ran_nca <-second.extinct(nca_ISmatrix2, participant = "lower", method="random", nrep=1000, details=FALSE)
plot(ex_ran_nca[,3] ~ ex_ran_nca[,1])
robustness(ex_ran_nca)

obs_rob <- data.frame(metric_val = c(robustness(ex_ran_haw), robustness(ex_ran_vir), robustness(ex_ran_mari), robustness(ex_ran_nca), robustness(ex_ran_mad),
                                    robustness(ex_deg_haw), robustness(ex_deg_vir), robustness(ex_deg_mari), robustness(ex_deg_nca), robustness(ex_deg_mad)),
                     site = c("haw","vir",'mari',"nca","mad","haw","vir",'mari',"nca","mad"),
                     ext = rep(c("ran","deg"),each=5),
                     value = "obs",
                     metric = "robustness") %>%
  mutate_at(vars(2:5), as.character)

#Plot of robustness
ggplot(data=obs_rob, mapping = aes(x=site, y=metric_val), group=ext) + 
  geom_point(aes(shape=ext), size=2.5) +
  geom_line(aes(group=ext), size=0.2) +
  labs(y="Robustness")

#Add robustness 
obs_full_long_plot <-  plyr::rbind.fill(obs_full_long_plot, obs_rob) %>%
  unique(.)

obs_full$robustness.ran <- c(robustness(ex_ran_mad), robustness(ex_ran_nca),robustness(ex_ran_mari),robustness(ex_ran_haw),robustness(ex_ran_vir))
obs_full$robustness.deg <- c(robustness(ex_deg_mad), robustness(ex_deg_nca),robustness(ex_deg_mari),robustness(ex_deg_haw),robustness(ex_deg_vir))
obs_full$modularity <- c(mod_mad@likelihood, mod_nca@likelihood,mod_mari@likelihood,mod_haw@likelihood,mod_vir@likelihood)

#Some plots
ggplot(data=obs_full, mapping = aes(x=connectance, y=robustness.ran), shape=site) + 
  geom_point(aes(shape=site), size=2.5) +
  geom_line(size=0.2) +
  labs(y="Robustness", x="connectance")

ggplot(data=obs_full, mapping = aes(x=modularity, y=robustness.ran), shape=site) + 
  geom_point(aes(shape=site), size=2.5) +
  geom_line(size=0.2) +
  labs(y="Robustness", x="Modularity")


ggplot(data=obs_full, mapping = aes(x=weighted.NODF, y=robustness.ran), shape=site) + 
  geom_point(aes(shape=site), size=2.5) +
  geom_line(size=0.2) +
  labs(y="Robustness", x="weighted.NODF")

#Robustness null model distributions
  #swap.web
rob_null_mari <- lapply(swap_mari_100, second.extinct, participant = "lower", method="random", nrep=1, details=FALSE)
robustness(rob_null_vir[[1]])
slope.bipartite(rob_null_vir[[1]])
class(rob_null_mari[[1]])
sapply(rob_null_mari, function(x) robustness(x)) #to get a vector of robustness value for each null model

rob_null_vir <- lapply(swap_vir_100, second.extinct, participant = "lower", method="random", nrep=1, details=FALSE)
rob_null_mad <- lapply(swap_mad_100, second.extinct, participant = "lower", method="random", nrep=1, details=FALSE)
rob_null_haw <- lapply(swap_haw_100, second.extinct, participant = "lower", method="random", nrep=1, details=FALSE)
rob_null_nca <- lapply(swap_nca_100, second.extinct, participant = "lower", method="random", nrep=1, details=FALSE)

rob_null_haw$robustness


null_rob <- data.frame(metric_val = c(sapply(rob_null_haw, function(x) robustness(x)),
                                     sapply(rob_null_vir, function(x) robustness(x)), 
                                     sapply(rob_null_mari, function(x) robustness(x)),
                                     sapply(rob_null_nca, function(x) robustness(x)),
                                     sapply(rob_null_mad, function(x) robustness(x))),
                      site = rep(c("haw","vir",'mari',"nca","mad"), each = 1000),
                      value = "null",
                      metric = "robustness")

ggplot(null_rob, aes(x=factor(site) , y=metric_val, fill=site)) +
  #stat_summary(fun.y = mean, geom="point",size=2, color="red") + 
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  geom_point(data = subset(obs_rob, ext %in% "ran"), size=2, shape=8) +
  stat_summary(fun.data = mean_IC, geom ="errorbar", size=0.4, color="red") +
  facet_grid(metric ~., scales="free_y", switch="y") +
  labs( x = "Regions", y = "Robustness") + 
  ggtitle("swap.web null model")+
  theme(legend.position="none")


  #rand
rob_rand_mari <- lapply(rand_mari, second.extinct, participant = "lower", method="random", nrep=1, details=FALSE)
rob_rand_vir <- lapply(rand_vir, second.extinct, participant = "lower", method="random", nrep=1, details=FALSE)
rob_rand_mad <- lapply(rand_mad, second.extinct, participant = "lower", method="random", nrep=1, details=FALSE)
rob_rand_haw <- lapply(rand_haw, second.extinct, participant = "lower", method="random", nrep=1, details=FALSE)
rob_rand_nca <- lapply(rand_nca, second.extinct, participant = "lower", method="random", nrep=1, details=FALSE)


rand_rob <- data.frame(metric_val = c(sapply(rob_rand_haw, function(x) robustness(x)),
                                      sapply(rob_rand_vir, function(x) robustness(x)), 
                                      sapply(rob_rand_mari, function(x) robustness(x)),
                                      sapply(rob_rand_nca, function(x) robustness(x)),
                                      sapply(rob_rand_mad, function(x) robustness(x))),
                       site = rep(c("haw","vir",'mari',"nca","mad"), each = 1000),
                       value = "rand",
                       metric = "robustness")

ggplot(rand_rob, aes(x=factor(site) , y=metric_val, fill=site)) +
  #stat_summary(fun.y = mean, geom="point",size=2, color="red") + 
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  geom_point(data = subset(obs_rob, ext %in% "ran"), size=2, shape=8) +
  stat_summary(fun.data = mean_IC, geom ="errorbar", size=0.4, color="red") +
  facet_grid(metric ~., scales="free_y", switch="y") +
  labs( x = "Regions", y = "Robustness") + 
  ggtitle("rand picante null model")+
  theme(legend.position="none")


  #r2dtable
rob_r2d_mari <- lapply(r2d_mari_100, second.extinct, participant = "lower", method="random", nrep=1, details=FALSE)
rob_r2d_vir <- lapply(r2d_vir_100, second.extinct, participant = "lower", method="random", nrep=1, details=FALSE)
rob_r2d_mad <- lapply(r2d_mad_100, second.extinct, participant = "lower", method="random", nrep=1, details=FALSE)
rob_r2d_haw <- lapply(r2d_haw_100, second.extinct, participant = "lower", method="random", nrep=1, details=FALSE)
rob_r2d_nca <- lapply(r2d_nca_100, second.extinct, participant = "lower", method="random", nrep=1, details=FALSE)


r2d_rob <- data.frame(metric_val = c(sapply(rob_r2d_haw, function(x) robustness(x)),
                                      sapply(rob_r2d_vir, function(x) robustness(x)), 
                                      sapply(rob_r2d_mari, function(x) robustness(x)),
                                      sapply(rob_r2d_nca, function(x) robustness(x)),
                                      sapply(rob_r2d_mad, function(x) robustness(x))),
                       site = rep(c("haw","vir",'mari',"nca","mad"), each = 1000),
                       value = "r2d",
                       metric = "robustness")


ggplot(r2d_rob, aes(x=factor(site) , y=metric_val, fill=site)) +
  #stat_summary(fun.y = mean, geom="point",size=2, color="red") + 
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  geom_point(data = subset(obs_rob, ext %in% "ran"), size=2, shape=8) +
  stat_summary(fun.data = mean_IC, geom ="errorbar", size=0.4, color="red") +
  facet_grid(metric ~., scales="free_y", switch="y") +
  labs( x = "Regions", y = "Robustness") + 
  ggtitle("r2d null model")+
  theme(legend.position="none")

####Combine df and make plots####
#Df with all null distributions
unique(null_full$site)

null_full$model <- "swap.web"  
null_full$modularity <- c(sapply(mod_null_mad, function(x) x@likelihood),
sapply(mod_null_nca, function(x) x@likelihood), 
sapply(mod_null_mari, function(x) x@likelihood),
sapply(mod_null_haw, function(x) x@likelihood),
sapply(mod_null_vir, function(x) x@likelihood))

  
null_full$robustness.ran <- c(sapply(rob_null_mad, function(x) robustness(x)),
sapply(rob_null_nca, function(x) robustness(x)), 
sapply(rob_null_mari, function(x) robustness(x)),
sapply(rob_null_haw, function(x) robustness(x)),
sapply(rob_null_vir, function(x) robustness(x)))


rand_null_full$model <- "rand.pic"  
rand_null_full$modularity <- c(sapply(mod_rand_mad, function(x) x@likelihood),
                          sapply(mod_rand_nca, function(x) x@likelihood), 
                          sapply(mod_rand_mari, function(x) x@likelihood),
                          sapply(mod_rand_haw, function(x) x@likelihood),
                          sapply(mod_rand_vir, function(x) x@likelihood))

rand_null_full$robustness.ran <- c(sapply(rob_rand_mad, function(x) robustness(x)),
                              sapply(rob_rand_nca, function(x) robustness(x)), 
                              sapply(rob_rand_mari, function(x) robustness(x)),
                              sapply(rob_rand_haw, function(x) robustness(x)),
                              sapply(rob_rand_vir, function(x) robustness(x)))

r2d_null_full$model <- "r2dtable"  
r2d_null_full$modularity <- c(sapply(mod_r2d_mad, function(x) x@likelihood),
                          sapply(mod_r2d_nca, function(x) x@likelihood), 
                          sapply(mod_r2d_mari, function(x) x@likelihood),
                          sapply(mod_r2d_haw, function(x) x@likelihood),
                          sapply(mod_r2d_vir, function(x) x@likelihood))

r2d_null_full$robustness.ran <- c(sapply(rob_r2d_mad, function(x) robustness(x)),
                              sapply(rob_r2d_nca, function(x) robustness(x)), 
                              sapply(rob_r2d_mari, function(x) robustness(x)),
                              sapply(rob_r2d_haw, function(x) robustness(x)),
                              sapply(rob_r2d_vir, function(x) robustness(x)))



all_index_modelnull <- rbind(null_full, rand_null_full, r2d_null_full)
all_index_modelnull_long <- gather(all_index_modelnull, key="metric",value ="metric_val", -site, -value, -model)

obs_full_long <- gather(obs_full, key="metric",value ="metric_val", -site, -value)


unique(all_index_modelnull_long$metric)
unique(obs_full_long$metric)

#Plot complexity and strategy
ggplot(data=all_index_modelnull, aes(x=site, y=metric_val, fill=site))+
geom_boxplot(data=subset(all_index_modelnull_long, metric %in% c("weighted.connectance","linkage.density","generality.HL","vulnerability.LL")), outlier.shape = NA) +
  scale_fill_manual(values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  geom_point(data= subset(obs_full_long,metric %in% c("weighted.connectance","linkage.density","generality.HL","vulnerability.LL")), size=2, shape=8)+
  
  facet_grid(metric ~ model, scales="free_y", switch="y")+
  ggtitle("Complexity and Asymmetry")

#Plot Structure and robustness
ggplot(data=all_index_modelnull, aes(x=site, y=metric_val, fill=site))+
  geom_boxplot(data=subset(all_index_modelnull_long, metric %in% c("weighted.NODF", "modularity","robustness.ran")), outlier.shape = NA) +
  scale_fill_manual(values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  geom_point(data= subset(obs_full_long,metric %in% c("weighted.NODF", "modularity","robustness.ran")), size=2, shape=8)+
  facet_grid(metric ~ model, scales="free_y", switch="y") +
  ggtitle("Structure and Robustness")


####Test with another null model picante::trialswap######
#Test with other null model on modularity
#trial swap" originaly for presence/absence data
rand_vir2 <- lapply(1:100, function(x) randomizeMatrix(vir_ISmatrix2, null.model = "trialswap")) #keep row and cols as in observed matrix
rand_mari2 <- lapply(1:100, function(x) randomizeMatrix(mari_ISmatrix2, null.model = "trialswap")) #keep row and cols as in observed matrix
rand_nca2 <- lapply(1:100, function(x) randomizeMatrix(nca_ISmatrix2, null.model = "trialswap")) #keep row and cols as in observed matrix
rand_haw2 <- lapply(1:100, function(x) randomizeMatrix(haw_ISmatrix2, null.model = "trialswap")) #keep row and cols as in observed matrix
rand_mad2 <- lapply(1:100, function(x) randomizeMatrix(mad_ISmatrix2, null.model = "trialswap")) #keep row and cols as in observed matrix

rowSums(rand_haw2[[99]])
rowSums(haw_ISmatrix2)


mod_rand2_mari <- unlist(sapply(rand_mari2, computeModules, method="Beckett",forceLPA = TRUE))
mod_rand2_vir <- unlist(sapply(rand_vir2, computeModules, method="Beckett",forceLPA = TRUE))
mod_rand2_mad <- unlist(sapply(rand_mad2, computeModules, method="Beckett", forceLPA = TRUE))
mod_rand2_haw <- unlist(sapply(rand_haw2, computeModules, method="Beckett", forceLPA = TRUE))
mod_rand2_nca <- unlist(sapply(rand_nca2, computeModules, method="Beckett", forceLPA = TRUE))

rand2_mod <- data.frame(metric_val = c(sapply(mod_rand2_haw, function(x) x@likelihood),
                                       sapply(mod_rand2_vir, function(x) x@likelihood), 
                                       sapply(mod_rand2_mari, function(x) x@likelihood),
                                       sapply(mod_rand2_nca, function(x) x@likelihood),
                                       sapply(mod_rand2_mad, function(x) x@likelihood)),
                        site = rep(c("haw","vir",'mari',"nca","mad"), each = 100),
                        value = "rand2",
                        metric = "modularity")

#just modularity
ggplot(rand2_mod, aes(x=factor(site) , y=metric_val, fill=site)) +
  #stat_summary(fun.y = mean, geom="point",size=2, color="red") + 
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  geom_point(data = obs_mod, size=2, shape=8) +
  stat_summary(fun.data = mean_IC, geom ="errorbar", size=0.4, color="red") +
  facet_grid(metric ~., scales="free_y", switch="y") +
  labs( x = "Regions", y = "Index values") + 
  ggtitle("rand.trialswap null model")+
  theme(legend.position="none")


z <- (obs_mod$metric_val[obs_mod$site =="haw"] - mean(sapply(mod_rand2_haw, function(x) x@likelihood))) / sd(sapply(mod_rand2_haw, function(x) x@likelihood))
z
z <- (obs_mod$metric_val[obs_mod$site =="haw"] - mean(sapply(mod_rand_haw, function(x) x@likelihood))) / sd(sapply(mod_rand_haw, function(x) x@likelihood))
z


#For other metrics
nest_rand2_vir <- unlist(sapply(rand_vir2, networklevel, weighted = TRUE, empty.web = TRUE, index=c("connectance","weighted connectance","weighted NODF"
                                                                                                    ,"linkage density", "generality","vulnerability")))

nest_rand2_mari <- unlist(sapply(rand_mari2, networklevel, weighted = TRUE, empty.web = TRUE, index=c("connectance","weighted connectance","weighted NODF"
                                                                                                      ,"linkage density", "generality","vulnerability")))

nest_rand2_mad <- unlist(sapply(rand_mad2, networklevel, weighted = TRUE, empty.web = TRUE, index=c("connectance","weighted connectance","weighted NODF"
                                                                                                    ,"linkage density", "generality","vulnerability")))

nest_rand2_nca <- unlist(sapply(rand_nca2, networklevel, weighted = TRUE, empty.web = TRUE, index=c("connectance","weighted connectance","weighted NODF"
                                                                                                    ,"linkage density", "generality","vulnerability")))

nest_rand2_haw <- unlist(sapply(rand_haw2, networklevel, weighted = TRUE, empty.web = TRUE, index=c("connectance","weighted connectance","weighted NODF"
                                                                                                    ,"linkage density", "generality","vulnerability")))


#Dataframe with random distributions
df_rand2_haw <- t(nest_rand2_haw) %>% data.frame(.)
df_rand2_haw$site <- "haw"
df_rand2_haw$value <- "null"

df_rand2_mari <- t(nest_rand2_mari) %>% data.frame(.) 
df_rand2_mari$site <- "mari"
df_rand2_mari$value <- "null"

df_rand2_mad <- t(nest_rand2_mad) %>% data.frame(.) 
df_rand2_mad$site <- "mad"
df_rand2_mad$value <- "null"

df_rand2_nca <- t(nest_rand2_nca) %>% data.frame(.) 
df_rand2_nca$site <- "nca"
df_rand2_nca$value <- "null"


df_rand2_vir <- t(nest_rand2_vir) %>% data.frame(.) 
df_rand2_vir$site <- "vir"
df_rand2_vir$value <- "null"



rand2_full <- plyr::rbind.fill(df_rand2_haw, df_rand2_mad, df_rand2_mari, df_rand2_nca, df_rand2_vir) %>% 
  left_join(rand2_mod[c(1,2)], by="site") %>%
  rename(modularity = metric_val)

rand2_full_long <- gather(rand2_full, key="metric",value ="metric_val", -site, -value)


ggplot(rand2_full_long, aes(x=factor(site) , y=metric_val, fill=site)) +
  #stat_summary(fun.y = mean, geom="point",size=2, color="red") + 
  geom_boxplot(data=subset(rand2_full_long, metric %in% c("weighted.connectance","linkage.density","generality.HL","vulnerability.LL","weighted.NODF")),outlier.shape = NA) +
  scale_fill_manual(values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  geom_point(data=subset(obs_full_long, metric %in% c("weighted.connectance","linkage.density","generality.HL","vulnerability.LL","weighted.NODF")), size=2, shape=8) +
  facet_grid(metric ~., scales="free_y", switch="y") +
  labs( x = "Regions", y = "Index values") + 
  ggtitle("rand.trialswap null model")+
  theme(legend.position="none")


unique(rand2_full_long$metric)

ggplot(data = subset(rand2_full_long, metric %in% c("weighted.NODF","modularity")), aes(x=factor(site) , y=metric_val, fill=site)) +
  #stat_summary(fun.y = mean, geom="point",size=2, color="red") + 
  geom_boxplot(data=subset(rand2_full_long, metric %in% c("weighted.NODF","modularity")),outlier.shape = NA) +
  scale_fill_manual(values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  geom_point(data=subset(obs_full_long, metric %in% c("weighted.NODF","modularity")), size=2, shape=8) +
  labs( x = "Regions", y = "Index values") +
  facet_grid(metric ~., scales="free_y", switch="y") +
  ggtitle("rand.trialswap null model")+
  theme(legend.position="none")







###Test with another null model picante::richness --> fixed rows ######
rand_row_vir <- lapply(1:1000, function(x) randomizeMatrix(vir_ISmatrix2, null.model = "richness")) #keep rows totals as in observed matrix
rand_row_mari <- lapply(1:1000, function(x) randomizeMatrix(mari_ISmatrix2, null.model = "richness")) 
rand_row_nca <- lapply(1:1000, function(x) randomizeMatrix(nca_ISmatrix2, null.model = "richness")) 
rand_row_haw <- lapply(1:1000, function(x) randomizeMatrix(haw_ISmatrix2, null.model = "richness")) 
rand_row_mad <- lapply(1:1000, function(x) randomizeMatrix(mad_ISmatrix2, null.model = "richness")) 

rowSums(rand_row_vir[[2]])
colSums(rand_row_vir[[2]])

#Basic indices
row_nulls_vir <- unlist(sapply(rand_row_vir, networklevel, weighted=TRUE, empty.web = TRUE, index=c("connectance","weighted connectance","weighted NODF",
                                                                                                    "NODF", "linkage density", "links per species",
                                                                                                    "generality","vulnerability","niche overlap")))

row_nulls_mari <- unlist(sapply(rand_row_mari, networklevel, weighted = TRUE, empty.web = TRUE, index=c("connectance","weighted connectance","weighted NODF",
                                                                                                        "NODF", "linkage density", "links per species",
                                                                                                        "generality","vulnerability","niche overlap")))

row_nulls_mad <- unlist(sapply(rand_row_mad, networklevel, weighted = TRUE, empty.web = TRUE, index=c("connectance","weighted connectance","weighted NODF",
                                                                                                      "NODF", "linkage density", "links per species",
                                                                                                      "generality","vulnerability","niche overlap")))

row_nulls_haw <- unlist(sapply(rand_row_haw, networklevel, weighted = TRUE, empty.web = TRUE, index=c("connectance","weighted connectance","weighted NODF",
                                                                                                      "NODF", "linkage density", "links per species",
                                                                                                      "generality","vulnerability","niche overlap")))

row_nulls_nca <- unlist(sapply(rand_row_nca, networklevel, weighted = TRUE, empty.web = TRUE, index=c("connectance","weighted connectance","weighted NODF",
                                                                                                      "NODF", "linkage density", "links per species",
                                                                                                      "generality","vulnerability","niche overlap")))

#Dataframe with random distributions
df_row_haw <- t(row_nulls_haw) %>% data.frame(.)
df_row_haw$site <- "haw"
df_row_haw$value <- "null"

df_row_mari <- t(row_nulls_mari) %>% data.frame(.) 
df_row_mari$site <- "mari"
df_row_mari$value <- "null"

df_row_mad <- t(row_nulls_mad) %>% data.frame(.) 
df_row_mad$site <- "mad"
df_row_mad$value <- "null"

df_row_nca <- t(row_nulls_nca) %>% data.frame(.) 
df_row_nca$site <- "nca"
df_row_nca$value <- "null"

df_row_vir <- t(row_nulls_vir) %>% data.frame(.) 
df_row_vir$site <- "vir"
df_row_vir$value <- "null"

row_null_full <- plyr::rbind.fill(df_row_mad, df_row_nca, df_row_mari, df_row_haw, df_row_vir) 

#Modularity
mod_row_mari <- unlist(sapply(rand_row_mari, computeModules, method="Beckett",forceLPA = TRUE))
mod_row_mad <- unlist(sapply(rand_row_mad, computeModules, method="Beckett", forceLPA = TRUE))
mod_row_haw <- unlist(sapply(rand_row_haw, computeModules, method="Beckett", forceLPA = TRUE))
mod_row_nca <- unlist(sapply(rand_row_nca, computeModules, method="Beckett", forceLPA = TRUE))
mod_row_vir <- unlist(sapply(rand_row_vir, computeModules, method="Beckett", forceLPA = TRUE))

row_mod <- data.frame(metric_val = c(sapply(mod_row_haw, function(x) x@likelihood),
                                      sapply(mod_row_vir, function(x) x@likelihood), 
                                      sapply(mod_row_mari, function(x) x@likelihood),
                                      sapply(mod_row_nca, function(x) x@likelihood),
                                      sapply(mod_row_mad, function(x) x@likelihood)),
                       site = rep(c("haw","vir",'mari',"nca","mad"), each = 1000),
                       value = "null",
                       metric = "modularity")



#Robustness
rob_row_mari <- lapply(rand_row_mari, second.extinct, participant = "lower", method="random", nrep=1, details=FALSE)
rob_row_vir <- lapply(rand_row_vir, second.extinct, participant = "lower", method="random", nrep=1, details=FALSE)
rob_row_mad <- lapply(rand_row_mad, second.extinct, participant = "lower", method="random", nrep=1, details=FALSE)
rob_row_haw <- lapply(rand_row_haw, second.extinct, participant = "lower", method="random", nrep=1, details=FALSE)
rob_row_nca <- lapply(rand_row_nca, second.extinct, participant = "lower", method="random", nrep=1, details=FALSE)


row_rob <- data.frame(metric_val = c(sapply(rob_row_haw, function(x) robustness(x)),
                                     sapply(rob_row_vir, function(x) robustness(x)), 
                                     sapply(rob_row_mari, function(x) robustness(x)),
                                     sapply(rob_row_nca, function(x) robustness(x)),
                                     sapply(rob_row_mad, function(x) robustness(x))),
                      site = rep(c("haw","vir",'mari',"nca","mad"), each = 1000),
                      value = "null",
                      metric = "robustness")


row_null_full$model <- "rand.row"  
row_null_full$modularity <- c(sapply(mod_row_mad, function(x) x@likelihood),
                               sapply(mod_row_nca, function(x) x@likelihood), 
                               sapply(mod_row_mari, function(x) x@likelihood),
                               sapply(mod_row_haw, function(x) x@likelihood),
                               sapply(mod_row_vir, function(x) x@likelihood))

row_null_full$robustness.ran <- c(sapply(rob_row_mad, function(x) robustness(x)),
                                   sapply(rob_row_nca, function(x) robustness(x)), 
                                   sapply(rob_row_mari, function(x) robustness(x)),
                                   sapply(rob_row_haw, function(x) robustness(x)),
                                   sapply(rob_row_vir, function(x) robustness(x)))


all_index_modelnull <- rbind(null_full, rand_null_full, r2d_null_full, row_null_full)
all_index_modelnull_long <- gather(all_index_modelnull, key="metric",value ="metric_val", -site, -value, -model)

row_col_null <- all_index_modelnull_long %>% filter(model =="rand.pic" | model=="rand.row" | model =="swap.web")

unique(all_index_modelnull_long$metric)
unique(row_col_null$metric)

#Plot
#Plot complexity and strategy
ggplot(data=subset(row_col_null, metric %in% c("connectance","weighted.connectance","linkage.density","generality.HL","vulnerability.LL") & model %in% "rand.pic"), aes(x=site, y=metric_val, fill=site))+
  #stat_summary(fun.data = mean_IC, geom ="errorbar", size=0.4, color="red") +
  geom_boxplot(data=subset(row_col_null, metric %in% c("connectance","weighted.connectance","linkage.density","generality.HL","vulnerability.LL") & model %in% "rand.pic"), outlier.shape = NA) +
  scale_fill_manual(values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  geom_point(data= subset(obs_full_long,metric %in% c("connectance", "weighted.connectance","linkage.density","generality.HL","vulnerability.LL")), size=2, shape=8)+
  facet_grid(metric ~ model, scales="free", switch ="y")+
  ggtitle("Complexity and Asymmetry_families") +
  labs( y = "Value", x="Sites") + 
  theme(legend.position = "none")

#Plot Structure and robustness
ggplot(data=subset(row_col_null, metric %in% c("weighted.NODF", "modularity","robustness.ran") & model %in% "rand.pic"), aes(x=site, y=metric_val, fill=site))+
  geom_boxplot(data=subset(row_col_null, metric %in% c("weighted.NODF", "modularity","robustness.ran") & model %in% "rand.pic"), outlier.shape = NA) +
  #stat_summary(fun.data = mean_IC, geom ="errorbar", size=0.4, color="red") +
  scale_fill_manual(values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  geom_point(data= subset(obs_full_long,metric %in% c("weighted.NODF", "modularity","robustness.ran")), size=2, shape=8)+
  facet_grid(metric ~ model, scales="free_y", switch="y") +
  ggtitle("Structure and Robustness_families") +
  labs( y = "Value", x="Sites") + 
  theme(legend.position = "none") 

