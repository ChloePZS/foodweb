################
#Robustness 3.0#
################

library(tidyverse)
library(bipartite)
library(Hmisc)


#With matrices without all sets of nodes but only the ones of the site

#Import  final matrices
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

      ######Random extinction######

#1. To random extinction observed matrices####
#Details = TRUE to have the data for all the n extinction simulations
ex_ran_vir_sp <- second.extinct(vir_ISmatrix_sp_std, participant = "lower", method="random", nrep=100, details=TRUE) #when set to true, returns a list of the nrep repetitions
ex_ran_mari_sp <- second.extinct(mari_ISmatrix_sp_std, participant = "lower", method="random", nrep=100, details=TRUE)
ex_ran_haw_sp <- second.extinct(haw_ISmatrix_sp_std, participant = "lower", method="random", nrep=100, details=TRUE)
ex_ran_mad_sp <- second.extinct(mad_ISmatrix_sp_std, participant = "lower", method="random", nrep=100, details=TRUE)
ex_ran_nca_sp <- second.extinct(nca_ISmatrix_sp_std, participant = "lower", method="random", nrep=100, details=TRUE)

#Qualitative matrices
ex_ran_vir_sp <- second.extinct(pa_vir_sp, participant = "lower", method="random", nrep=100, details=TRUE)
ex_ran_mari_sp <- second.extinct(pa_mari_sp, participant = "lower", method="random", nrep=100, details=TRUE)
ex_ran_haw_sp <- second.extinct(pa_haw_sp, participant = "lower", method="random", nrep=100, details=TRUE)
ex_ran_mad_sp <- second.extinct(pa_mad_sp, participant = "lower", method="random", nrep=100, details=TRUE)
ex_ran_nca_sp <- second.extinct(pa_nca_sp, participant = "lower", method="random", nrep=100, details=TRUE)



#2. Get the extinction curves mean, quantiles and bootstrapped CI of the observed simulations ####
#Madagascar
ex_ran_mad_sp.dist = sapply(1:length(ex_ran_mad_sp), function (x){ex_ran_mad_sp[[x]][,3]}) #get the nb of ext.higher for each element of the list

mean.q.ext_ran.mad <- rowMeans(ex_ran_mad_sp.dist) %>% 
  cbind(matrixStats::rowQuantiles(ex_ran_mad_sp.dist, probs  = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower") #mean and quantiles of the number of extinct pred for each prey ext steps

names(mean.q.ext_ran.mad)[2:4] <- c("Mean", "q25","q75")
mean.q.ext_ran.mad$ext.lower <- as.numeric(mean.q.ext_ran.mad$ext.lower)

prop_ran_mad.obs <- apply(ex_ran_mad_sp.dist, 2, function(x) (139-cumsum(x))/139) #get the cumulative sums of the proportions species remaining alive

mean.q.ext_ran.mad <- mean.q.ext_ran.mad %>% mutate(ext.higher_prop = rowMeans(prop_ran_mad.obs), #mean proportions of sp remaining alive
                                                    ext.lower_prop = seq(1:nrow(prop_ran_mad.obs))/nrow(prop_ran_mad.obs), #proportion of extinct preys
                                                    site = 'mad') %>%
  cbind(matrixStats::rowQuantiles(prop_ran_mad.obs, probs  = c(0.25,0.75))) %>% data.frame(.) 

#%>% #proportions quantiles
  #mutate(usem = rowMeans(prop_ran_mad.obs) + matrixStats::rowSds(prop_ran_mad.obs)/sqrt(1000), #mean +/- standard error of the mean
         #lsem = rowMeans(prop_ran_mad.obs) - matrixStats::rowSds(prop_ran_mad.obs)/sqrt(1000),
         #uci = rowMeans(prop_ran_mad.obs) + qnorm(0.975)*matrixStats::rowSds(prop_ran_mad.obs)/sqrt(1000), # mean +/- conf interval 95%
         #lci = rowMeans(prop_ran_mad.obs) - qnorm(0.975)*matrixStats::rowSds(prop_ran_mad.obs)/sqrt(1000))

names(mean.q.ext_ran.mad)[c(8,9)] <- c("q25_prop","q75_prop")

#tint <- matrix(NA, nrow = dim(prop_ran_mad.obs)[1], ncol = 2) #Get confidence interval from t distribution
#for (i in 1:dim(prop_ran_mad.obs)[1]) {
  #temp <- t.test(prop_ran_mad.obs[i,], conf.level = 0.95)
  #tint[i,] <- temp$conf.int
  #upse <- mean(prop_ran_mad.obs[i,])+1.96*sd(prop_ran_mad.obs[i,])/sqrt(length(prop_ran_mad.obs[i,]))
  #tint[i,] <- upse$use
  #lowse <- mean(prop_ran_mad.obs[i,])-1.96*sd(prop_ran_mad.obs[i,])/sqrt(length(prop_ran_mad.obs[i,]))
  #tint[i,] <- lowse$lse
#}
#colnames(tint) <- c("lcl", "ucl") #should be able to add lines to this for SEM

ic_boot_mad <- t(apply(prop_ran_mad.obs, 1, smean.cl.boot)) %>% data.frame(.) #bootstrapped confidence intervals. Bootstrap = random resampling with replacement
mean.q.ext_ran.mad <- mean.q.ext_ran.mad %>% cbind(ic_boot_mad[,c(2,3)])

ggplot(mean.q.ext_ran.mad, aes(x=1-ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=Lower, ymax=Upper), alpha=0.2) #Like Giovanni's plot with 1-extlower


#Virgin Islands
ex_ran_vir_sp.dist = sapply(1:length(ex_ran_vir_sp), function (x){ex_ran_vir_sp[[x]][,3]})
mean.q.ext_ran.vir <- rowMeans(ex_ran_vir_sp.dist) %>% 
  cbind(matrixStats::rowQuantiles(ex_ran_vir_sp.dist, probs  = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower")

names(mean.q.ext_ran.vir)[2:4] <- c("Mean", "q25","q75")
mean.q.ext_ran.vir$ext.lower <- as.numeric(mean.q.ext_ran.vir$ext.lower)

prop_ran_vir.obs <- apply(ex_ran_vir_sp.dist, 2, function(x) (191-cumsum(x))/191) #get the cumulative sums of the proportions species remaining alive

mean.q.ext_ran.vir <- mean.q.ext_ran.vir %>% mutate(ext.higher_prop = rowMeans(prop_ran_vir.obs),
                                                    ext.lower_prop = seq(1:nrow(prop_ran_vir.obs))/nrow(prop_ran_vir.obs),
                                                    site = 'vir') %>%
  cbind(matrixStats::rowQuantiles(prop_ran_vir.obs, probs  = c(0.25,0.75))) %>% data.frame(.) 
names(mean.q.ext_ran.vir)[c(8,9)] <- c("q25_prop","q75_prop")

ic_boot_vir <- t(apply(prop_ran_vir.obs, 1, smean.cl.boot)) %>% data.frame(.) #bootstrapped confidence intervals
mean.q.ext_ran.vir <- mean.q.ext_ran.vir %>% cbind(ic_boot_vir[,c(2,3)])

ggplot(mean.q.ext_ran.vir, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=Lower, ymax=Upper), alpha=0.2)


#Marshall Islands
ex_ran_mari_sp.dist = sapply(1:length(ex_ran_mari_sp), function (x){ex_ran_mari_sp[[x]][,3]})
mean.q.ext_ran.mari <- rowMeans(ex_ran_mari_sp.dist) %>% 
  cbind(matrixStats::rowQuantiles(ex_ran_mari_sp.dist, probs  = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower")

names(mean.q.ext_ran.mari)[2:4] <- c("Mean", "q25","q75")
mean.q.ext_ran.mari$ext.lower <- as.numeric(mean.q.ext_ran.mari$ext.lower)

prop_ran_mari.obs <- apply(ex_ran_mari_sp.dist, 2, function(x) (139-cumsum(x))/139) #get the cumulative sums of the proportions species remaining alive

mean.q.ext_ran.mari <- mean.q.ext_ran.mari %>% mutate(ext.higher_prop = rowMeans(prop_ran_mari.obs),
                                                      ext.lower_prop = seq(1:nrow(prop_ran_mari.obs))/nrow(prop_ran_mari.obs),
                                                      site = 'mari') %>%
  cbind(matrixStats::rowQuantiles(prop_ran_mari.obs, probs  = c(0.25,0.75))) %>% data.frame(.) 

names(mean.q.ext_ran.mari)[c(8,9)] <- c("q25_prop","q75_prop")

ic_boot_mari <- t(apply(prop_ran_mari.obs, 1, smean.cl.boot)) %>% data.frame(.) #bootstrapped confidence intervals
mean.q.ext_ran.mari <- mean.q.ext_ran.mari %>% cbind(ic_boot_mari[,c(2,3)])

ggplot(mean.q.ext_ran.mari, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=Lower, ymax=Upper), alpha=0.2)



#Hawai
ex_ran_haw_sp.dist = sapply(1:length(ex_ran_haw_sp), function (x){ex_ran_haw_sp[[x]][,3]})
mean.q.ext_ran.haw <- rowMeans(ex_ran_haw_sp.dist) %>% 
  cbind(matrixStats::rowQuantiles(ex_ran_haw_sp.dist, probs  = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower")

names(mean.q.ext_ran.haw)[2:4] <- c("Mean", "q25","q75")
mean.q.ext_ran.haw$ext.lower <- as.numeric(mean.q.ext_ran.haw$ext.lower)

prop_ran_haw.obs <- apply(ex_ran_haw_sp.dist, 2, function(x) (91-cumsum(x))/91) #get the cumulative sums of the proportions species remaining alive

mean.q.ext_ran.haw <- mean.q.ext_ran.haw %>% mutate(ext.higher_prop = rowMeans(prop_ran_haw.obs),
                                                    ext.lower_prop = seq(1:nrow(prop_ran_haw.obs))/nrow(prop_ran_haw.obs),
                                                    site = 'haw') %>%
  cbind(matrixStats::rowQuantiles(prop_ran_haw.obs, probs  = c(0.25,0.75))) %>% data.frame(.)
names(mean.q.ext_ran.haw)[c(8,9)] <- c("q25_prop","q75_prop")

ic_boot_haw <- t(apply(prop_ran_haw.obs, 1, smean.cl.boot)) %>% data.frame(.) #bootstrapped confidence intervals
mean.q.ext_ran.haw <- mean.q.ext_ran.haw %>% cbind(ic_boot_haw[,c(2,3)])

ggplot(mean.q.ext_ran.haw, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=Lower, ymax=Upper), alpha=0.5)


#New Caledonia
ex_ran_nca_sp.dist = sapply(1:length(ex_ran_nca_sp), function (x){ex_ran_nca_sp[[x]][,3]})
mean.q.ext_ran.nca <- rowMeans(ex_ran_nca_sp.dist) %>% 
  cbind(matrixStats::rowQuantiles(ex_ran_nca_sp.dist, probs  = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower")

names(mean.q.ext_ran.nca)[2:4] <- c("Mean", "q25","q75")
mean.q.ext_ran.nca$ext.lower <- as.numeric(mean.q.ext_ran.nca$ext.lower)

prop_ran_nca.obs <- apply(ex_ran_nca_sp.dist, 2, function(x) (164-cumsum(x))/164) #get the cumulative sums of the proportions species remaining alive

mean.q.ext_ran.nca <- mean.q.ext_ran.nca %>% mutate(ext.higher_prop = rowMeans(prop_ran_nca.obs),
                                                    ext.lower_prop = seq(1:nrow(prop_ran_nca.obs))/nrow(prop_ran_nca.obs),
                                                    site = 'nca') %>%
  cbind(matrixStats::rowQuantiles(prop_ran_nca.obs, probs  = c(0.25,0.75))) %>% data.frame(.) 
names(mean.q.ext_ran.nca)[c(8,9)] <- c("q25_prop","q75_prop")

ic_boot_nca <- t(apply(prop_ran_nca.obs, 1, smean.cl.boot)) %>% data.frame(.) #bootstrapped confidence intervals
mean.q.ext_ran.nca <- mean.q.ext_ran.nca %>% cbind(ic_boot_nca[,c(2,3)])

ggplot(mean.q.ext_ran.nca, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=Lower, ymax=Upper), alpha=0.2)


#Df with observed extinction 
mean.q.ext.ran_obs <- rbind(mean.q.ext_ran.haw, mean.q.ext_ran.mad, mean.q.ext_ran.mari, mean.q.ext_ran.vir,mean.q.ext_ran.nca)


#Plots

#With confidence intervals bootstrapped IC --> don't imply normality
ggplot(mean.q.ext.ran_obs, aes(x=1-ext.lower_prop, y=ext.higher_prop)) + 
  geom_ribbon(data = mean.q.ext.ran_obs, aes(ymin=Lower, ymax=Upper, fill = site), alpha=0.15) +
  geom_line(aes(color = site), size=0.75)+ 
  scale_color_manual(name = "Regions",labels = c("Hawaï","Madagascar", "Marshall Islands", "New Caledonia", "Virgin Islands"), values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  scale_fill_manual(name = "Regions", labels = c("Hawaï","Madagascar", "Marshall Islands", "New Caledonia", "Virgin Islands"), values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  labs( x= "Preys diversity", y="Predators diversity") +
  ggtitle( "a) Random extinction") +
  theme_classic() 

#With quantiles at 25 and 75%
mean.q.ext.ran_obs$site <- as.factor(mean.q.ext.ran_obs$site)
ggplot(mean.q.ext.ran_obs, aes(x=1-ext.lower_prop, y=ext.higher_prop)) + 
  geom_ribbon(data = mean.q.ext.ran_obs, aes(ymin=q25_prop, ymax=q75_prop, fill = site), alpha=0.08) +
  geom_line(aes(color = site), size=1)+ 
  scale_color_manual(name = "Regions",labels = c("Hawaï","Madagascar", "Marshall Islands", "New Caledonia", "Virgin Islands"), values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  scale_fill_manual(name = "Regions", labels = c("Hawaï","Madagascar", "Marshall Islands", "New Caledonia", "Virgin Islands"), values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  labs( x= "Preys diversity", y="Predators diversity") +
  ggtitle( "a) Random extinction") +
  theme_classic() 




  #3. Get extinction curves of the random matrices ####
ran_vir_sp_null <- lapply(rand_vir_sp, second.extinct, participant = "lower", method="random", nrep=10, details=FALSE)
ran_mari_sp_null <- lapply(rand_mari_sp, second.extinct, participant = "lower", method="random", nrep=10, details=FALSE)
ran_nca_sp_null <- lapply(rand_nca_sp, second.extinct, participant = "lower", method="random", nrep=10, details=FALSE)
ran_haw_sp_null <- lapply(rand_haw_sp, second.extinct, participant = "lower", method="random", nrep=10, details=FALSE)
ran_mad_sp_null <- lapply(rand_mad_sp, second.extinct, participant = "lower", method="random", nrep=10, details=FALSE)


#Madagascar
null_ran_mad_sp.dist = sapply(1:length(ran_mad_sp_null), function (x){ran_mad_sp_null[[x]][,3]})

mean.q.ext_ran.mad_null <- rowMeans(null_ran_mad_sp.dist) %>% 
  cbind(matrixStats::rowQuantiles(null_ran_mad_sp.dist, probs  = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower")

names(mean.q.ext_ran.mad_null)[2:4] <- c("Mean", "q25","q75")
mean.q.ext_ran.mad$ext.lower <- as.numeric(mean.q.ext_ran.mad_null$ext.lower)

prop_ran_mad.null <- apply(null_ran_mad_sp.dist, 2, function(x) (139-cumsum(x))/139) #get the cumulative sums of the proportions species remaining alive

mean.q.ext_ran.mad_null <- mean.q.ext_ran.mad_null %>% mutate(ext.higher_prop = rowMeans(prop_ran_mad.null),
                                                    ext.lower_prop = seq(1:30)/30,
                                                    site = 'mad') %>%
  cbind(matrixStats::rowQuantiles(prop_ran_mad.null, probs  = c(0.25,0.75))) %>% data.frame(.) %>%
  mutate(usem = rowMeans(prop_ran_mad.null) + matrixStats::rowSds(prop_ran_mad.null)/sqrt(1000),
         lsem = rowMeans(prop_ran_mad.null) - matrixStats::rowSds(prop_ran_mad.null)/sqrt(1000),
         uci = rowMeans(prop_ran_mad.null) + qnorm(0.975)*matrixStats::rowSds(prop_ran_mad.null)/sqrt(1000),
         lci = rowMeans(prop_ran_mad.null) - qnorm(0.975)*matrixStats::rowSds(prop_ran_mad.null)/sqrt(1000))

names(mean.q.ext_ran.mad_null)[c(8,9)] <- c("q25_prop","q75_prop")

ggplot(mean.q.ext_ran.mad_null, aes(x=1-ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=lsem, ymax=usem), alpha=0.2) #Like Giovanni's plot with 1-extlower

tint <- matrix(NA, nrow = dim(prop_ran_mad.null)[1], ncol = 2) #Get confidence interval from t distribution
for (i in 1:dim(prop_ran_mad.null)[1]) {
  temp <- t.test(prop_ran_mad.null[i,], conf.level = 0.95)
  tint[i,] <- temp$conf.int
  #upse <- mean(prop_ran_mad.obs[i,])+1.96*sd(prop_ran_mad.obs[i,])/sqrt(length(prop_ran_mad.obs[i,]))
  #tint[i,] <- upse$use
  #lowse <- mean(prop_ran_mad.obs[i,])-1.96*sd(prop_ran_mad.obs[i,])/sqrt(length(prop_ran_mad.obs[i,]))
  #tint[i,] <- lowse$lse
  
}
colnames(tint) <- c("lcl", "ucl") #should be able to add lines to this for SEM

mean.q.ext_ran.mad_null <- mean.q.ext_ran.mad_null %>% cbind(tint)


ggplot(mean.q.ext_ran.mad_null, aes(x=1-ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2) #Like Giovanni's plot with 1-extlower


#Virgin Islands
null_ran_vir_sp.dist = sapply(1:length(ran_vir_sp_null), function (x){ran_vir_sp_null[[x]][,3]})

mean.q.ext_ran.vir_null <- rowMeans(null_ran_vir_sp.dist) %>% 
  cbind(matrixStats::rowQuantiles(null_ran_vir_sp.dist, probs  = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower")

names(mean.q.ext_ran.vir_null)[2:4] <- c("Mean", "q25","q75")
mean.q.ext_ran.vir$ext.lower <- as.numeric(mean.q.ext_ran.vir_null$ext.lower)

prop_ran_vir.null <- apply(null_ran_vir_sp.dist, 2, function(x) (191-cumsum(x))/191) #get the cumulative sums of the proportions species remaining alive

mean.q.ext_ran.vir_null <- mean.q.ext_ran.vir_null %>% mutate(ext.higher_prop = rowMeans(prop_ran_vir.null),
                                                              ext.lower_prop = seq(1:30)/30,
                                                              site = 'vir') %>%
  cbind(matrixStats::rowQuantiles(prop_ran_vir.null, probs  = c(0.25,0.75))) %>% data.frame(.) %>%
  mutate(usem = rowMeans(prop_ran_vir.null) + matrixStats::rowSds(prop_ran_vir.null)/sqrt(1000),
         lsem = rowMeans(prop_ran_vir.null) - matrixStats::rowSds(prop_ran_vir.null)/sqrt(1000),
         uci = rowMeans(prop_ran_vir.null) + qnorm(0.975)*matrixStats::rowSds(prop_ran_vir.null)/sqrt(1000),
         lci = rowMeans(prop_ran_vir.null) - qnorm(0.975)*matrixStats::rowSds(prop_ran_vir.null)/sqrt(1000))

names(mean.q.ext_ran.vir_null)[c(8,9)] <- c("q25_prop","q75_prop")

ggplot(mean.q.ext_ran.vir_null, aes(x=1-ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=lsem, ymax=usem), alpha=0.2) #Like Giovanni's plot with 1-extlower

tint <- matrix(NA, nrow = dim(prop_ran_vir.null)[1], ncol = 2) #Get confidence interval from t distribution
for (i in 1:dim(prop_ran_vir.null)[1]) {
  temp <- t.test(prop_ran_vir.null[i,], conf.level = 0.95)
  tint[i,] <- temp$conf.int
}
colnames(tint) <- c("lcl", "ucl") #should be able to add lines to this for SEM

mean.q.ext_ran.vir_null <- mean.q.ext_ran.vir_null %>% cbind(tint)


ggplot(mean.q.ext_ran.vir_null, aes(x=1-ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2) #Like Giovanni's plot with 1-extlower

#Marshall Islands
null_ran_mari_sp.dist = sapply(1:length(ran_mari_sp_null), function (x){ran_mari_sp_null[[x]][,3]})

mean.q.ext_ran.mari_null <- rowMeans(null_ran_mari_sp.dist) %>% 
  cbind(matrixStats::rowQuantiles(null_ran_mari_sp.dist, probs  = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower")

names(mean.q.ext_ran.mari_null)[2:4] <- c("Mean", "q25","q75")
mean.q.ext_ran.mari$ext.lower <- as.numeric(mean.q.ext_ran.mari_null$ext.lower)

prop_ran_mari.null <- apply(null_ran_mari_sp.dist, 2, function(x) (139-cumsum(x))/139) #get the cumulative sums of the proportions species remaining alive

mean.q.ext_ran.mari_null <- mean.q.ext_ran.mari_null %>% mutate(ext.higher_prop = rowMeans(prop_ran_mari.null),
                                                              ext.lower_prop = seq(1:27)/27,
                                                              site = 'mari') %>%
  cbind(matrixStats::rowQuantiles(prop_ran_mari.null, probs  = c(0.25,0.75))) %>% data.frame(.) %>%
  mutate(usem = rowMeans(prop_ran_mari.null) + matrixStats::rowSds(prop_ran_mari.null)/sqrt(1000),
         lsem = rowMeans(prop_ran_mari.null) - matrixStats::rowSds(prop_ran_mari.null)/sqrt(1000),
         uci = rowMeans(prop_ran_mari.null) + qnorm(0.975)*matrixStats::rowSds(prop_ran_mari.null)/sqrt(1000),
         lci = rowMeans(prop_ran_mari.null) - qnorm(0.975)*matrixStats::rowSds(prop_ran_mari.null)/sqrt(1000))

names(mean.q.ext_ran.mari_null)[c(8,9)] <- c("q25_prop","q75_prop")

ggplot(mean.q.ext_ran.mari_null, aes(x=1-ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=lsem, ymax=usem), alpha=0.2) #Like Giovanni's plot with 1-extlower

tint <- matrix(NA, nrow = dim(prop_ran_mari.null)[1], ncol = 2) #Get confidence interval from t distribution
for (i in 1:dim(prop_ran_mari.null)[1]) {
  temp <- t.test(prop_ran_mari.null[i,], conf.level = 0.95)
  tint[i,] <- temp$conf.int
  #upse <- mean(prop_ran_mari.obs[i,])+1.96*sd(prop_ran_mari.obs[i,])/sqrt(length(prop_ran_mari.obs[i,]))
  #tint[i,] <- upse$use
  #lowse <- mean(prop_ran_mari.obs[i,])-1.96*sd(prop_ran_mari.obs[i,])/sqrt(length(prop_ran_mari.obs[i,]))
  #tint[i,] <- lowse$lse
  
}
colnames(tint) <- c("lcl", "ucl") #should be able to add lines to this for SEM

mean.q.ext_ran.mari_null <- mean.q.ext_ran.mari_null %>% cbind(tint)


ggplot(mean.q.ext_ran.mari_null, aes(x=1-ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2) #Like Giovanni's plot with 1-extlower

#Hawai
null_ran_haw_sp.dist = sapply(1:length(ran_haw_sp_null), function (x){ran_haw_sp_null[[x]][,3]})

mean.q.ext_ran.haw_null <- rowMeans(null_ran_haw_sp.dist) %>% 
  cbind(matrixStats::rowQuantiles(null_ran_haw_sp.dist, probs  = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower")

names(mean.q.ext_ran.haw_null)[2:4] <- c("Mean", "q25","q75")
mean.q.ext_ran.haw$ext.lower <- as.numeric(mean.q.ext_ran.haw_null$ext.lower)

prop_ran_haw.null <- apply(null_ran_haw_sp.dist, 2, function(x) (91-cumsum(x))/91) #get the cumulative sums of the proportions species remaining alive

mean.q.ext_ran.haw_null <- mean.q.ext_ran.haw_null %>% mutate(ext.higher_prop = rowMeans(prop_ran_haw.null),
                                                              ext.lower_prop = seq(1:31)/31,
                                                              site = 'haw') %>%
  cbind(matrixStats::rowQuantiles(prop_ran_haw.null, probs  = c(0.25,0.75))) %>% data.frame(.) %>%
  mutate(usem = rowMeans(prop_ran_haw.null) + matrixStats::rowSds(prop_ran_haw.null)/sqrt(1000),
         lsem = rowMeans(prop_ran_haw.null) - matrixStats::rowSds(prop_ran_haw.null)/sqrt(1000),
         uci = rowMeans(prop_ran_haw.null) + qnorm(0.975)*matrixStats::rowSds(prop_ran_haw.null)/sqrt(1000),
         lci = rowMeans(prop_ran_haw.null) - qnorm(0.975)*matrixStats::rowSds(prop_ran_haw.null)/sqrt(1000))

names(mean.q.ext_ran.haw_null)[c(8,9)] <- c("q25_prop","q75_prop")

ggplot(mean.q.ext_ran.haw_null, aes(x=1-ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=lsem, ymax=usem), alpha=0.2) #Like Giovanni's plot with 1-extlower

tint <- matrix(NA, nrow = dim(prop_ran_haw.null)[1], ncol = 2) #Get confidence interval from t distribution
for (i in 1:dim(prop_ran_haw.null)[1]) {
  temp <- t.test(prop_ran_haw.null[i,], conf.level = 0.95)
  tint[i,] <- temp$conf.int
  #upse <- mean(prop_ran_haw.obs[i,])+1.96*sd(prop_ran_haw.obs[i,])/sqrt(length(prop_ran_haw.obs[i,]))
  #tint[i,] <- upse$use
  #lowse <- mean(prop_ran_haw.obs[i,])-1.96*sd(prop_ran_haw.obs[i,])/sqrt(length(prop_ran_haw.obs[i,]))
  #tint[i,] <- lowse$lse
  
}
colnames(tint) <- c("lcl", "ucl") #should be able to add lines to this for SEM

mean.q.ext_ran.haw_null <- mean.q.ext_ran.haw_null %>% cbind(tint)


ggplot(mean.q.ext_ran.haw_null, aes(x=1-ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2) #Like Giovanni's plot with 1-extlower


#New Caledonia
null_ran_nca_sp.dist = sapply(1:length(ran_nca_sp_null), function (x){ran_nca_sp_null[[x]][,3]})

mean.q.ext_ran.nca_null <- rowMeans(null_ran_nca_sp.dist) %>% 
  cbind(matrixStats::rowQuantiles(null_ran_nca_sp.dist, probs  = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower")

names(mean.q.ext_ran.nca_null)[2:4] <- c("Mean", "q25","q75")
mean.q.ext_ran.nca$ext.lower <- as.numeric(mean.q.ext_ran.nca_null$ext.lower)

prop_ran_nca.null <- apply(null_ran_nca_sp.dist, 2, function(x) (164-cumsum(x))/164) #get the cumulative sums of the proportions species remaining alive

mean.q.ext_ran.nca_null <- mean.q.ext_ran.nca_null %>% mutate(ext.higher_prop = rowMeans(prop_ran_nca.null),
                                                              ext.lower_prop = seq(1:23)/23,
                                                              site = 'nca') %>%
  cbind(matrixStats::rowQuantiles(prop_ran_nca.null, probs  = c(0.25,0.75))) %>% data.frame(.) %>%
  mutate(usem = rowMeans(prop_ran_nca.null) + matrixStats::rowSds(prop_ran_nca.null)/sqrt(1000),
         lsem = rowMeans(prop_ran_nca.null) - matrixStats::rowSds(prop_ran_nca.null)/sqrt(1000),
         uci = rowMeans(prop_ran_nca.null) + qnorm(0.975)*matrixStats::rowSds(prop_ran_nca.null)/sqrt(1000),
         lci = rowMeans(prop_ran_nca.null) - qnorm(0.975)*matrixStats::rowSds(prop_ran_nca.null)/sqrt(1000))

names(mean.q.ext_ran.nca_null)[c(8,9)] <- c("q25_prop","q75_prop")

ggplot(mean.q.ext_ran.nca_null, aes(x=1-ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=lsem, ymax=usem), alpha=0.2) #Like Giovanni's plot with 1-extlower

tint <- matrix(NA, nrow = dim(prop_ran_nca.null)[1], ncol = 2) #Get confidence interval from t distribution
for (i in 1:dim(prop_ran_nca.null)[1]) {
  temp <- t.test(prop_ran_nca.null[i,], conf.level = 0.95)
  tint[i,] <- temp$conf.int
  #upse <- mean(prop_ran_nca.obs[i,])+1.96*sd(prop_ran_nca.obs[i,])/sqrt(length(prop_ran_nca.obs[i,]))
  #tint[i,] <- upse$use
  #lowse <- mean(prop_ran_nca.obs[i,])-1.96*sd(prop_ran_nca.obs[i,])/sqrt(length(prop_ran_nca.obs[i,]))
  #tint[i,] <- lowse$lse
  
}
colnames(tint) <- c("lcl", "ucl") #should be able to add lines to this for SEM

mean.q.ext_ran.nca_null <- mean.q.ext_ran.nca_null %>% cbind(tint)


ggplot(mean.q.ext_ran.nca_null, aes(x=1-ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2) #Like Giovanni's plot with 1-extlower


#Df with observed extinction 
mean.q.ext.ran_obs <- rbind(mean.q.ext_ran.haw, mean.q.ext_ran.mad, mean.q.ext_ran.mari, mean.q.ext_ran.vir,mean.q.ext_ran.nca) %>%
  mutate(type = "obs")
mean.q.ext.ran_null <- rbind(mean.q.ext_ran.haw_null, mean.q.ext_ran.mad_null, mean.q.ext_ran.mari_null, mean.q.ext_ran.vir_null,mean.q.ext_ran.nca_null) %>%
  mutate(type ="null")

mean.q.ext.ran_full <- rbind(mean.q.ext.ran_null, mean.q.ext.ran_obs)


#With quantiles at 25 and 75%
mean.q.ext.ran_null$site <- as.factor(mean.q.ext.ran_null$site)
ggplot(mean.q.ext.ran_null, aes(x=1-ext.lower_prop, y=ext.higher_prop)) + 
  geom_ribbon(data = mean.q.ext.ran_null, aes(ymin=q25_prop, ymax=q75_prop, fill = site), alpha=0.08) +
  geom_line(aes(color = site), size=1)+ 
  scale_color_manual(name = "Regions",labels = c("Hawaï","Madagascar", "Marshall Islands", "New Caledonia", "Virgin Islands"), values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  scale_fill_manual(name = "Regions", labels = c("Hawaï","Madagascar", "Marshall Islands", "New Caledonia", "Virgin Islands"), values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  labs( x= "Preys diversity", y="Predators diversity") +
  ggtitle( "a) Random extinction_null") +
  theme_classic() 

#Plots with both distribution null vs obs
ggplot(mean.q.ext.ran_full, aes(x=1-ext.lower_prop, y=ext.higher_prop)) + 
  geom_ribbon(data = subset(mean.q.ext.ran_full, type %in% "null"), aes(ymin=lcl, ymax=lcl, fill = site), alpha=0.2) +
  geom_line(aes(linetype = type, color = site)) + 
  scale_linetype_manual(name = "", labels = (c("Null distribution", "Observed")), values = c(1,2)) +
  scale_color_manual(name = "Regions",labels = c("Hawaï","Madagascar", "Marshall Islands", "New Caledonia", "Virgin Islands"), values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  scale_fill_manual(name = "Regions", labels = c("Hawaï","Madagascar", "Marshall Islands", "New Caledonia", "Virgin Islands"), values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  labs( x= "Proportions of primary extinctions - Preys", y="Proportions of predators still alive") +
  facet_wrap(site~.) +
  theme(strip.text.x = element_text(size=9, face="bold"),
        strip.text.y = element_blank(),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(colour = "black", 
                                   size=0.5, 
                                   lineend = "butt"),
        axis.line.y = element_line(colour = "black", 
                                   size=0.5)) +
  ggtitle("Random extinction - Null model connectance")




  #4. Degree extinction ####
ex_deg_vir_sp <- second.extinct(pa_vir_sp, participant = "lower", method="degre")

ex_deg_mari_sp <- second.extinct(pa_mari_sp, participant = "lower", method="degre")

ex_deg_haw_sp <- second.extinct(pa_haw_sp, participant = "lower", method="degre")

ex_deg_mad_sp <- second.extinct(pa_mad_sp, participant = "lower", method="degre")

ex_deg_nca_sp <- second.extinct(pa_nca_sp, participant = "lower", method="degre")

slope.bipartite(ex_deg_nca_sp)


prop_deg.obs <-  ex_deg_mad_sp[,2:3] %>% data.frame(.) %>%
  mutate(ext.higher_prop = (139-cumsum(ex_deg_mad_sp[,3]))/139,
         ext.lower_prop = seq(1:30)/30) %>%
  rbind(c(0,0,1,0)) %>%
  mutate(site = "mad") %>%
  rbind(ex_deg_vir_sp[,2:3] %>% data.frame(.) %>% 
          mutate(ext.higher_prop = (191-cumsum(ex_deg_vir_sp[,3]))/191,
                 ext.lower_prop = seq(1:30)/30) %>%
          rbind(c(0,0,1,0)) %>%
                 mutate(site = "vir")) %>%
  rbind(ex_deg_mari_sp[,2:3] %>% data.frame(.) %>% 
          mutate(ext.higher_prop = (139-cumsum(ex_deg_mari_sp[,3]))/139,
                 ext.lower_prop = seq(1:27)/27) %>%
          rbind(c(0,0,1,0)) %>%
                 mutate(site = "mari")) %>%
  rbind(ex_deg_haw_sp[,2:3] %>% data.frame(.) %>% 
          mutate(ext.higher_prop = (91-cumsum(ex_deg_haw_sp[,3]))/91,
                 ext.lower_prop = seq(1:31)/31) %>%
          rbind(c(0,0,1,0)) %>%
                 mutate(site = "haw")) %>%
  rbind(ex_deg_nca_sp[,2:3] %>% data.frame(.) %>% 
          mutate(ext.higher_prop = (164-cumsum(ex_deg_nca_sp[,3]))/164,
                 ext.lower_prop = seq(1:23)/23) %>%
          rbind(c(0,0,1,0)) %>%
                 mutate(site = "nca")) 
  

ggplot(prop_deg.obs, aes(x=1-ext.lower_prop, y=ext.higher_prop)) + 
  geom_line(aes(color = site), size=0.75)+ 
  scale_color_manual(name = "Regions",labels = c("Hawaii","Madagascar", "Marshall Islands", "New Caledonia", "Virgin Islands"), values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  scale_fill_manual(name = "Regions", labels = c("Hawaii","Madagascar", "Marshall Islands", "New Caledonia", "Virgin Islands"), values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  labs( x= "Preys diversity", y="Predators diversity") +
  ggtitle( "b) From most-connected nodes") +
  theme_classic() 


deg_vir_sp_null <- lapply(rand_vir_sp, second.extinct, participant = "lower", method="degree")
deg_mari_sp_null <- lapply(rand_mari_sp, second.extinct, participant = "lower", method="degree")
deg_nca_sp_null <- lapply(rand_nca_sp, second.extinct, participant = "lower", method="degree")
deg_haw_sp_null <- lapply(rand_haw_sp, second.extinct, participant = "lower", method="degree")
deg_mad_sp_null <- lapply(rand_mad_sp, second.extinct, participant = "lower", method="degree")

v <- ex_deg_vir_sp[,2:3] %>% data.frame(.) %>% 
  mutate(ext.higher_prop = (191-cumsum(ex_deg_vir_sp[,3]))/191,
         ext.lower_prop = seq(1:30)/30) %>%
  rbind(c(0,0,1,0)) %>%
         mutate(site = "vir")

ggplot(v, aes(x=1-ext.lower_prop, y=ext.higher_prop))+
  geom_line()
