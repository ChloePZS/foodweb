#######################
#Robustness analysis with suffled columns rand_sp
#################################################


############
#Robustness#
############

#1. To random extinction####
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

obs_ext.ran <- ex_ran_vir_sp[,3] %>% data.frame(.) %>% mutate(ext.higher_prop = (628-cumsum(ex_ran_vir_sp[,3]))/628, 
                                                              ext.lower_prop = ex_ran_vir_sp[,1]/38,
                                                              site ="vir") %>% 
  rbind(ex_ran_mari_sp[,3] %>% data.frame(.) %>% mutate(ext.higher_prop = (628-cumsum(ex_ran_mari_sp[,3]))/628, 
                                                        ext.lower_prop = ex_ran_mari_sp[,1]/38,
                                                        site ="mari")) %>%
  rbind(ex_ran_mad_sp[,3] %>% data.frame(.) %>% mutate(ext.higher_prop = (628-cumsum(ex_ran_mad_sp[,3]))/628, 
                                                       ext.lower_prop = ex_ran_mad_sp[,1]/38,
                                                       site ="mad")) %>%
  rbind(ex_ran_nca_sp[,3] %>% data.frame(.) %>% mutate(ext.higher_prop = (628-cumsum(ex_ran_nca_sp[,3]))/628, 
                                                       ext.lower_prop = ex_ran_nca_sp[,1]/38,
                                                       site ="nca")) %>%
  rbind(ex_ran_haw_sp[,3] %>% data.frame(.) %>% mutate(ext.higher_prop = (628-cumsum(ex_ran_haw_sp[,3]))/628, 
                                                       ext.lower_prop = ex_ran_haw_sp[,1]/38,
                                                       site ="haw"))
names(obs_ext.ran)[1] <- "ext.higher_nb"



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
rob_vir_sp2_null <- lapply(rand_vir_sp2, second.extinct, participant = "lower", method="random", nrep=10, details=FALSE)
rob_mari_sp2_null <- lapply(rand_mari_sp2, second.extinct, participant = "lower", method="random", nrep=10, details=FALSE)
rob_nca_sp2_null <- lapply(rand_nca_sp2, second.extinct, participant = "lower", method="random", nrep=10, details=FALSE)
rob_haw_sp2_null <- lapply(rand_haw_sp2, second.extinct, participant = "lower", method="random", nrep=10, details=FALSE)
rob_mad_sp2_null <- lapply(rand_mad_sp2, second.extinct, participant = "lower", method="random", nrep=10, details=FALSE)


#Get extinction curves
#Madagascar
ext.ran_null_mad2 = sapply(1:length(rob_mad_sp2_null), function (x){rob_mad_sp2_null[[x]][,3]})
mean.q.ext.mad2 <- rowMeans(ext.ran_null_mad2) %>%
  cbind(matrixStats::rowQuantiles(ext.ran_null_mad2, probs = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower")


#Wrong methods to get the quantiles !!!!!
names(mean.q.ext.mad2)[2:4] <- c("Mean", "q25","q75")
mean.q.ext.mad2$ext.lower <- as.numeric(mean.q.ext.mad2$ext.lower)


mean.q.ext.mad2$ext.higher_prop <- (628-cumsum(mean.q.ext.mad2[,2]))/628
mean.q.ext.mad2$ext.lower_prop <- mean.q.ext.mad2[,1]/38
mean.q.ext.mad2$q25_prop <- (628-cumsum(mean.q.ext.mad2[,3]))/628
mean.q.ext.mad2$q75_prop <- (628-cumsum(mean.q.ext.mad2[,4]))/628
mean.q.ext.mad2$site <- "mad2"

plot(mean.q.ext.mad2[,1]/38, (628-cumsum(mean.q.ext.mad2[,2]))/628) 

ggplot(mean.q.ext.mad2, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2)


prop_mad2 <- apply(ext.ran_null_mad2, 2, function(x) (628-cumsum(x))/628)
prop_mad2.mean.q <- rowMeans(prop_mad2) %>%
  cbind(matrixStats::rowQuantiles(prop_mad2, probs  = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower") %>%
  mutate(ext.lower_prop = seq(1:38)/38,
         site = 'mad')

names(prop_mad2.mean.q)[2:4] <- c("ext.higher_prop","q25_prop","q75_prop")

ggplot(prop_mad2.mean.q, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2)



#Virgin
ext.ran_null_vir2 = sapply(1:length(rob_vir_sp2_null), function (x){rob_vir_sp2_null[[x]][,3]})
mean.q.ext.vir2 <- rowMeans(ext.ran_null_vir2) %>% 
  cbind(matrixStats::rowQuantiles(ext.ran_null_vir2, probs = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower")



names(mean.q.ext.vir2)[2:4] <- c("Mean", "q25","q75")
mean.q.ext.vir2$ext.lower <- as.numeric(mean.q.ext.vir2$ext.lower)

mean.q.ext.vir2$ext.higher_prop <- (628-cumsum(mean.q.ext.vir2[,2]))/628
mean.q.ext.vir2$ext.lower_prop <- mean.q.ext.vir2[,1]/38
mean.q.ext.vir2$q25_prop <- (628-cumsum(mean.q.ext.vir2[,3]))/628
mean.q.ext.vir2$q75_prop <- (628-cumsum(mean.q.ext.vir2[,4]))/628
mean.q.ext.vir2$site <- "vir2"

plot(mean.q.ext.vir2[,1]/38, (628-cumsum(mean.q.ext.vir2[,2]))/628) 

ggplot(mean.q.ext.vir2, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2)


prop_vir2 <- apply(ext.ran_null_vir2, 2, function(x) (628-cumsum(x))/628)
prop_vir2.mean.q <- rowMeans(prop_vir2) %>%
  cbind(matrixStats::rowQuantiles(prop_vir2, probs  = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower") %>%
  mutate(ext.lower_prop = seq(1:38)/38,
         site ="vir")

names(prop_vir2.mean.q)[2:4] <- c("ext.higher_prop","q25_prop","q75_prop")

ggplot(prop_vir2.mean.q, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2)


#Marshall
ext.ran_null_mari2 = sapply(1:length(rob_mari_sp2_null), function (x){rob_mari_sp2_null[[x]][,3]})
mean.q.ext.mari2 <- rowMeans(ext.ran_null_mari2) %>% 
  cbind(matrixStats::rowQuantiles(ext.ran_null_mari2, probs = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower")

names(mean.q.ext.mari2)[2:4] <- c("Mean", "q25","q75")
mean.q.ext.mari2$ext.lower <- as.numeric(mean.q.ext.mari2$ext.lower)

mean.q.ext.mari2$ext.higher_prop <- (628-cumsum(mean.q.ext.mari2[,2]))/628
mean.q.ext.mari2$ext.lower_prop <- mean.q.ext.mari2[,1]/38
mean.q.ext.mari2$q25_prop <- (628-cumsum(mean.q.ext.mari2[,3]))/628
mean.q.ext.mari2$q75_prop <- (628-cumsum(mean.q.ext.mari2[,4]))/628
mean.q.ext.mari2$site <- "mari2"

plot(mean.q.ext.mari2[,1]/38, (628-cumsum(mean.q.ext.mari2[,2]))/628) 

ggplot(mean.q.ext.mari2, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2)


prop_mari2 <- apply(ext.ran_null_mari2, 2, function(x) (628-cumsum(x))/628)
prop_mari2.mean.q <- rowMeans(prop_mari2) %>%
  cbind(matrixStats::rowQuantiles(prop_mari2, probs  = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower") %>%
  mutate(ext.lower_prop = seq(1:38)/38, 
         site = "mari")

names(prop_mari2.mean.q)[2:4] <- c("ext.higher_prop","q25_prop","q75_prop")

ggplot(prop_mari2.mean.q, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2)


#Hawai
ext.ran_null_haw2 = sapply(1:length(rob_haw_sp2_null), function (x){rob_haw_sp2_null[[x]][,3]})
mean.q.ext.haw2 <- rowMeans(ext.ran_null_haw2) %>% 
  cbind(matrixStats::rowQuantiles(ext.ran_null_haw2, probs = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower")

names(mean.q.ext.haw2)[2:4] <- c("Mean", "q25","q75")
mean.q.ext.haw2$ext.lower <- as.numeric(mean.q.ext.haw2$ext.lower)

mean.q.ext.haw2$ext.higher_prop <- (628-cumsum(mean.q.ext.haw2[,2]))/628
mean.q.ext.haw2$ext.lower_prop <- mean.q.ext.haw2[,1]/38
mean.q.ext.haw2$q25_prop <- (628-cumsum(mean.q.ext.haw2[,3]))/628
mean.q.ext.haw2$q75_prop <- (628-cumsum(mean.q.ext.haw2[,4]))/628
mean.q.ext.haw2$site <- "haw2"

plot(mean.q.ext.haw2[,1]/38, (628-cumsum(mean.q.ext.haw2[,2]))/628) 

ggplot(mean.q.ext.haw2, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2)


prop_haw2 <- apply(ext.ran_null_haw2, 2, function(x) (628-cumsum(x))/628)
prop_haw2.mean.q <- rowMeans(prop_haw2) %>%
  cbind(matrixStats::rowQuantiles(prop_haw2, probs  = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower") %>%
  mutate(ext.lower_prop = seq(1:38)/38,
         site = "haw")

names(prop_haw2.mean.q)[2:4] <- c("ext.higher_prop","q25_prop","q75_prop")

ggplot(prop_haw2.mean.q, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2)


#New Caledonia
ext.ran_null_nca2 = sapply(1:length(rob_nca_sp2_null), function (x){rob_nca_sp2_null[[x]][,3]})
mean.q.ext.nca2 <- rowMeans(ext.ran_null_nca2) %>% 
  cbind(matrixStats::rowQuantiles(ext.ran_null_nca2, probs = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower")

names(mean.q.ext.nca2)[2:4] <- c("Mean", "q25","q75")
mean.q.ext.nca2$ext.lower <- as.numeric(mean.q.ext.nca2$ext.lower)

mean.q.ext.nca2$ext.higher_prop <- (628-cumsum(mean.q.ext.nca2[,2]))/628
mean.q.ext.nca2$ext.lower_prop <- mean.q.ext.nca2[,1]/38
mean.q.ext.nca2$q25_prop <- (628-cumsum(mean.q.ext.nca2[,3]))/628
mean.q.ext.nca2$q75_prop <- (628-cumsum(mean.q.ext.nca2[,4]))/628
mean.q.ext.nca2$site <- "nca2"

plot(mean.q.ext.nca2[,1]/38, (628-cumsum(mean.q.ext.nca2[,2]))/628) 

ggplot(mean.q.ext.nca2, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2)


prop_nca2 <- apply(ext.ran_null_nca2, 2, function(x) (628-cumsum(x))/628)
prop_nca2.mean.q <- rowMeans(prop_nca2) %>%
  cbind(matrixStats::rowQuantiles(prop_nca2, probs  = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower") %>%
  mutate(ext.lower_prop = seq(1:38)/38,
         site = "nca")

names(prop_nca2.mean.q)[2:4] <- c("ext.higher_prop","q25_prop","q75_prop")

ggplot(prop_nca2.mean.q, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2)


#Dataframe with null distributions and observed values
mean.q.ext.ran2 <- rbind(mean.q.ext.haw2, mean.q.ext.mad2, mean.q.ext.mari2, mean.q.ext.nca2, mean.q.ext.vir2) %>% mutate (type = "null")
obs_ext.ran <- mutate(obs_ext.ran, type ="obs")
ext.ran_full2 <- rbind.fill(mean.q.ext.ran2, obs_ext.ran)

prop_mean.q.ran2 <- rbind(prop_haw2.mean.q, prop_mad2.mean.q, prop_mari2.mean.q, prop_vir2.mean.q, prop_nca2.mean.q) %>% mutate(type ="null")
prop_ext.ran_full2 <- rbind.fill(prop_mean.q.ran2, obs_ext.ran)


#Grouped plot
mean.q.ext.ran2$site <- as.factor(mean.q.ext.ran2$site)
ggplot(prop_ext.ran_full2, aes(x=ext.lower_prop, y=ext.higher_prop)) + 
  geom_ribbon(data = subset(prop_ext.ran_full2, type %in% "null"), aes(ymin=q25_prop, ymax=q75_prop, fill = site), alpha=0.2) +
  geom_line(aes(linetype = type, color = site)) + 
  scale_color_manual(name = "Regions",labels = c("Hawaï","Madagascar", "Marshall Islands", "New Caledonia", "Virgin Islands"), values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  scale_fill_manual(name = "Regions", labels = c("Hawaï","Madagascar", "Marshall Islands", "New Caledonia", "Virgin Islands"), values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  scale_linetype_manual(name = "", labels = (c("Null distribution", "Observed")), values = c(1,2)) +
  labs( x= "Proportions of primary extinctions - Preys", y="Proportions of predators still alive") +
  ggtitle( "a) ran2dom extinction") +
  theme_classic() 


#Check Area under curve vs Robustness value
MESS::auc(x = (38-cumsum(ex_ran_nca_sp[,2]))/38, y = ex_ran_nca_sp[,1]/38) #curve lower extinction given slope.bipartite plot
MESS::auc(x = (628-cumsum(ex_ran_nca_sp[,3]))/628, y = ex_ran_nca_sp[,1]/38) #curve lower ext with calculated prop --> okay so robustness seems to be the area one of the right curve
MESS::auc(x =(628 - cumsum(rob_nca_sp_null[[1]][,3]))/628, y= rob_nca_sp_null[[1]][,1]/38) # curve lower random




#2. Extinction increasing degree --> from the most to least connected nodes ####
ex_degdec_vir_sp <- second.extinct(vir_ISmatrix_sp2, participant = "lower", method="degree", nrep=100, details=FALSE)
robustness(ex_degdec_vir_sp)
slope.bipartite(ex_degdec_vir_sp)

ex_degdec_mari_sp <- second.extinct(mari_ISmatrix_sp2, participant = "lower", method="degree", nrep=100, details=FALSE)
robustness(ex_degdec_mari_sp)
slope.bipartite(ex_degdec_mari_sp)

ex_degdec_haw_sp <- second.extinct(haw_ISmatrix_sp2, participant = "lower", method="degree", nrep=100, details=FALSE)
robustness(ex_degdec_haw_sp)
slope.bipartite(ex_degdec_haw_sp)

ex_degdec_mad_sp <- second.extinct(mad_ISmatrix_sp2, participant = "lower", method="degree", nrep=100, details=FALSE)
robustness(ex_degdec_mad_sp)
slope.bipartite(ex_degdec_mad_sp)

ex_degdec_nca_sp <- second.extinct(nca_ISmatrix_sp2, participant = "lower", method="degree", nrep=100, details=FALSE)
robustness(ex_degdec_nca_sp)
slope.bipartite(ex_degdec_nca_sp)


obs_full_sp$robustness.degdec <- c(robustness(ex_degdec_mad_sp), robustness(ex_degdec_nca_sp), robustness(ex_degdec_mari_sp), robustness(ex_degdec_haw_sp), robustness(ex_degdec_vir_sp))

#Robustness null models
degdec_vir_sp2_null <- lapply(rand_vir_sp2, second.extinct, participant = "lower", method="degree", nrep=10, details=FALSE)
degdec_mari_sp2_null <- lapply(rand_mari_sp2, second.extinct, participant = "lower", method="degree", nrep=10, details=FALSE)
degdec_nca_sp2_null <- lapply(rand_nca_sp2, second.extinct, participant = "lower", method="degree", nrep=10, details=FALSE)
degdec_haw_sp2_null <- lapply(rand_haw_sp2, second.extinct, participant = "lower", method="degree", nrep=10, details=FALSE)
degdec_mad_sp2_null <- lapply(rand_mad_sp2, second.extinct, participant = "lower", method="degree", nrep=10, details=FALSE)


#Extinction curves

#Madagascar
ext.degdec_null_mad2 = sapply(1:length(degdec_mad_sp2_null), function (x){degdec_mad_sp2_null[[x]][,3]})
mean.q.ext_deg.mad2 <- rowMeans(ext.degdec_null_mad2) %>% 
  cbind(matrixStats::rowQuantiles(ext.degdec_null_mad2, probs  = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower")

names(mean.q.ext_deg.mad2)[2:4] <- c("Mean", "q25","q75")
mean.q.ext_deg.mad2$ext.lower <- as.numeric(mean.q.ext_deg.mad2$ext.lower)


mean.q.ext_deg.mad2$ext.higher_prop <- (628-cumsum(mean.q.ext_deg.mad2[,2]))/628
mean.q.ext_deg.mad2$ext.lower_prop <- mean.q.ext_deg.mad2[,1]/38
mean.q.ext_deg.mad2$q25_prop <- (628-cumsum(mean.q.ext_deg.mad2[,3]))/628
mean.q.ext_deg.mad2$q75_prop <- (628-cumsum(mean.q.ext_deg.mad2[,4]))/628
mean.q.ext_deg.mad2$site <- "mad2"

plot(mean.q.ext_deg.mad2[,1]/38, (628-cumsum(mean.q.ext_deg.mad2[,2]))/628) 

ggplot(mean.q.ext_deg.mad2, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2)


prop_degdec.mad2 <- apply(ext.degdec_null_mad2, 2, function(x) (628-cumsum(x))/628)
prop_degdec.mad2.mean.q <- rowMeans(prop_degdec.mad2) %>%
  cbind(matrixStats::rowQuantiles(prop_degdec.mad2, probs  = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower") %>%
  mutate(ext.lower_prop = seq(1:38)/38,
         site = "mad")

names(prop_degdec.mad2.mean.q)[2:4] <- c("ext.higher_prop","q25_prop","q75_prop")

ggplot(prop_degdec.mad2.mean.q, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2)




#Virgin
ext.degdec_null_vir2 = sapply(1:length(degdec_vir_sp2_null), function (x){degdec_vir_sp2_null[[x]][,3]})
mean.q.ext_deg.vir2 <- rowMeans(ext.degdec_null_vir2) %>% 
  cbind(matrixStats::rowQuantiles(ext.degdec_null_vir2, probs = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower")

names(mean.q.ext_deg.vir2)[2:4] <- c("Mean", "q25","q75")
mean.q.ext_deg.vir2$ext.lower <- as.numeric(mean.q.ext_deg.vir2$ext.lower)

mean.q.ext_deg.vir2$ext.higher_prop <- (628-cumsum(mean.q.ext_deg.vir2[,2]))/628
mean.q.ext_deg.vir2$ext.lower_prop <- mean.q.ext_deg.vir2[,1]/38
mean.q.ext_deg.vir2$q25_prop <- (628-cumsum(mean.q.ext_deg.vir2[,3]))/628
mean.q.ext_deg.vir2$q75_prop <- (628-cumsum(mean.q.ext_deg.vir2[,4]))/628
mean.q.ext_deg.vir2$site <- "vir2"

plot(mean.q.ext_deg.vir2[,1]/38, (628-cumsum(mean.q.ext_deg.vir2[,2]))/628) 

ggplot(mean.q.ext_deg.vir2, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2)


prop_degdec.vir2 <- apply(ext.degdec_null_vir2, 2, function(x) (628-cumsum(x))/628)
prop_degdec.vir2.mean.q <- rowMeans(prop_degdec.vir2) %>%
  cbind(matrixStats::rowQuantiles(prop_degdec.vir2, probs  = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower") %>%
  mutate(ext.lower_prop = seq(1:38)/38,
         site = "vir")

names(prop_degdec.vir2.mean.q)[2:4] <- c("ext.higher_prop","q25_prop","q75_prop")

ggplot(prop_degdec.vir2.mean.q, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2)



#Marshall
ext.degdec_null_mari2 = sapply(1:length(degdec_mari_sp2_null), function (x){degdec_mari_sp2_null[[x]][,3]})
mean.q.ext_deg.mari2 <- rowMeans(ext.degdec_null_mari2) %>% 
  cbind(matrixStats::rowQuantiles(ext.degdec_null_mari2, probs = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower")

names(mean.q.ext_deg.mari2)[2:4] <- c("Mean", "q25","q75")
mean.q.ext_deg.mari2$ext.lower <- as.numeric(mean.q.ext_deg.mari2$ext.lower)

mean.q.ext_deg.mari2$ext.higher_prop <- (628-cumsum(mean.q.ext_deg.mari2[,2]))/628
mean.q.ext_deg.mari2$ext.lower_prop <- mean.q.ext_deg.mari2[,1]/38
mean.q.ext_deg.mari2$q25_prop <- (628-cumsum(mean.q.ext_deg.mari2[,3]))/628
mean.q.ext_deg.mari2$q75_prop <- (628-cumsum(mean.q.ext_deg.mari2[,4]))/628
mean.q.ext_deg.mari2$site <- "mari2"

plot(mean.q.ext_deg.mari2[,1]/38, (628-cumsum(mean.q.ext_deg.mari2[,2]))/628) 

ggplot(mean.q.ext_deg.mari2, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2)


prop_degdec.mari2 <- apply(ext.degdec_null_mari2, 2, function(x) (628-cumsum(x))/628)
prop_degdec.mari2.mean.q <- rowMeans(prop_degdec.mari2) %>%
  cbind(matrixStats::rowQuantiles(prop_degdec.mari2, probs  = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower") %>%
  mutate(ext.lower_prop = seq(1:38)/38,
         site = "mari")

names(prop_degdec.mari2.mean.q)[2:4] <- c("ext.higher_prop","q25_prop","q75_prop")

ggplot(prop_degdec.mari2.mean.q, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2)



#Hawai
ext.degdec_null_haw2 = sapply(1:length(degdec_haw_sp2_null), function (x){degdec_haw_sp2_null[[x]][,3]})
mean.q.ext_deg.haw2 <- rowMeans(ext.degdec_null_haw2) %>% 
  cbind(matrixStats::rowQuantiles(ext.degdec_null_haw2, probs = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower")

names(mean.q.ext_deg.haw2)[2:4] <- c("Mean", "q25","q75")
mean.q.ext_deg.haw2$ext.lower <- as.numeric(mean.q.ext_deg.haw2$ext.lower)

mean.q.ext_deg.haw2$ext.higher_prop <- (628-cumsum(mean.q.ext_deg.haw2[,2]))/628
mean.q.ext_deg.haw2$ext.lower_prop <- mean.q.ext_deg.haw2[,1]/38
mean.q.ext_deg.haw2$q25_prop <- (628-cumsum(mean.q.ext_deg.haw2[,3]))/628
mean.q.ext_deg.haw2$q75_prop <- (628-cumsum(mean.q.ext_deg.haw2[,4]))/628
mean.q.ext_deg.haw2$site <- "haw2"

plot(mean.q.ext_deg.haw2[,1]/38, (628-cumsum(mean.q.ext_deg.haw2[,2]))/628) 

ggplot(mean.q.ext_deg.haw2, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2)


prop_degdec.haw2 <- apply(ext.degdec_null_haw2, 2, function(x) (628-cumsum(x))/628)
prop_degdec.haw2.mean.q <- rowMeans(prop_degdec.haw2) %>%
  cbind(matrixStats::rowQuantiles(prop_degdec.haw2, probs  = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower") %>%
  mutate(ext.lower_prop = seq(1:38)/38,
         site = "haw")

names(prop_degdec.haw2.mean.q)[2:4] <- c("ext.higher_prop","q25_prop","q75_prop")

ggplot(prop_degdec.haw2.mean.q, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2)



#New Caledonia
ext.degdec_null_nca2 = sapply(1:length(degdec_nca_sp2_null), function (x){degdec_nca_sp2_null[[x]][,3]})
mean.q.ext_deg.nca2 <- rowMeans(ext.degdec_null_nca2) %>% 
  cbind(matrixStats::rowQuantiles(ext.degdec_null_nca2, probs = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower")

names(mean.q.ext_deg.nca2)[2:4] <- c("Mean", "q25","q75")
mean.q.ext_deg.nca2$ext.lower <- as.numeric(mean.q.ext_deg.nca2$ext.lower)

mean.q.ext_deg.nca2$ext.higher_prop <- (628-cumsum(mean.q.ext_deg.nca2[,2]))/628
mean.q.ext_deg.nca2$ext.lower_prop <- mean.q.ext_deg.nca2[,1]/38
mean.q.ext_deg.nca2$q25_prop <- (628-cumsum(mean.q.ext_deg.nca2[,3]))/628
mean.q.ext_deg.nca2$q75_prop <- (628-cumsum(mean.q.ext_deg.nca2[,4]))/628
mean.q.ext_deg.nca2$site <- "nca2"

plot(mean.q.ext_deg.nca2[,1]/38, (628-cumsum(mean.q.ext_deg.nca2[,2]))/628) 

ggplot(mean.q.ext_deg.nca2, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2)


prop_degdec.nca2 <- apply(ext.degdec_null_nca2, 2, function(x) (628-cumsum(x))/628)
prop_degdec.nca2.mean.q <- rowMeans(prop_degdec.nca2) %>%
  cbind(matrixStats::rowQuantiles(prop_degdec.nca2, probs  = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower") %>%
  mutate(ext.lower_prop = seq(1:38)/38,
         site = "nca")

names(prop_degdec.nca2.mean.q)[2:4] <- c("ext.higher_prop","q25_prop","q75_prop")

ggplot(prop_degdec.nca2.mean.q, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2)


#Dataframe with null distributions and observed values
mean.q.ext_degdec2 <- rbind(mean.q.ext_deg.haw, mean.q.ext_deg.mad, mean.q.ext_deg.mari, mean.q.ext_deg.nca, mean.q.ext_deg.vir) %>% mutate (type = "null")

obs_ext.degdec <- ex_degdec_vir_sp[,3] %>% data.frame(.) %>% mutate(ext.higher_prop = (628-cumsum(ex_degdec_vir_sp[,3]))/628, 
                                                                    ext.lower_prop = ex_degdec_vir_sp[,1]/38,
                                                                    site ="vir") %>% 
  rbind(ex_degdec_mari_sp[,3] %>% data.frame(.) %>% mutate(ext.higher_prop = (628-cumsum(ex_degdec_mari_sp[,3]))/628, 
                                                           ext.lower_prop = ex_degdec_mari_sp[,1]/38,
                                                           site ="mari")) %>%
  rbind(ex_degdec_mad_sp[,3] %>% data.frame(.) %>% mutate(ext.higher_prop = (628-cumsum(ex_degdec_mad_sp[,3]))/628, 
                                                          ext.lower_prop = ex_degdec_mad_sp[,1]/38,
                                                          site ="mad")) %>%
  rbind(ex_degdec_nca_sp[,3] %>% data.frame(.) %>% mutate(ext.higher_prop = (628-cumsum(ex_degdec_nca_sp[,3]))/628, 
                                                          ext.lower_prop = ex_degdec_nca_sp[,1]/38,
                                                          site ="nca")) %>%
  rbind(ex_degdec_haw_sp[,3] %>% data.frame(.) %>% mutate(ext.higher_prop = (628-cumsum(ex_degdec_haw_sp[,3]))/628, 
                                                          ext.lower_prop = ex_degdec_haw_sp[,1]/38,
                                                          site ="haw"))
names(obs_ext.degdec)[1] <- "ext.higher_nb"



obs_ext.degdec <- mutate(obs_ext.degdec, type ="obs")
ext.degdec_full2 <- rbind.fill(mean.q.ext_degdec, obs_ext.degdec)

prop_mean.q.deg2 <- rbind(prop_degdec.haw2.mean.q, prop_degdec.mad2.mean.q, prop_degdec.mari2.mean.q, prop_degdec.vir2.mean.q, prop_degdec.nca2.mean.q) %>% mutate(type ="null")
prop_ext.deg_full2 <- rbind.fill(prop_mean.q.deg2, obs_ext.degdec)


#Grouped plot
mean.q.ext_degdec$site <- as.factor(mean.q.ext_degdec$site)

ggplot(prop_ext.deg_full2, aes(x=ext.lower_prop, y=ext.higher_prop)) + 
  geom_ribbon(data = subset(prop_ext.deg_full2, type %in% "null"), aes(ymin=q25_prop, ymax=q75_prop, fill = site), alpha=0.2) +
  geom_line(aes(linetype = type, color = site)) + 
  scale_color_manual(name = "Regions",labels = c("Hawaï","Madagascar", "Marshall Islands", "New Caledonia", "Virgin Islands"), values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  scale_fill_manual(name = "Regions", labels = c("Hawaï","Madagascar", "Marshall Islands", "New Caledonia", "Virgin Islands"), values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  scale_linetype_manual(name = "", labels = (c("Null distribution", "Observed")), values = c(1,2)) +
  labs( x= "Proportions of primary extinctions - Preys", y="Proportions of predators still alive") +
  ggtitle( "b) Decreasing degree connectivity extinction") +
  theme_classic() 



#Facet plot of each regions both scenarios
prop_ext.ran_full2 $ext <- "Random"
prop_ext.deg_full2$ext <- "Decreasing nodes connectivity"

prop_ext_full2 <- rbind(prop_ext.ran_full2, prop_ext.deg_full2)

ggplot(prop_ext_full2, aes(x=ext.lower_prop, y=ext.higher_prop)) + 
  geom_ribbon(data = subset(prop_ext_full2, type %in% "null"), aes(ymin=q25_prop, ymax=q75_prop, fill = site), alpha=0.2) +
  geom_line(aes(linetype = type, color = site)) + 
  scale_linetype_manual(name = "", labels = (c("Null distribution", "Observed")), values = c(1,2)) +
  scale_color_manual(name = "Regions",labels = c("Hawaï","Madagascar", "Marshall Islands", "New Caledonia", "Virgin Islands"), values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  scale_fill_manual(name = "Regions", labels = c("Hawaï","Madagascar", "Marshall Islands", "New Caledonia", "Virgin Islands"), values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  labs( x= "Proportions of primary extinctions - Preys", y="Proportions of predators still alive") +
  facet_grid(site~ext) +
  theme(strip.text.x = element_text(size=9, face="bold"),
        strip.text.y = element_blank(),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(colour = "black", 
                                   size=0.5, 
                                   lineend = "butt"),
        axis.line.y = element_line(colour = "black", 
                                   size=0.5)) 


#3. With increasing degree --> from the least to most connected #####

#Load corrected fonction to have degree from the least to most connected nodes
source("script/extinction_corrected.R")
source("script/second.extinct_corrected.R")

#Error coz of argument count = TRUE, present in the second.extinct function

ex_deginc_vir_sp <- second.extinct_corrected(vir_ISmatrix_sp2, participant = "lower", method="degree", nrep=100, details=FALSE)
robustness(ex_deginc_vir_sp)
slope.bipartite(ex_deginc_vir_sp)

ex_deginc_mari_sp <- second.extinct_corrected(mari_ISmatrix_sp2, participant = "lower", method="degree", nrep=100, details=FALSE)
robustness(ex_deginc_mari_sp)
slope.bipartite(ex_deginc_mari_sp)

ex_deginc_haw_sp <- second.extinct_corrected(haw_ISmatrix_sp2, participant = "lower", method="degree", nrep=100, details=FALSE)
robustness(ex_deginc_haw_sp)
slope.bipartite(ex_deginc_haw_sp)

ex_deginc_mad_sp <- second.extinct_corrected(mad_ISmatrix_sp2, participant = "lower", method="degree", nrep=100, details=FALSE)
robustness(ex_deginc_mad_sp)
slope.bipartite(ex_deginc_mad_sp)

ex_deginc_nca_sp <- second.extinct_corrected(nca_ISmatrix_sp2, participant = "lower", method="degree", nrep=100, details=FALSE)
robustness(ex_deginc_nca_sp)
slope.bipartite(ex_deginc_nca_sp)




