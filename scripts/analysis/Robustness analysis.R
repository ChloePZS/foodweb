############
#Robustness#
############

    #1. To random extinction####
ex_ran_vir_sp <- second.extinct(vir_ISmatrix_sp2, participant = "lower", method="random", nrep=1000, details=FALSE) #when set to true, returns a list of the nrep repetitions
robustness(ex_ran_vir_sp)
slope.bipartite(ex_ran_vir_sp)

ex_ran_vir_sp[[2]]

ex_ran_mari_sp <- second.extinct(mari_ISmatrix_sp2, participant = "lower", method="random", nrep=1000, details=TRUE)
robustness(ex_ran_mari_sp)
slope.bipartite(ex_ran_mari_sp) #Can't work if details = FALSE

ex_ran_haw_sp <- second.extinct(haw_ISmatrix_sp2, participant = "lower", method="random", nrep=1000, details=TRUE)
robustness(ex_ran_haw_sp)
slope.bipartite(ex_ran_haw_sp)

ex_ran_mad_sp <- second.extinct(mad_ISmatrix_sp2, participant = "lower", method="random", nrep=1000, details=TRUE)
robustness(ex_ran_mad_sp)
slope.bipartite(ex_ran_mad_sp)

ex_ran_nca_sp <- second.extinct(nca_ISmatrix_sp2, participant = "lower", method="random", nrep=1000, details=TRUE)
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
rob_vir_sp_null <- lapply(rand_vir_sp, second.extinct, participant = "lower", method="random", nrep=10, details=FALSE)
rob_mari_sp_null <- lapply(rand_mari_sp, second.extinct, participant = "lower", method="random", nrep=10, details=FALSE)
rob_nca_sp_null <- lapply(rand_nca_sp, second.extinct, participant = "lower", method="random", nrep=10, details=FALSE)
rob_haw_sp_null <- lapply(rand_haw_sp, second.extinct, participant = "lower", method="random", nrep=10, details=FALSE)
rob_mad_sp_null <- lapply(rand_mad_sp, second.extinct, participant = "lower", method="random", nrep=10, details=FALSE)


#Get extinction curves
#Madagascar
ext.ran_null_mad = sapply(1:length(rob_mad_sp_null), function (x){rob_mad_sp_null[[x]][,3]})
mean.q.ext.mad <- rowMeans(ext.ran_null_mad) %>% 
  cbind(matrixStats::rowQuantiles(ext.ran_null_mad, probs = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower")

names(mean.q.ext.mad)[2:4] <- c("Mean", "q25","q75")
mean.q.ext.mad$ext.lower <- as.numeric(mean.q.ext.mad$ext.lower)


mean.q.ext.mad$ext.higher_prop <- (628-cumsum(mean.q.ext.mad[,2]))/628
mean.q.ext.mad$ext.lower_prop <- mean.q.ext.mad[,1]/38
mean.q.ext.mad$q25_prop <- (628-cumsum(mean.q.ext.mad[,3]))/628
mean.q.ext.mad$q75_prop <- (628-cumsum(mean.q.ext.mad[,4]))/628
mean.q.ext.mad$site <- "mad"

plot(mean.q.ext.mad[,1]/38, (628-cumsum(mean.q.ext.mad[,2]))/628) 

ggplot(mean.q.ext.mad, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2)


prop_mad <- apply(ext.ran_null_mad, 2, function(x) (628-cumsum(x))/628)
prop_mad.mean.q <- rowMeans(prop_mad) %>%
  cbind(matrixStats::rowQuantiles(prop_mad, probs  = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower") %>%
  mutate(ext.lower_prop = seq(1:38)/38,
         site = 'mad')

names(prop_mad.mean.q)[2:4] <- c("ext.higher_prop","q25_prop","q75_prop")

ggplot(prop_mad.mean.q, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2)


#Virgin
ext.ran_null_vir = sapply(1:length(rob_vir_sp_null), function (x){rob_vir_sp_null[[x]][,3]})
mean.q.ext.vir <- rowMeans(ext.ran_null_vir) %>% 
  cbind(matrixStats::rowQuantiles(ext.ran_null_vir, probs = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower")

names(mean.q.ext.vir)[2:4] <- c("Mean", "q25","q75")
mean.q.ext.vir$ext.lower <- as.numeric(mean.q.ext.vir$ext.lower)

mean.q.ext.vir$ext.higher_prop <- (628-cumsum(mean.q.ext.vir[,2]))/628
mean.q.ext.vir$ext.lower_prop <- mean.q.ext.vir[,1]/38
mean.q.ext.vir$q25_prop <- (628-cumsum(mean.q.ext.vir[,3]))/628
mean.q.ext.vir$q75_prop <- (628-cumsum(mean.q.ext.vir[,4]))/628
mean.q.ext.vir$site <- "vir"

plot(mean.q.ext.vir[,1]/38, (628-cumsum(mean.q.ext.vir[,2]))/628) 

ggplot(mean.q.ext.vir, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2)


prop_vir <- apply(ext.ran_null_vir, 2, function(x) (628-cumsum(x))/628)
prop_vir.mean.q <- rowMeans(prop_vir) %>%
  cbind(matrixStats::rowQuantiles(prop_vir, probs  = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower") %>%
  mutate(ext.lower_prop = seq(1:38)/38,
         site = 'vir')

names(prop_vir.mean.q)[2:4] <- c("ext.higher_prop","q25_prop","q75_prop")

ggplot(prop_vir.mean.q, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2)


#Marshall
ext.ran_null_mari = sapply(1:length(rob_mari_sp_null), function (x){rob_mari_sp_null[[x]][,3]})
mean.q.ext.mari <- rowMeans(ext.ran_null_mari) %>% 
  cbind(matrixStats::rowQuantiles(ext.ran_null_mari, probs = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower")

names(mean.q.ext.mari)[2:4] <- c("Mean", "q25","q75")
mean.q.ext.mari$ext.lower <- as.numeric(mean.q.ext.mari$ext.lower)

mean.q.ext.mari$ext.higher_prop <- (628-cumsum(mean.q.ext.mari[,2]))/628
mean.q.ext.mari$ext.lower_prop <- mean.q.ext.mari[,1]/38
mean.q.ext.mari$q25_prop <- (628-cumsum(mean.q.ext.mari[,3]))/628
mean.q.ext.mari$q75_prop <- (628-cumsum(mean.q.ext.mari[,4]))/628
mean.q.ext.mari$site <- "mari"

plot(mean.q.ext.mari[,1]/38, (628-cumsum(mean.q.ext.mari[,2]))/628) 

ggplot(mean.q.ext.mari, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2)


prop_mari <- apply(ext.ran_null_mari, 2, function(x) (628-cumsum(x))/628)
prop_mari.mean.q <- rowMeans(prop_mari) %>%
  cbind(matrixStats::rowQuantiles(prop_mari, probs  = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower") %>%
  mutate(ext.lower_prop = seq(1:38)/38,
         site = 'mari')

names(prop_mari.mean.q)[2:4] <- c("ext.higher_prop","q25_prop","q75_prop")

ggplot(prop_mari.mean.q, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2)


#Hawai
ext.ran_null_haw = sapply(1:length(rob_haw_sp_null), function (x){rob_haw_sp_null[[x]][,3]})
mean.q.ext.haw <- rowMeans(ext.ran_null_haw) %>% 
  cbind(matrixStats::rowQuantiles(ext.ran_null_haw, probs = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower")

names(mean.q.ext.haw)[2:4] <- c("Mean", "q25","q75")
mean.q.ext.haw$ext.lower <- as.numeric(mean.q.ext.haw$ext.lower)

mean.q.ext.haw$ext.higher_prop <- (628-cumsum(mean.q.ext.haw[,2]))/628
mean.q.ext.haw$ext.lower_prop <- mean.q.ext.haw[,1]/38
mean.q.ext.haw$q25_prop <- (628-cumsum(mean.q.ext.haw[,3]))/628
mean.q.ext.haw$q75_prop <- (628-cumsum(mean.q.ext.haw[,4]))/628
mean.q.ext.haw$site <- "haw"

plot(mean.q.ext.haw[,1]/38, (628-cumsum(mean.q.ext.haw[,2]))/628) 

ggplot(mean.q.ext.haw, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2)


prop_haw <- apply(ext.ran_null_haw, 2, function(x) (628-cumsum(x))/628)
prop_haw.mean.q <- rowMeans(prop_haw) %>%
  cbind(matrixStats::rowQuantiles(prop_haw, probs  = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower") %>%
  mutate(ext.lower_prop = seq(1:38)/38,
         site = 'haw')

names(prop_haw.mean.q)[2:4] <- c("ext.higher_prop","q25_prop","q75_prop")

ggplot(prop_haw.mean.q, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2)


#New Caledonia
ext.ran_null_nca = sapply(1:length(rob_nca_sp_null), function (x){rob_nca_sp_null[[x]][,3]})
mean.q.ext.nca <- rowMeans(ext.ran_null_nca) %>% 
  cbind(matrixStats::rowQuantiles(ext.ran_null_nca, probs = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower")

names(mean.q.ext.nca)[2:4] <- c("Mean", "q25","q75")
mean.q.ext.nca$ext.lower <- as.numeric(mean.q.ext.nca$ext.lower)

mean.q.ext.nca$ext.higher_prop <- (628-cumsum(mean.q.ext.nca[,2]))/628
mean.q.ext.nca$ext.lower_prop <- mean.q.ext.nca[,1]/38
mean.q.ext.nca$q25_prop <- (628-cumsum(mean.q.ext.nca[,3]))/628
mean.q.ext.nca$q75_prop <- (628-cumsum(mean.q.ext.nca[,4]))/628
mean.q.ext.nca$site <- "nca"

plot(mean.q.ext.nca[,1]/38, (628-cumsum(mean.q.ext.nca[,2]))/628) 

ggplot(mean.q.ext.nca, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2)


prop_nca <- apply(ext.ran_null_nca, 2, function(x) (628-cumsum(x))/628)
prop_nca.mean.q <- rowMeans(prop_nca) %>%
  cbind(matrixStats::rowQuantiles(prop_nca, probs  = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower") %>%
  mutate(ext.lower_prop = seq(1:38)/38,
         site = 'nca')

names(prop_nca.mean.q)[2:4] <- c("ext.higher_prop","q25_prop","q75_prop")

ggplot(prop_nca.mean.q, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2)


#Dataframe with null distributions and observed values
mean.q.ext.ran <- rbind(mean.q.ext.haw, mean.q.ext.mad, mean.q.ext.mari, mean.q.ext.nca, mean.q.ext.vir) %>% mutate (type = "null")
obs_ext.ran <- mutate(obs_ext.ran, type ="obs")
ext.ran_full <- rbind.fill(mean.q.ext.ran, obs_ext.ran)

prop_mean.q.ext.ran <- rbind(prop_haw.mean.q, prop_mad.mean.q, prop_mari.mean.q, prop_nca.mean.q, prop_vir.mean.q) %>% mutate (type = "null")
prop_ext.ran_full <-rbind.fill(prop_mean.q.ext.ran, obs_ext.ran)

#Grouped plot
mean.q.ext.ran$site <- as.factor(mean.q.ext.ran$site)
ggplot(ext.ran_full, aes(x=ext.lower_prop, y=ext.higher_prop)) + 
  geom_ribbon(data = subset(ext.ran_full, type %in% "null"), aes(ymin=q25_prop, ymax=q75_prop, fill = site), alpha=0.2) +
  geom_line(aes(linetype = type, color = site)) + 
  scale_color_manual(name = "Regions",labels = c("Hawaï","Madagascar", "Marshall Islands", "New Caledonia", "Virgin Islands"), values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  scale_fill_manual(name = "Regions", labels = c("Hawaï","Madagascar", "Marshall Islands", "New Caledonia", "Virgin Islands"), values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  scale_linetype_manual(name = "", labels = (c("Null distribution", "Observed")), values = c(1,2)) +
  labs( x= "Proportions of primary extinctions - Preys", y="Proportions of predators still alive") +
  ggtitle( "a) Random extinction") +
  theme_classic() 



#With corrected IC
mean.q.ext.ran$site <- as.factor(mean.q.ext.ran$site)
ggplot(prop_ext.ran_full, aes(x=ext.lower_prop, y=ext.higher_prop)) + 
  geom_ribbon(data = subset(prop_ext.ran_full, type %in% "null"), aes(ymin=q25_prop, ymax=q75_prop, fill = site), alpha=0.2) +
  geom_line(aes(linetype = type, color = site)) + 
  scale_color_manual(name = "Regions",labels = c("Hawaï","Madagascar", "Marshall Islands", "New Caledonia", "Virgin Islands"), values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  scale_fill_manual(name = "Regions", labels = c("Hawaï","Madagascar", "Marshall Islands", "New Caledonia", "Virgin Islands"), values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  scale_linetype_manual(name = "", labels = (c("Null distribution", "Observed")), values = c(1,2)) +
  labs( x= "Proportions of primary extinctions - Preys", y="Proportions of predators still alive") +
  ggtitle( "a) Random extinction") +
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
degdec_vir_sp_null <- lapply(rand_vir_sp, second.extinct, participant = "lower", method="degree", nrep=10, details=FALSE)
degdec_mari_sp_null <- lapply(rand_mari_sp, second.extinct, participant = "lower", method="degree", nrep=10, details=FALSE)
degdec_nca_sp_null <- lapply(rand_nca_sp, second.extinct, participant = "lower", method="degree", nrep=10, details=FALSE)
degdec_haw_sp_null <- lapply(rand_haw_sp, second.extinct, participant = "lower", method="degree", nrep=10, details=FALSE)
degdec_mad_sp_null <- lapply(rand_mad_sp, second.extinct, participant = "lower", method="degree", nrep=10, details=FALSE)


#Extinction curves

rm(mean.q.ext_deg.mad)
#Madagascar
ext.degdec_null_mad = sapply(1:length(degdec_mad_sp_null), function (x){degdec_mad_sp_null[[x]][,3]})
mean.q.ext_deg.mad <- rowMeans(ext.degdec_null_mad) %>% 
  cbind(matrixStats::rowQuantiles(ext.degdec_null_mad, probs  = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower")

names(mean.q.ext_deg.mad)[2:4] <- c("Mean", "q25","q75")
mean.q.ext_deg.mad$ext.lower <- as.numeric(mean.q.ext_deg.mad$ext.lower)


mean.q.ext_deg.mad$ext.higher_prop <- (628-cumsum(mean.q.ext_deg.mad[,2]))/628
mean.q.ext_deg.mad$ext.lower_prop <- mean.q.ext_deg.mad[,1]/38
mean.q.ext_deg.mad$q25_prop <- (628-cumsum(mean.q.ext_deg.mad[,3]))/628
mean.q.ext_deg.mad$q75_prop <- (628-cumsum(mean.q.ext_deg.mad[,4]))/628
mean.q.ext_deg.mad$site <- "mad"

plot(mean.q.ext_deg.mad[,1]/38, (628-cumsum(mean.q.ext_deg.mad[,2]))/628) 

ggplot(mean.q.ext_deg.mad, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2)


prop_deg_mad <- apply(ext.degdec_null_mad, 2, function(x) (628-cumsum(x))/628)
prop_deg_mad.mean.q <- rowMeans(prop_deg_mad) %>%
  cbind(matrixStats::rowQuantiles(prop_deg_mad, probs  = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower") %>%
  mutate(ext.lower_prop = seq(1:38)/38,
         site = 'mad')

names(prop_deg_mad.mean.q)[2:4] <- c("ext.higher_prop","q25_prop","q75_prop")

ggplot(prop_deg_mad.mean.q, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2)


#Virgin
ext.degdec_null_vir = sapply(1:length(degdec_vir_sp_null), function (x){degdec_vir_sp_null[[x]][,3]})
mean.q.ext_deg.vir <- rowMeans(ext.degdec_null_vir) %>% 
  cbind(matrixStats::rowQuantiles(ext.degdec_null_vir, probs = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower")

names(mean.q.ext_deg.vir)[2:4] <- c("Mean", "q25","q75")
mean.q.ext_deg.vir$ext.lower <- as.numeric(mean.q.ext_deg.vir$ext.lower)

mean.q.ext_deg.vir$ext.higher_prop <- (628-cumsum(mean.q.ext_deg.vir[,2]))/628
mean.q.ext_deg.vir$ext.lower_prop <- mean.q.ext_deg.vir[,1]/38
mean.q.ext_deg.vir$q25_prop <- (628-cumsum(mean.q.ext_deg.vir[,3]))/628
mean.q.ext_deg.vir$q75_prop <- (628-cumsum(mean.q.ext_deg.vir[,4]))/628
mean.q.ext_deg.vir$site <- "vir"

plot(mean.q.ext_deg.vir[,1]/38, (628-cumsum(mean.q.ext_deg.vir[,2]))/628) 

ggplot(mean.q.ext_deg.vir, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2)


prop_deg_vir <- apply(ext.degdec_null_vir, 2, function(x) (628-cumsum(x))/628)
prop_deg_vir.mean.q <- rowMeans(prop_deg_vir) %>%
  cbind(matrixStats::rowQuantiles(prop_deg_vir, probs  = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower") %>%
  mutate(ext.lower_prop = seq(1:38)/38,
         site = 'vir')

names(prop_deg_vir.mean.q)[2:4] <- c("ext.higher_prop","q25_prop","q75_prop")

ggplot(prop_deg_vir.mean.q, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2)


#Marshall
ext.degdec_null_mari = sapply(1:length(degdec_mari_sp_null), function (x){degdec_mari_sp_null[[x]][,3]})
mean.q.ext_deg.mari <- rowMeans(ext.degdec_null_mari) %>% 
  cbind(matrixStats::rowQuantiles(ext.degdec_null_mari, probs = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower")

names(mean.q.ext_deg.mari)[2:4] <- c("Mean", "q25","q75")
mean.q.ext_deg.mari$ext.lower <- as.numeric(mean.q.ext_deg.mari$ext.lower)

mean.q.ext_deg.mari$ext.higher_prop <- (628-cumsum(mean.q.ext_deg.mari[,2]))/628
mean.q.ext_deg.mari$ext.lower_prop <- mean.q.ext_deg.mari[,1]/38
mean.q.ext_deg.mari$q25_prop <- (628-cumsum(mean.q.ext_deg.mari[,3]))/628
mean.q.ext_deg.mari$q75_prop <- (628-cumsum(mean.q.ext_deg.mari[,4]))/628
mean.q.ext_deg.mari$site <- "mari"

plot(mean.q.ext_deg.mari[,1]/38, (628-cumsum(mean.q.ext_deg.mari[,2]))/628) 

ggplot(mean.q.ext_deg.mari, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2)


prop_deg_mari <- apply(ext.degdec_null_mari, 2, function(x) (628-cumsum(x))/628)
prop_deg_mari.mean.q <- rowMeans(prop_deg_mari) %>%
  cbind(matrixStats::rowQuantiles(prop_deg_mari, probs  = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower") %>%
  mutate(ext.lower_prop = seq(1:38)/38,
         site = 'mari')

names(prop_deg_mari.mean.q)[2:4] <- c("ext.higher_prop","q25_prop","q75_prop")

ggplot(prop_deg_mari.mean.q, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2)


#Hawai
ext.degdec_null_haw = sapply(1:length(degdec_haw_sp_null), function (x){degdec_haw_sp_null[[x]][,3]})
mean.q.ext_deg.haw <- rowMeans(ext.degdec_null_haw) %>% 
  cbind(matrixStats::rowQuantiles(ext.degdec_null_haw, probs = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower")

names(mean.q.ext_deg.haw)[2:4] <- c("Mean", "q25","q75")
mean.q.ext_deg.haw$ext.lower <- as.numeric(mean.q.ext_deg.haw$ext.lower)

mean.q.ext_deg.haw$ext.higher_prop <- (628-cumsum(mean.q.ext_deg.haw[,2]))/628
mean.q.ext_deg.haw$ext.lower_prop <- mean.q.ext_deg.haw[,1]/38
mean.q.ext_deg.haw$q25_prop <- (628-cumsum(mean.q.ext_deg.haw[,3]))/628
mean.q.ext_deg.haw$q75_prop <- (628-cumsum(mean.q.ext_deg.haw[,4]))/628
mean.q.ext_deg.haw$site <- "haw"

plot(mean.q.ext_deg.haw[,1]/38, (628-cumsum(mean.q.ext_deg.haw[,2]))/628) 

ggplot(mean.q.ext_deg.haw, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2)



prop_deg_haw <- apply(ext.degdec_null_haw, 2, function(x) (628-cumsum(x))/628)
prop_deg_haw.mean.q <- rowMeans(prop_deg_haw) %>%
  cbind(matrixStats::rowQuantiles(prop_deg_haw, probs  = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower") %>%
  mutate(ext.lower_prop = seq(1:38)/38,
         site = 'haw')

names(prop_deg_haw.mean.q)[2:4] <- c("ext.higher_prop","q25_prop","q75_prop")

ggplot(prop_deg_haw.mean.q, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2)

#New Caledonia
ext.degdec_null_nca = sapply(1:length(degdec_nca_sp_null), function (x){degdec_nca_sp_null[[x]][,3]})
mean.q.ext_deg.nca <- rowMeans(ext.degdec_null_nca) %>% 
  cbind(matrixStats::rowQuantiles(ext.degdec_null_nca, probs = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower")

names(mean.q.ext_deg.nca)[2:4] <- c("Mean", "q25","q75")
mean.q.ext_deg.nca$ext.lower <- as.numeric(mean.q.ext_deg.nca$ext.lower)

mean.q.ext_deg.nca$ext.higher_prop <- (628-cumsum(mean.q.ext_deg.nca[,2]))/628
mean.q.ext_deg.nca$ext.lower_prop <- mean.q.ext_deg.nca[,1]/38
mean.q.ext_deg.nca$q25_prop <- (628-cumsum(mean.q.ext_deg.nca[,3]))/628
mean.q.ext_deg.nca$q75_prop <- (628-cumsum(mean.q.ext_deg.nca[,4]))/628
mean.q.ext_deg.nca$site <- "nca"

plot(mean.q.ext_deg.nca[,1]/38, (628-cumsum(mean.q.ext_deg.nca[,2]))/628) 

ggplot(mean.q.ext_deg.nca, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2)


prop_deg_nca <- apply(ext.degdec_null_nca, 2, function(x) (628-cumsum(x))/628)
prop_deg_nca.mean.q <- rowMeans(prop_deg_nca) %>%
  cbind(matrixStats::rowQuantiles(prop_deg_nca, probs  = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower") %>%
  mutate(ext.lower_prop = seq(1:38)/38,
         site = 'nca')

names(prop_deg_nca.mean.q)[2:4] <- c("ext.higher_prop","q25_prop","q75_prop")

ggplot(prop_deg_nca.mean.q, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2)


#Dataframe with null distributions and observed values
mean.q.ext_degdec <- rbind(mean.q.ext_deg.haw, mean.q.ext_deg.mad, mean.q.ext_deg.mari, mean.q.ext_deg.nca, mean.q.ext_deg.vir) %>% mutate (type = "null")

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
ext.degdec_full <- rbind.fill(mean.q.ext_degdec, obs_ext.degdec)

prop_mean.q.ext_degdec <- rbind(prop_deg_haw.mean.q, prop_deg_mad.mean.q, prop_deg_mari.mean.q, prop_deg_nca.mean.q,prop_deg_vir.mean.q) %>% mutate (type = "null")
prop_ext.degdec_full <- rbind.fill(prop_mean.q.ext_degdec, obs_ext.degdec)


#Grouped plot
mean.q.ext_degdec$site <- as.factor(mean.q.ext_degdec$site)

ggplot(prop_ext.degdec_full, aes(x=ext.lower_prop, y=ext.higher_prop)) + 
  geom_ribbon(data = subset(prop_ext.degdec_full, type %in% "null"), aes(ymin=q25_prop, ymax=q75_prop, fill = site), alpha=0.2) +
  geom_line(aes(linetype = type, color = site)) + 
  scale_color_manual(name = "Regions",labels = c("Hawaï","Madagascar", "Marshall Islands", "New Caledonia", "Virgin Islands"), values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  scale_fill_manual(name = "Regions", labels = c("Hawaï","Madagascar", "Marshall Islands", "New Caledonia", "Virgin Islands"), values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  scale_linetype_manual(name = "", labels = (c("Null distribution", "Observed")), values = c(1,2)) +
  labs( x= "Proportions of primary extinctions - Preys", y="Proportions of predators still alive") +
  ggtitle( "b) Decreasing degree connectivity extinction") +
  facet_grid(site ~.)+
  theme_classic() 



#Facet plot of each regions both scenarios
prop_ext.ran_full$ext <- "Random"
prop_ext.degdec_full$ext <- "Decreasing nodes connectivity"

prop_ext_full <- rbind(prop_ext.ran_full, prop_ext.degdec_full)

ggplot(prop_ext_full, aes(x=ext.lower_prop, y=ext.higher_prop)) + 
  geom_ribbon(data = subset(prop_ext_full, type %in% "null"), aes(ymin=q25_prop, ymax=q75_prop, fill = site), alpha=0.2) +
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



  #4. Get the extinction curves mean and quantiles of the observed simulations ####
#Madagascar
ex_ran_mad_sp.dist = sapply(1:length(ex_ran_mad_sp), function (x){ex_ran_mad_sp[[x]][,3]})
mean.q.ext_ran.mad <- rowMeans(ex_ran_mad_sp.dist) %>% 
  cbind(matrixStats::rowQuantiles(ex_ran_mad_sp.dist, probs  = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower")

names(ex_ran_mad_sp.dist)[2:4] <- c("Mean", "q25","q75")
mean.q.ext_ran.mad$ext.lower <- as.numeric(mean.q.ext_ran.mad$ext.lower)

prop_ran_mad.obs <- apply(ex_ran_mad_sp.dist, 2, function(x) (628-cumsum(x))/628) #get the cumulative sums of the proportions species remaining alive

mean.q.ext_ran.mad <- mean.q.ext_ran.mad %>% mutate(ext.higher_prop = rowMeans(prop_ran_mad.obs),
                                                    ext.lower_prop = seq(1:38)/38,
                                                    site = 'mad') %>%
  cbind(matrixStats::rowQuantiles(prop_ran_mad.obs, probs  = c(0.25,0.75))) %>% data.frame(.)

names(mean.q.ext_ran.mad)[c(2,8,9)] <- c("ext.higher","q25_prop","q75_prop")

ggplot(mean.q.ext_ran.mad, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2)



#Virgin Islands
ex_ran_vir_sp.dist = sapply(1:length(ex_ran_vir_sp), function (x){ex_ran_vir_sp[[x]][,3]})
mean.q.ext_ran.vir <- rowMeans(ex_ran_vir_sp.dist) %>% 
  cbind(matrixStats::rowQuantiles(ex_ran_vir_sp.dist, probs  = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower")

names(ex_ran_vir_sp.dist)[2:4] <- c("Mean", "q25","q75")
mean.q.ext_ran.vir$ext.lower <- as.numeric(mean.q.ext_ran.vir$ext.lower)

prop_ran_vir.obs <- apply(ex_ran_vir_sp.dist, 2, function(x) (628-cumsum(x))/628) #get the cumulative sums of the proportions species remaining alive

mean.q.ext_ran.vir <- mean.q.ext_ran.vir %>% mutate(ext.higher_prop = rowMeans(prop_ran_vir.obs),
                                                    ext.lower_prop = seq(1:38)/38,
                                                    site = 'vir') %>%
  cbind(matrixStats::rowQuantiles(prop_ran_vir.obs, probs  = c(0.25,0.75))) %>% data.frame(.)

names(mean.q.ext_ran.vir)[c(2,8,9)] <- c("ext.higher","q25_prop","q75_prop")

ggplot(mean.q.ext_ran.vir, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2)

#Marshall Islands
ex_ran_mari_sp.dist = sapply(1:length(ex_ran_mari_sp), function (x){ex_ran_mari_sp[[x]][,3]})
mean.q.ext_ran.mari <- rowMeans(ex_ran_mari_sp.dist) %>% 
  cbind(matrixStats::rowQuantiles(ex_ran_mari_sp.dist, probs  = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower")

names(ex_ran_mari_sp.dist)[2:4] <- c("Mean", "q25","q75")
mean.q.ext_ran.mari$ext.lower <- as.numeric(mean.q.ext_ran.mari$ext.lower)

prop_ran_mari.obs <- apply(ex_ran_mari_sp.dist, 2, function(x) (628-cumsum(x))/628) #get the cumulative sums of the proportions species remaining alive

mean.q.ext_ran.mari <- mean.q.ext_ran.mari %>% mutate(ext.higher_prop = rowMeans(prop_ran_mari.obs),
                                                    ext.lower_prop = seq(1:38)/38,
                                                    site = 'mari') %>%
  cbind(matrixStats::rowQuantiles(prop_ran_mari.obs, probs  = c(0.25,0.75))) %>% data.frame(.)

names(mean.q.ext_ran.mari)[c(2,8,9)] <- c("ext.higher","q25_prop","q75_prop")

ggplot(mean.q.ext_ran.mari, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2)


#Hawai
ex_ran_haw_sp.dist = sapply(1:length(ex_ran_haw_sp), function (x){ex_ran_haw_sp[[x]][,3]})
mean.q.ext_ran.haw <- rowMeans(ex_ran_haw_sp.dist) %>% 
  cbind(matrixStats::rowQuantiles(ex_ran_haw_sp.dist, probs  = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower")

names(ex_ran_haw_sp.dist)[2:4] <- c("Mean", "q25","q75")
mean.q.ext_ran.haw$ext.lower <- as.numeric(mean.q.ext_ran.haw$ext.lower)

prop_ran_haw.obs <- apply(ex_ran_haw_sp.dist, 2, function(x) (628-cumsum(x))/628) #get the cumulative sums of the proportions species remaining alive

mean.q.ext_ran.haw <- mean.q.ext_ran.haw %>% mutate(ext.higher_prop = rowMeans(prop_ran_haw.obs),
                                                    ext.lower_prop = seq(1:38)/38,
                                                    site = 'haw') %>%
  cbind(matrixStats::rowQuantiles(prop_ran_haw.obs, probs  = c(0.25,0.75))) %>% data.frame(.)

names(mean.q.ext_ran.haw)[c(2,8,9)] <- c("ext.higher","q25_prop","q75_prop")

ggplot(mean.q.ext_ran.haw, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2)



#New Caledonia
ex_ran_nca_sp.dist = sapply(1:length(ex_ran_nca_sp), function (x){ex_ran_nca_sp[[x]][,3]})
mean.q.ext_ran.nca <- rowMeans(ex_ran_nca_sp.dist) %>% 
  cbind(matrixStats::rowQuantiles(ex_ran_nca_sp.dist, probs  = c(0.25,0.75))) %>% data.frame(.) %>%
  rownames_to_column(., var = "ext.lower")

names(ex_ran_nca_sp.dist)[2:4] <- c("Mean", "q25","q75")
mean.q.ext_ran.nca$ext.lower <- as.numeric(mean.q.ext_ran.nca$ext.lower)

prop_ran_nca.obs <- apply(ex_ran_nca_sp.dist, 2, function(x) (628-cumsum(x))/628) #get the cumulative sums of the proportions species remaining alive

mean.q.ext_ran.nca <- mean.q.ext_ran.nca %>% mutate(ext.higher_prop = rowMeans(prop_ran_nca.obs),
                                                    ext.lower_prop = seq(1:38)/38,
                                                    site = 'nca') %>%
  cbind(matrixStats::rowQuantiles(prop_ran_nca.obs, probs  = c(0.25,0.75))) %>% data.frame(.)

names(mean.q.ext_ran.nca)[c(2,8,9)] <- c("ext.higher","q25_prop","q75_prop")

mean.q.ext_ran.nca %>% rbind(c(0,628))

ggplot(mean.q.ext_ran.nca, aes(x=ext.lower_prop, y=ext.higher_prop))+ 
  geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=q25_prop, ymax=q75_prop), alpha=0.2)


cumsum(ex_ran_nca_sp[[2]][,3])
length(nca_ISmatrix_sp2[which(colSums(nca_ISmatrix_sp2) != 0)]) #164 sp
length(nca_ISmatrix_sp_std[which(colSums(nca_ISmatrix_sp_std) != 0)]) #164 sp, this matrix is the one with only nodes of the site and not from all regions

#Df with observed extinction 
mean.q.ext.ran_obs <- rbind(mean.q.ext_ran.haw, mean.q.ext_ran.mad, mean.q.ext_ran.mari, mean.q.ext_ran.vir,mean.q.ext_ran.nca)


#Plot
mean.q.ext.ran_obs$site <- as.factor(mean.q.ext.ran_obs$site)
ggplot(mean.q.ext.ran_obs, aes(x=ext.lower_prop, y=ext.higher_prop)) + 
  geom_ribbon(data = mean.q.ext.ran_obs, aes(ymin=q25_prop, ymax=q75_prop, fill = site), alpha=0.1) +
  geom_line(aes(color = site), size=0.7)+ 
  scale_color_manual(name = "Regions",labels = c("Hawaï","Madagascar", "Marshall Islands", "New Caledonia", "Virgin Islands"), values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  scale_fill_manual(name = "Regions", labels = c("Hawaï","Madagascar", "Marshall Islands", "New Caledonia", "Virgin Islands"), values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  labs( x= "Proportions of primary extinctions - Preys", y="Proportions of predators still alive") +
  ggtitle( "a) Random extinction") +
  theme_classic() 

