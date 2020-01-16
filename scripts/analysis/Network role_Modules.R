#######################################
#Modularity analysis with network role# FROM BIPARTITE PACKAGE
#######################################

library(tidyverse)
library(bipartite)
library(picante)
#library(Cairo)
library(ggpubr)


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
jap_ISmatrix_sp_std <- read.csv("data/jap_ISmatrix_sp_std_grp6.csv", sep=",", row.names = 1) %>%
  as.matrix(.)

#2. Transform into qualitative matrices
pa_vir_sp <- decostand(vir_ISmatrix_sp_std, method="pa")
pa_mari_sp <- decostand(mari_ISmatrix_sp_std, method="pa")
pa_nca_sp <- decostand(nca_ISmatrix_sp_std, method="pa")
pa_haw_sp <- decostand(haw_ISmatrix_sp_std, method="pa")
pa_mad_sp <- decostand(mad_ISmatrix_sp_std, method="pa")
pa_jap_sp <- decostand(jap_ISmatrix_sp_std, method="pa")

#With matrice using item_volper (132 sp instead of 117)
pa_jap_sp_vol <- decostand(jap_ISmatrix_sp_vol, method="pa")
mod_jap_sp_vol <- computeModules(pa_jap_sp_vol)


#3. Modularity measure ####
#Qualitative webs
#Single algorithme run

set.seed(1)
mod_mad_sp <- computeModules(pa_mad_sp)
set.seed(2)
mod_nca_sp <- computeModules(pa_nca_sp)
set.seed(3)
mod_mari_sp <- computeModules(pa_mari_sp)
set.seed(4)
mod_haw_sp <- computeModules(pa_haw_sp)
set.seed(5)
mod_vir_sp <- computeModules(pa_vir_sp) 
set.seed(6)
mod_jap_sp <- computeModules(pa_jap_sp)

mod_jap_sp_vol <- computeModules(pa_jap_sp_vol)


obs_mod <- c(mod_mad_sp@likelihood, mod_nca_sp@likelihood, mod_mari_sp@likelihood, mod_haw_sp@likelihood, mod_vir_sp@likelihood,mod_jap_sp@likelihood) %>%
  as.data.frame() %>%
  dplyr::rename(., mod =".") %>%
  add_column(site = c("mad","nca","mari","haw","vir","jap")) %>%
  add_column(obs = "obs")



#metamod_vir_sp <- metaComputeModules(pa_vir_sp, N = 10) #re-runs algo and return most modular


#repetition to have a mean and SD for modularity
#Virgin islands
mod_vir_sp_rep <- parallel::mclapply(1:100, function(x) computeModules(pa_vir_sp), mc.cores = 40)
mod_vir_rep <- sapply(mod_vir_sp_rep, function(x) x@likelihood)
mean(mod_vir_rep); sd(mod_vir_rep)

nb_mod_vir <- sapply(mod_vir_sp_rep, function(x) dim(x@modules)[1]-1) 


plotModuleWeb(mod_vir_sp)

#Hawai
mod_haw_sp_rep <- parallel::mclapply(1:100, function(x) computeModules(pa_haw_sp),mc.cores = 40)
mod_haw_rep <- sapply(mod_haw_sp_rep, function(x) x@likelihood)
mean(mod_haw_rep); sd(mod_haw_rep)

nb_mod_haw <- sapply(mod_haw_sp_rep, function(x) dim(x@modules)[1]-1) 
mean(nb_mod_haw); sd(nb_mod_haw)

#Marshall Islands
mod_mari_sp_rep <- parallel::mclapply(1:100, function(x) computeModules(pa_mari_sp), mc.cores = 40)
mod_mari_rep <- sapply(mod_mari_sp_rep, function(x) x@likelihood)
mean(mod_mari_rep); sd(mod_mari_rep)

nb_mod_mari <- sapply(mod_mari_sp_rep, function(x) dim(x@modules)[1]-1) 
mean(nb_mod_mari); sd(nb_mod_mari)

#New caledonia
mod_nca_sp_rep <- parallel::mclapply(1:100, function(x) computeModules(pa_nca_sp), mc.cores = 40)
mod_nca_rep <- sapply(mod_nca_sp_rep, function(x) x@likelihood)
mean(mod_nca_rep); sd(mod_nca_rep)

nb_mod_nca <- sapply(mod_nca_sp_rep, function(x) dim(x@modules)[1]-1) 
mean(nb_mod_nca); sd(nb_mod_nca)

#Madagascar
mod_mad_sp_rep <- parallel::mclapply(1:100, function(x) computeModules(pa_mad_sp), mc.cores = 40)
mod_mad_rep <- sapply(mod_mad_sp_rep, function(x) x@likelihood)
mean(mod_mad_rep); sd(mod_mad_rep)

nb_mod_mad <- sapply(mod_mad_sp_rep, function(x) dim(x@modules)[1]-1) 
mean(nb_mod_mad); sd(nb_mod_mad)

#Japan
mod_jap_sp_rep <- parallel::mclapply(1:100, function(x) computeModules(pa_jap_sp), mc.cores = 40)
mod_jap_rep <- sapply(mod_jap_sp_rep, function(x) x@likelihood)
mean(mod_jap_rep); sd(mod_jap_rep)

nb_mod_jap <- sapply(mod_jap_sp_rep, function(x) dim(x@modules)[1]-1) 
mean(nb_mod_jap); sd(nb_mod_jap)


obs_mod_grp6 <- c(mod_mad_rep, mod_nca_rep, mod_mari_rep, mod_haw_rep, mod_vir_rep, mod_jap_rep) %>%
  as.data.frame() %>%
  rename(., mod =".") %>%
  add_column(site = rep(c("mad","nca","mari","haw","vir","jap"), each = 100)) %>%
  add_column(obs = "grp6")

write.csv(obs_mod_grp6, "output/obs_mod_grp6.csv")
  
#Quantitative webs
#mod_vir_sp <- computeModules(vir_ISmatrix_sp_std, method="Beckett", forceLPA = FALSE) #DIRT algorithm from Becket 2016
#mod_mari_sp <- computeModules(mari_ISmatrix_sp_std, method="Beckett", forceLPA = FALSE)
#mod_mad_sp <- computeModules(mad_ISmatrix_sp_std, method="Beckett", forceLPA = FALSE)
#mod_nca_sp <- computeModules(nca_ISmatrix_sp_std, method="Beckett", forceLPA = FALSE)
#mod_haw_sp <- computeModules(haw_ISmatrix_sp_std, method="Beckett", forceLPA = FALSE)


#4. Null models from the less to the most conservative ####

#4.1 bipartite::shuffle.web --> keep connectance
rand_vir_sp1 <- shuffle.web(pa_vir_sp, N=100)
rand_mari_sp1 <- shuffle.web(pa_mari_sp, N=100)
rand_nca_sp1 <- shuffle.web(pa_nca_sp, N=100)
rand_mad_sp1 <- shuffle.web(pa_mad_sp, N=100)
rand_haw_sp1 <- shuffle.web(pa_haw_sp, N=100)

#Null modularity
mod_mari_sp_null1 <- unlist(sapply(rand_mari_sp1, computeModules))
mod_mad_sp_null1 <- unlist(sapply(rand_mad_sp1, computeModules))
mod_haw_sp_null1 <- unlist(sapply(rand_haw_sp1, computeModules))
mod_nca_sp_null1 <- unlist(sapply(rand_nca_sp1, computeModules))
mod_vir_sp_null1 <- unlist(sapply(rand_vir_sp1, computeModules))

mod_null1 <- c(sapply(mod_mad_sp_null1, function(x) x@likelihood),
                             sapply(mod_nca_sp_null1, function(x) x@likelihood),
                             sapply(mod_mari_sp_null1, function(x) x@likelihood),
                             sapply(mod_haw_sp_null1, function(x) x@likelihood),
                             sapply(mod_vir_sp_null1, function(x) x@likelihood)) %>%
  as.data.frame() %>%
  rename(., mod =".") %>%
  add_column(site = rep(c("mad","nca","mari","haw","vir"), each=100)) %>%
  add_column(null = "shuffle")
  

#4.2 picante::randomizeMatrix "frequency" --> keep colSums and connectance 
p <- 100  #for 100 random matrices

rand_vir_sp2 <- lapply(1:p, function(x) randomizeMatrix(pa_vir_sp, null.model = "frequency"))
rand_mari_sp2 <- lapply(1:p, function(x) randomizeMatrix(pa_mari_sp, null.model = "frequency"))
rand_nca_sp2 <- lapply(1:p, function(x) randomizeMatrix(pa_nca_sp, null.model = "frequency"))
rand_mad_sp2 <- lapply(1:p, function(x) randomizeMatrix(pa_mad_sp, null.model = "frequency"))
rand_haw_sp2 <- lapply(1:p, function(x) randomizeMatrix(pa_haw_sp, null.model = "frequency"))


#Null modularity
mod_mari_sp_null2 <- unlist(sapply(rand_mari_sp2, computeModules))
mod_mad_sp_null2 <- unlist(sapply(rand_mad_sp2, computeModules))
mod_haw_sp_null2 <- unlist(sapply(rand_haw_sp2, computeModules))
mod_nca_sp_null2 <- unlist(sapply(rand_nca_sp2, computeModules))
mod_vir_sp_null2 <- unlist(sapply(rand_vir_sp2, computeModules))

mod_null2 <- c(sapply(mod_mad_sp_null2, function(x) x@likelihood),
               sapply(mod_nca_sp_null2, function(x) x@likelihood),
               sapply(mod_mari_sp_null2, function(x) x@likelihood),
               sapply(mod_haw_sp_null2, function(x) x@likelihood),
               sapply(mod_vir_sp_null2, function(x) x@likelihood)) %>%
  as.data.frame() %>%
  rename(., mod =".") %>%
  add_column(site = rep(c("mad","nca","mari","haw","vir"), each=100)) %>%
  add_column(null = "picante")


#4.3 Curveball algorithme of Giovanni
#Function
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

rand_vir_sp3 <- lapply(1:p, function(x) curve_ball(pa_vir_sp))
rand_mari_sp3 <- lapply(1:p, function(x) curve_ball(pa_mari_sp))
rand_nca_sp3 <- lapply(1:p, function(x) curve_ball(pa_nca_sp))
rand_mad_sp3 <- lapply(1:p, function(x) curve_ball(pa_mad_sp))
rand_haw_sp3 <- lapply(1:p, function(x) curve_ball(pa_haw_sp))
rand_jap_sp3 <- lapply(1:p, function(x) curve_ball(pa_jap_sp))
rand_jap_sp3_vol <- lapply(1:p, function(x) curve_ball(pa_jap_sp_vol))

#Null modularity
mod_mari_sp_null3 <- unlist(sapply(rand_mari_sp3, computeModules))
mod_mad_sp_null3 <- unlist(sapply(rand_mad_sp3, computeModules))
mod_haw_sp_null3 <- unlist(sapply(rand_haw_sp3, computeModules))
mod_nca_sp_null3 <- unlist(sapply(rand_nca_sp3, computeModules))
mod_vir_sp_null3 <- unlist(sapply(rand_vir_sp3, computeModules))
mod_jap_sp_null3 <- unlist(sapply(rand_jap_sp3, computeModules))
mod_jap_sp_null3_vol <- unlist(sapply(rand_jap_sp3_vol, computeModules))

#!! add either of the two japan food web modularity estimates (item_freq VS item_volper)
mod_null3 <- c(sapply(mod_mad_sp_null3, function(x) x@likelihood),
               sapply(mod_nca_sp_null3, function(x) x@likelihood),
               sapply(mod_mari_sp_null3, function(x) x@likelihood),
               sapply(mod_haw_sp_null3, function(x) x@likelihood),
               sapply(mod_vir_sp_null3, function(x) x@likelihood),
               sapply(mod_jap_sp_null3, function(x) x@likelihood)) %>%
  as.data.frame() %>%
  dplyr::rename(., mod = 1) %>%
  add_column(site = rep(c("mad","nca","mari","haw","vir","jap"), each=100)) %>%
  add_column(null = "curveball") %>%
  add_column(obs = "grp6")

#df with all null model results
mod_null <- rbind(mod_null1, mod_null2, mod_null3)


#plot with all null models
ggplot(data=mod_null, aes(x=site, y=mod, fill=site)) +
  geom_point(data=obs_mod, size=2, shape=8)+
  stat_summary(fun.data = mean_cl_boot, size=0.7, linetype="solid", geom="errorbar", color="black")  +
  geom_boxplot(data=mod_null, outlier.shape = NA) +
  scale_fill_manual(values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  facet_grid(null ~., scales="free", switch ="y")+
  scale_x_discrete(breaks=c("haw","mad","mari","nca","vir"),
                   labels=c("Hawaii", "Madagascar", "Marshall Islands","New Caledonia","West Indies"))+
  labs( y = "Modularity\n", x="") + 
  ggtitle("Newman's modularity - Bipartite")+
  theme_test() +
  theme(strip.text.y = element_text(size=11),
      axis.line.x = element_line(colour = "black",size=0.5, lineend = "butt"),
       axis.line.y = element_line(colour = "black", size=0.5),
       axis.title = element_text(size=13.5),
       axis.text = element_text(size=12, colour = "black")) +
  guides(color=FALSE) +
  theme(legend.position = "none") 

#plot with curveball null model only
mod_plot <- ggplot(data=mod_null3, aes(x=site, y=mod, fill=site)) +
  stat_summary(fun.data = mean_cl_boot, size=0.5, linetype="solid", geom="errorbar", color="black")  +
  geom_boxplot(data=mod_null3, outlier.shape = NA,  fill = "grey50", color = "grey50") +
  stat_summary(geom = "crossbar", width=0.7, fatten=1, color="white", fun.data = function(x){ return(c(y=median(x), ymin=median(x), ymax=median(x))) })  +
  geom_point(data=obs_mod, size=3, shape=8)+
  #scale_fill_manual(values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  scale_x_discrete(breaks=c("haw","mad","mari","nca","vir","jap"),
                   labels=c("Hawaii", "Madagascar", "Marshall Islands","New Caledonia","West Indies","Okinawa Island"))+
  labs(y = "Modularity\n", x="") + 
  theme_test() +
  theme(strip.text.y = element_text(size=11),
        axis.line.x = element_line(colour = "black",size=0.5, lineend = "butt"),
        axis.line.y = element_line(colour = "black", size=0.5),
        axis.title = element_text(size=13.5),
        axis.text = element_text(size=12, colour = "black")) +
  guides(color=FALSE) +
  theme(legend.position = "none") 

mod_plot


#Other plot
quantiles_95 <- function(x) {
  r <- c(mean(x), quantile(x, probs=c(0.05)),quantile(x, probs=c(0.95)))
names(r) <- c("y","ymin","ymax")
r
}

mod_plot2 <- ggplot(data=mod_null3, aes(x=factor(site, levels = c("haw","mad","mari","nca","jap","vir")), y=mod, fill=site)) +
  geom_point(data=obs_mod, size=3.5, aes(shape="Observed values", color ="Observed values"))+
  stat_summary(fun.data = quantiles_95, geom ="errorbar", size=0.3, width=0.08) +
  stat_summary(fun.y = mean, geom="point",size=3.5,aes(shape = "Null distributions", color = "Null distributions" )) + 
  #scale_fill_manual(values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  scale_shape_manual(name = NULL, values = c(19,18))+
  scale_color_manual(name = NULL, values = c("grey50","black"))+
  scale_x_discrete(breaks=c("haw","mad","mari","nca","vir","jap"),
                   labels=c("Hawaii", "Madagascar", "Marshall Islands","New Caledonia","West Indies","Okinawa"))+
  labs(y = "Modularity\n", x="") + 
  theme_test() +
  theme(strip.text.y = element_text(size=11),
        legend.text = element_text(colour="black", size=10),
        axis.line.x = element_line(colour = "black",size=0.5, lineend = "butt"),
        axis.line.y = element_line(colour = "black", size=0.5),
        axis.title = element_text(size=13.5),
        axis.text = element_text(size=12, colour = "black")) +
  guides(fill=FALSE) +
  theme(legend.position = c(0.1,0.9),
        legend.box = NULL)

mod_plot2




#5. Network role - CZ values (by default weighted = FALSE) ####
cz_vir.high <- czvalues(mod_vir_sp, weighted = FALSE,  level = "higher") %>%
  data.frame(.) %>%
  rownames_to_column(., var = "species") %>%
  mutate(site ="vir")
cz_mari.high <- czvalues(mod_mari_sp,weighted = FALSE, level = "higher") %>%
  data.frame(.) %>%
  rownames_to_column(., var = "species") %>%
  mutate(site ="mari")
cz_mad.high <- czvalues(mod_mad_sp,weighted = FALSE, level = "higher") %>%
  data.frame(.) %>%
  rownames_to_column(., var = "species") %>%
  mutate(site ="mad")
cz_nca.high <- czvalues(mod_nca_sp,weighted = FALSE,level = "higher") %>%
  data.frame(.) %>%
  rownames_to_column(., var = "species") %>%
  mutate(site ="nca")
cz_haw.high <- czvalues(mod_haw_sp,weighted = FALSE,level = "higher") %>%
  data.frame(.) %>%
  rownames_to_column(., var = "species") %>%
  mutate(site ="haw")
cz_jap.high <- czvalues(mod_jap_sp,weighted = FALSE,level = "higher") %>%
  data.frame(.) %>%
  rownames_to_column(., var = "species") %>%
  mutate(site ="jap")

#create a df
cz_full <- rbind(cz_haw.high, cz_mad.high, cz_mari.high, cz_nca.high, cz_vir.high, cz_jap.high) %>%
  mutate(role = case_when(z <= 2.5 & c <= 0.62 ~ "periph",
                          z <= 2.5 & c > 0.62 ~"connect",
                          z > 2.5 & c > 0.62 ~ "nethub",
                          z > 2.5 & c <= 0.62 ~ "modhub")) %>%
  mutate(species = sub("\\.", " ",species)) #to replace the dots by a space

site.lab <- c("Hawaii","Madagascar","Marshall Islands","New Caledonia","West Indies","Okinawa")
names(site.lab) <-c("haw","mad","mari","nca","vir","jap")

#Check which species are network and module hubs
cz_full %>% filter(role == "nethub" | role == "modhub")
# Balistapus undulatus nethub mari
# Heteropriacanthus cruentatus  modhub mari
# Pempheris oualensis modhub mari
# Lethrinus nebulosus nethub nca


#get the number of prey categories
data_ISfull_grp_final2 %>% filter(fish_sp %in% c("Balistapus undulatus","Heteropriacanthus cruentatus","Pempheris oualensis","Lethrinus nebulosus") & site_code %in% c("mari","nca")) %>%
  group_by(fish_sp, site_code) %>%
  summarize(n = n_distinct(grp6))

a <- data_ISfull_grp_final2 %>% group_by(fish_sp, site_code) %>%
  summarize(n = n_distinct(grp6))

cz_full %>% filter(species %in% c("Balistapus undulatus","Heteropriacanthus cruentatus","Pempheris oualensis","Lethrinus nebulosus"))

#Plot
netrole_plot <- ggplot(cz_full, aes(x = c, y = z)) +
  geom_point(aes(fill = as.factor(role), color = as.factor(role)),
             size = 3, shape = 21, alpha = 0.8) +
  geom_hline(yintercept = 2.5)+
  geom_vline(xintercept = 0.62) +
  scale_fill_manual(values = c("chartreuse3","turquoise3","coral1","darkorchid4"),
                     name = "",
                     labels = c("Connectors","Module hubs", "Network hubs","Peripherals"),
                     na.translate = F) +
  scale_color_manual(values = c("chartreuse3","turquoise3","coral1","darkorchid4"),
                    name = "",
                    labels = c("Connectors","Module hubs", "Network hubs","Peripherals"),
                    na.translate = F) +
  #facet_wrap(site ~ ., labeller = labeller(site=site.lab)) +
  labs(x = "\nAmong-module connectivity (c)", y = "Within-module degree (z)\n") +
  #ggtitle ("Fish species distribution according to their network role") +
  xlim(0,1) +
  ylim(-3, 8) +
  theme_classic2() +
  theme(legend.position = c(0.85,0.87), 
    panel.background = element_rect(colour = "black", size=0.5),
        axis.title = element_text(size=13.5),
        axis.text = element_text(size=11, colour = "black"),
        legend.text = element_text(color = "black", size = 12))

netrole_plot


#facet plot
#Facet plot network role
png("output/figures/network role_facet.png", units="in", width=11, height=7, res=300)

cz_full$site <- factor(cz_full$site,levels = c("haw","mad","mari","nca","jap","vir"))

ggplot(cz_full, aes(x = c, y = z)) +
  geom_point(aes(fill = as.factor(role), color = as.factor(role)),
             size = 3, shape = 21, alpha = 0.8) +
  geom_hline(yintercept = 2.5)+
  geom_vline(xintercept = 0.62) +
  scale_fill_manual(values = c("chartreuse3","turquoise3","coral1","darkorchid4"),
                    name = "",
                    labels = c("Connectors","Module hubs", "Network hubs","Peripherals"),
                    na.translate = F) +
  scale_color_manual(values = c("chartreuse3","turquoise3","coral1","darkorchid4"),
                     name = "",
                     labels = c("Connectors","Module hubs", "Network hubs","Peripherals"),
                     na.translate = F) +
  facet_wrap(site ~ ., labeller = labeller(site=site.lab)) +
  labs(x = "\nAmong-module connectivity (c)", y = "Within-module degree (z)\n") +
  #ggtitle ("Fish species distribution according to their network role") +
  xlim(0,1) +
  ylim(-3, 8) +
  theme_classic() +
  theme(strip.background = element_rect(color="black", fill="lightgrey", size=0.5, linetype="solid"),
        strip.text = element_text(size = 11),
        panel.background = element_rect(colour = "black", size=0.5),
        axis.title = element_text(size=13.5),
        axis.text = element_text(size=12, colour = "black"),
        legend.text = element_text(color = "black", size = 12))

dev.off()



#Combine plot
png("output/figures/network role2.png", units="in", width=9, height=11, res=300)

ggarrange(mod_plot2, netrole_plot, 
          labels = c("A","B"),
          font.label = list(size = 12, face = "bold"),
          ncol = 1, nrow = 2, align = "v",
          heights = c(1,2))

#save plot
dev.off()




#To get percentage of species per role
cz_full %>% group_by(role) %>% dplyr::summarize (n = n()) %>%
  mutate(per = n/sum(n)*100) 
        
cz_full_spread <- spread(cz_full, key ="site",value="role") %>%
  dplyr::rename(fish_sp = species) %>%
  dplyr::left_join(.,data_ISfull_grp_final[,c("fish_sp","family_cor")], by="fish_sp") %>%
  unique(.) 

c0 <- cz_full %>% 
  dplyr::rename(fish_sp = species) %>%
  dplyr::left_join(.,data_ISfull[,c("fish_sp","family_cor")], by="fish_sp") %>%
  unique(.)%>%
  filter(c == 0) %>% group_by(family_cor,role) %>% dplyr::summarize (n=n())


cz_fam <- cz_full %>% dplyr::rename(fish_sp = species) %>%
  dplyr::left_join(.,data_ISfull_grp_final2[,c("fish_sp","family_cor")], by="fish_sp") %>%
  unique(.) %>%
  group_by(family_cor,role) %>% dplyr::summarize (n=n_distinct(fish_sp), mean_c = mean(c),mean_z = mean(z)) %>%
  spread(., key ="role",value="n")


a <- cz_full %>% dplyr::rename(fish_sp = species) %>%
  dplyr::left_join(.,data_ISfull_grp_final2[,c("fish_sp","family_cor")], by="fish_sp") %>%
  unique(.) %>%
  mutate(family_cor=replace(family_cor, fish_sp== "Tylosurus acus.acus", "Belonidae")) %>%
  group_by(family_cor) %>% dplyr::summarize (mean_c = mean(c), sd_c = sd(c),mean_z = mean(z), sd_z = sd(z))%>%
  mutate(role = case_when(mean_z <= 2.5 & mean_c <= 0.62 ~ "periph",
                          mean_z <= 2.5 & mean_c > 0.62 ~"connect",
                          mean_z > 2.5 & mean_c > 0.62 ~ "nethub",
                          mean_z > 2.5 & mean_c <= 0.62 ~ "modhub"))



#More lines when merging the family names !!!!
cz_full_spread %>% group_by(fish_sp) %>% summarize(n = n_distinct(family_cor)) %>% filter(n > 1)
#Yep 4 species with assigned to two families  FUUUUUUUUCKK!! --> Corrected


data_ISfull_grp_final2 %>% filter(fish_sp == "Tylosurus acus acus")

  