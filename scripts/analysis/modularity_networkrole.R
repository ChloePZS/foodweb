#######################################
#Modularity analysis and network role# 
#######################################

library(tidyverse)
library(bipartite)
library(picante)
library(ggpubr)
library(reshape2)

a<-read.csv('data/foodweb_final.csv',header = T, sep = ",") 
sites<-unique(a$site)
a <- a %>% select(., site, cons_class, cons_fam, cons_sp, cons_size, lev1, lev2, w) %>% unique()

#aggregate(w~site+cons_class+cons_fam+cons_sp+cons_size+lev1+lev2, a, mean)


null_mod<-function(x){  #x is the dataset for a given locality, i.e. see b above; function returns a randomized version of b, that you can then use in all the analyses
  l <- list()
  spp <- unique(x$cons_sp)
  for (sp in spp){l[[sp]]<-x[x$cons_sp==sp,]}
  null_l <- l
  for (rep in 1:1000){
    r_pair <- as.character(sample(spp,2))
    r1<-null_l[[r_pair[1]]]
    r2<-null_l[[r_pair[2]]]
    if (r1$cons_size[1]/r2$cons_size[1]>0.8 && r1$cons_size[1]/r2$cons_size[1]<1.2){ 
      r1_r2<-which(r1$lev1 %in% setdiff(r1$lev1,r2$lev1))
      r2_r1<-which(r2$lev1 %in% setdiff(r2$lev1,r1$lev1))
      if (length(r1_r2)*length(r2_r1)>0){
        if(length(r1_r2)==1)
        {n1<-r1_r2[1]} else {n1<-sample(r1_r2,1)} #thanks R for the stupid sample behavior
        if(length(r2_r1)==1)
        {n2<-r2_r1[1]} else {n2<-sample(r2_r1,1)}
        r1_<-r1[n1,5:7]
        r2_<-r2[n2,5:7]
        r1[n1,5:7]<-r2_
        r2[n2,5:7]<-r1_
        null_l[[r_pair[1]]]<-r1
        null_l[[r_pair[2]]]<-r2
      }
    }
  }
  l<-c()
  for (sp in spp){
    l<-rbind(l,null_l[[sp]]) 
  }
  return (l)
}

#For mod analysis, use lev2 prey classifications and species levels
#Carefull to set the argument "bin"
get_mat<-function(l,fun='mean',bin='T'){
  if (fun=='mean'){
    l_mat<-dcast(l,lev2~cons_sp,value.var="w",fun.aggregate = mean,fill=0,drop=T)} 
  else {
    l_mat<-dcast(l,lev2~cons_sp,value.var="w",fun.aggregate = sum, fill=0,drop=T)}
  #use fun.aggregate = sum for vir, mean for the others
  row.names(l_mat)<-l_mat[,1]
  l_mat<-l_mat[,2:dim(l_mat)[2]]
  l_mat<-t(t(l_mat)/colSums(l_mat))
  if (bin!='F'){l_mat<-1*(l_mat>0)}
  l_mat[is.nan(l_mat)] <- 0
  return (l_mat)
}  

###example, select the virgin islands data
#sites [1] vir  mad  nca  jap  haw  mari

#1. Get weighted matrices
vir_ISmatrix_sp_std <- get_mat(a[a$site=='vir',],fun='sum')
mari_ISmatrix_sp_std <- get_mat(a[a$site=='mari',])
nca_ISmatrix_sp_std <- get_mat(a[a$site=='nca',])
haw_ISmatrix_sp_std <- get_mat(a[a$site=='haw',])
mad_ISmatrix_sp_std <- get_mat(a[a$site=='mad',])
jap_ISmatrix_sp_std <- get_mat(a[a$site=='jap',])


#2. Get qualitative matrices
pa_vir_sp <- get_mat(a[a$site=='vir',],fun='sum', bin = 'T')
pa_mari_sp <- get_mat(a[a$site=='mari',], bin = 'T')
pa_nca_sp <- get_mat(a[a$site=='nca',], bin = 'T')
pa_haw_sp <- get_mat(a[a$site=='haw',], bin = 'T')
pa_mad_sp <- get_mat(a[a$site=='mad',], bin = 'T')
pa_jap_sp <- get_mat(a[a$site=='jap',], bin = 'T')


  #3. Modularity measure ####
#Use Beckett's alg LPAwb+ for binary weigthed matrices. If networks are binary then the resuts are equivalent to the LPAb+ alg for binary networks of Liu & Murata 2010
#Quantitative webs
# mod_mad_sp <- lapply(1:500, function(x) {
#   set.seed(x)
#   computeModules(mad_ISmatrix_sp_std, method = "Beckett",forceLPA = TRUE)})
# 
# mod_mari_sp <- lapply(1:500, function(x) {
#   set.seed(x)
#   computeModules(mari_ISmatrix_sp_std, method = "Beckett",forceLPA = TRUE)})
# 
# mod_nca_sp <- lapply(1:500, function(x) {
#   set.seed(x)
#   computeModules(nca_ISmatrix_sp_std, method = "Beckett",forceLPA = TRUE)})
# 
# mod_haw_sp <- lapply(1:500, function(x) {
#   set.seed(x)
#   computeModules(haw_ISmatrix_sp_std, method = "Beckett",forceLPA = TRUE)})
# 
# mod_vir_sp <- lapply(1:500, function(x) {
#   set.seed(x)
#   computeModules(vir_ISmatrix_sp_std, method = "Beckett",forceLPA = TRUE)})
# 
#mod_jap_sp <- lapply(1:500, function(x) {
#   set.seed(x)
#   computeModules(jap_ISmatrix_sp_std)})
# 
# #store Q value
# mod_obsW <- c(sapply(mod_mad_sp, function(x) x@likelihood),
#                sapply(mod_nca_sp, function(x) x@likelihood),
#                sapply(mod_mari_sp, function(x) x@likelihood),
#                sapply(mod_haw_sp, function(x) x@likelihood),
#                sapply(mod_vir_sp, function(x) x@likelihood),
#                sapply(mod_jap_sp, function(x) x@likelihood)) %>%
#   as.data.frame() %>%
#   dplyr::rename(., mod =".") %>%
#   add_column(site = rep(c("mad","nca","mari","haw","vir","jap"), each = 500)) %>%
#   add_column(obs = "obs")


#Qualitative webs with Newman's alg 
mod_mad_sp <- 
computeModules(pa_mad_sp)
# 
mod_mari_sp <- 
computeModules(pa_mari_sp)
# 
mod_nca_sp <- 
computeModules(pa_nca_sp)
# 
mod_haw_sp <- 
computeModules(pa_haw_sp)
# 
mod_vir_sp <- 
computeModules(pa_vir_sp)
# 
mod_jap_sp <- 
computeModules(pa_jap_sp)
# 
mod_obsB <- c(mod_mad_sp@likelihood,
              mod_nca_sp@likelihood,
              mod_mari_sp@likelihood,
              mod_haw_sp@likelihood,
              mod_vir_sp@likelihood,
              mod_jap_sp@likelihood) %>%
   as.data.frame() %>%
   dplyr::rename(., mod =".") %>%
   add_column(site = rep(c("mad","nca","mari","haw","vir","jap"), each = 1)) %>%
   add_column(obs = "obs")

#to have number of modules for each networks
# #nb_mod <- c(dim(mod_mad_sp@modules)[1]-1, dim(mod_nca_sp@modules)[1]-1, dim(mod_mari_sp@modules)[1]-1, dim(mod_haw_sp@modules)[1]-1, dim(mod_vir_sp@modules)[1]-1, dim(mod_jap_sp@modules)[1]-1) %>%
#   as.data.frame() %>%
#   dplyr::rename(., nb_mod =".") %>%
#   add_column(site = c("mad","nca","mari","haw","vir","jap")) %>%
#   add_column(obs = "obs")


#4. Permutations ####
  #new null model
rand_vir_sp3 <- list()
rand_mari_sp3 <- list()
rand_nca_sp3 <- list()
rand_mad_sp3 <- list()
rand_haw_sp3 <- list()
rand_jap_sp3 <- list()

nm_n<-1000 
#Careful to change the argument bin to 'T' or 'F' 
for (i in 1:nm_n){
  b<-a[a$site=='vir',]
  b<-null_mod(b)
  rand_vir_sp3[[i]]<-get_mat(b,fun='sum',bin='T')
  
  b<-a[a$site=='mari',]
  b<-null_mod(b)
  rand_mari_sp3[[i]]<-get_mat(b,bin='T')
  
  b<-a[a$site=='nca',]
  b<-null_mod(b)
  rand_nca_sp3[[i]]<-get_mat(b,bin='T')
  
  b<-a[a$site=='mad',]
  b<-null_mod(b)
  rand_mad_sp3[[i]]<-get_mat(b,bin='T')
  
  b<-a[a$site=='haw',]
  b<-null_mod(b)
  rand_haw_sp3[[i]]<-get_mat(b,bin='T')
  
  b<-a[a$site=='jap',]
  b<-null_mod(b)
  rand_jap_sp3[[i]]<-get_mat(b,bin='T')
  
  print (i) }

  #Null modularity with Newman's
mod_mari_sp_null3 <- unlist(sapply(rand_mari_sp3, computeModules))
mod_mad_sp_null3 <- unlist(sapply(rand_mad_sp3, computeModules))
mod_haw_sp_null3 <- unlist(sapply(rand_haw_sp3, computeModules))
mod_nca_sp_null3 <- unlist(sapply(rand_nca_sp3, computeModules))
mod_vir_sp_null3 <- unlist(sapply(rand_vir_sp3, computeModules))
mod_jap_sp_null3 <- unlist(sapply(rand_jap_sp3, computeModules))


  #To store modularity value of random weighted matrices
# mod_null3W <- c(sapply(mod_mad_sp_null3, function(x) x@likelihood),
#                 sapply(mod_nca_sp_null3, function(x) x@likelihood),
#                 sapply(mod_mari_sp_null3, function(x) x@likelihood),
#                 sapply(mod_haw_sp_null3, function(x) x@likelihood),
#                 sapply(mod_vir_sp_null3, function(x) x@likelihood),
#                 sapply(mod_jap_sp_null3, function(x) x@likelihood)) %>%
#   as.data.frame() %>%
#   dplyr::rename(., mod = 1) %>%
#   add_column(site = rep(c("mad","nca","mari","haw","vir","jap"), each=nm_n)) %>%
#   add_column(null = "giovanni") %>%
#   add_column(obs = "lev2")

#To store modularity value of random binary matrices
mod_null3B <- c(sapply(mod_mad_sp_null3, function(x) x@likelihood),
                sapply(mod_nca_sp_null3, function(x) x@likelihood),
                sapply(mod_mari_sp_null3, function(x) x@likelihood),
                sapply(mod_haw_sp_null3, function(x) x@likelihood),
                sapply(mod_vir_sp_null3, function(x) x@likelihood),
                sapply(mod_jap_sp_null3, function(x) x@likelihood)) %>%
   as.data.frame() %>%
   dplyr::rename(., mod = 1) %>%
   add_column(site = rep(c("mad","nca","mari","haw","vir","jap"), each=nm_n)) %>%
   add_column(null = "giovanni") %>%
   add_column(obs = "lev2")

  #Calculate relative modularity as M* = (M-Mr)/Mr
mod_null3B %>% group_by(site) %>% summarize(mean = mean(mod),
                                           sd = sd(mod)) %>%
  left_join(., obs_mod[,-3], by = "site") %>%
  mutate(mod_r = (mod - mean)/mean,
         zscore = (mod-mean)/sd,
         pval = 1-pnorm(zscore))

  #5. Plot
quantiles_95 <- function(x) {
  r <- c(mean(x), quantile(x, probs=c(0.05)),quantile(x, probs=c(0.95)))
  names(r) <- c("y","ymin","ymax")
  r
}

#Plot for modularity - Change data mod_null3W and mod_obsW vs mod_null3B and mod_obsB 
mod_plot2 <- ggplot(data=mod_null3B, aes(x=factor(site, levels = c("haw","mad","mari","nca","jap","vir")), y=mod, fill=site)) +
  stat_summary(fun.data = quantiles_95, geom ="errorbar", width=0.08) +
  stat_summary(fun = mean, geom="point",size=3.3,aes(shape = "Null distributions", color = "Null distributions" )) + 
  #stat_summary(data = mod_obsB, aes(y = mod, x=factor(site, levels = c("haw","mad","mari","nca","jap","vir"))), fun.data = quantiles_95, geom ="errorbar", width=0.08) +
  geom_point(data = mod_obsB, aes(y = mod, x=factor(site, levels = c("haw","mad","mari","nca","jap","vir")), shape = "Observed values", color = "Observed values" ),size=3.3) + 
  scale_shape_manual(name = NULL, values = c(19,19))+
  scale_color_manual(name = NULL, values = c("grey60","black"))+
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

#6. Network role - CZ values (by default weighted = FALSE) ####
#Now mod_..._sp has 500 networks. So use the metaComputemodules to generate the algo 500 times and returns the most modular.
#Quantitative
# set.seed(13)
# metamod_vir_sp <- metaComputeModules(vir_ISmatrix_sp_std, method = "Beckett", forceLPA = TRUE)
# 
# set.seed(14)
# metamod_mari_sp <- metaComputeModules(mari_ISmatrix_sp_std, method = "Beckett", forceLPA = TRUE)
# 
# set.seed(15)
# metamod_mad_sp <- metaComputeModules(mad_ISmatrix_sp_std, method = "Beckett", forceLPA = TRUE)
# 
# set.seed(15)
# metamod_nca_sp <- metaComputeModules(nca_ISmatrix_sp_std, method = "Beckett", forceLPA = TRUE)
# 
# set.seed(16)
# metamod_haw_sp <- metaComputeModules(haw_ISmatrix_sp_std, method = "Beckett", forceLPA = TRUE)
# 
# set.seed(17)
# metamod_jap_sp <- metaComputeModules(jap_ISmatrix_sp_std, method = "Beckett", forceLPA = TRUE)
# 
# #Qualitative
# set.seed(18)
#  metamod_vir_sp <- metaComputeModules(pa_vir_sp, method = "Beckett", forceLPA = TRUE)
# # 
# set.seed(19)
# metamod_mari_sp <- metaComputeModules(pa_mari_sp, method = "Beckett", forceLPA = TRUE)
# # 
# set.seed(20)
# metamod_mad_sp <- metaComputeModules(pa_mad_sp, method = "Beckett", forceLPA = TRUE)
# # 
# set.seed(21)
# metamod_nca_sp <- metaComputeModules(pa_nca_sp, method = "Beckett", forceLPA = TRUE)
# # 
# set.seed(22)
# metamod_haw_sp <- metaComputeModules(pa_haw_sp, method = "Beckett", forceLPA = TRUE)
# # 
# set.seed(23)
# metamod_jap_sp <- metaComputeModules(pa_jap_sp, method = "Beckett", forceLPA = TRUE)


#With Newman's alg
set.seed(18)
metamod_vir_sp <- metaComputeModules(pa_vir_sp, N = 500)
# 
set.seed(19)
metamod_mari_sp <- metaComputeModules(pa_mari_sp, N = 500)
# 
set.seed(20)
metamod_mad_sp <- metaComputeModules(pa_mad_sp, N = 500)
# 
set.seed(21)
metamod_nca_sp <- metaComputeModules(pa_nca_sp, N = 500)
# 
set.seed(22)
metamod_haw_sp <- metaComputeModules(pa_haw_sp, N = 500)
# 
set.seed(23)
metamod_jap_sp <- metaComputeModules(pa_jap_sp, N = 500)

#Getting c and z values - Carefull which network is loaded ! weighted = TRUE or FALSE
cz_vir.high <- czvalues(metamod_vir_sp, weighted = FALSE,  level = "higher") %>%
  data.frame(.) %>%
  rownames_to_column(., var = "species") %>%
  mutate(site ="vir")
cz_mari.high <- czvalues(metamod_mari_sp,weighted = FALSE, level = "higher") %>%
  data.frame(.) %>%
  rownames_to_column(., var = "species") %>%
  mutate(site ="mari")
cz_mad.high <- czvalues(metamod_mad_sp,weighted = FALSE, level = "higher") %>%
  data.frame(.) %>%
  rownames_to_column(., var = "species") %>%
  mutate(site ="mad")
cz_nca.high <- czvalues(metamod_nca_sp,weighted = FALSE,level = "higher") %>%
  data.frame(.) %>%
  rownames_to_column(., var = "species") %>%
  mutate(site ="nca")
cz_haw.high <- czvalues(metmod_haw_sp,weighted = FALSE,level = "higher") %>%
  data.frame(.) %>%
  rownames_to_column(., var = "species") %>%
  mutate(site ="haw")
cz_jap.high <- czvalues(metamod_jap_sp,weighted = FALSE,level = "higher") %>%
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

#To get percentage of species per role
cz_full %>% group_by(role) %>% dplyr::summarize (n = n()) %>%
  mutate(per = n/sum(n)*100) 


#Plot
site.lab <- c("Hawaii","Madagascar","Marshall Islands","New Caledonia","West Indies","Okinawa")
names(site.lab) <-c("haw","mad","mari","nca","vir","jap")

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
  labs(x = "\nAmong-module connectivity (c)", y = "Within-module degree (z)\n") +
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
netplot_facet <- ggplot(cz_full, aes(x = c, y = z)) +
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
                     na.translate = F)+
  facet_wrap(site ~ ., labeller = labeller(site=site.lab)) +
  labs(x = "\nAmong-module connectivity (c)", y = "Within-module degree (z)\n") +
  xlim(0,1) +
  ylim(-3, 8) +
  theme_classic() +
  theme(strip.background = element_rect(color="black", fill="lightgrey", size=0.5, linetype="solid"),
        strip.text = element_text(size = 11),
        panel.background = element_rect(colour = "black", size=0.5),
        axis.title = element_text(size=13.5),
        axis.text = element_text(size=12, colour = "black"),
        legend.text = element_text(color = "black", size = 12))

netplot_facet

ggsave(ggsave("output/figures/network role plot_facetB_NewmanQ.png", plot = netplot_facet, units="in", width=11, height=7, dpi=300)
)

  #7. Combine plots and save
mod.plot.cb <- ggarrange(mod_plot2, netrole_plot, 
          labels = c("A","B"),
          font.label = list(size = 12, face = "bold"),
          ncol = 1, nrow = 2, align = "v",
          heights = c(1,2))
mod.plot.cb 
ggsave("output/figures/modularity & network role_binarynet_Newman.png", plot = mod.plot.cb, units="in", width=9, height=11, dpi=300)


