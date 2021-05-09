#######################################
#Modularity analysis and network role# 
#######################################

#packages
library(tidyverse); library(bipartite); library(ggpubr); library(reshape2)

    #1. Load data
a<-read.csv('data/foodweb_final.csv',header = T, sep = ",") 
sites<-unique(a$site)
a <- a %>% select(., site, cons_class, cons_fam, cons_sp, cons_size, lev1, lev2, w) %>% unique()

    #2. Generate matrices ####
#use lev2 prey classification and species level
get_mat<-function(l,fun='mean',bin='F'){
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

#binary matrices
pa_vir_sp <- get_mat(a[a$site=='vir',],fun='sum', bin = 'T')
pa_mari_sp <- get_mat(a[a$site=='mari',], bin = 'T')
pa_nca_sp <- get_mat(a[a$site=='nca',], bin = 'T')
pa_haw_sp <- get_mat(a[a$site=='haw',], bin = 'T')
pa_mad_sp <- get_mat(a[a$site=='mad',], bin = 'T')
pa_jap_sp <- get_mat(a[a$site=='jap',], bin = 'T')

    #3. Null model and random matrices ####
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

#generate random matrices
rand_vir_sp <- list()
rand_mari_sp <- list()
rand_nca_sp <- list()
rand_mad_sp <- list()
rand_haw_sp <- list()
rand_jap_sp <- list()

for (i in 1:1000){
  b<-a[a$site=='vir',]
  b<-null_mod(b)
  rand_vir_sp[[i]]<-get_mat(b,fun='sum',bin='T')
  
  b<-a[a$site=='mari',]
  b<-null_mod(b)
  rand_mari_sp[[i]]<-get_mat(b,bin='T')
  
  b<-a[a$site=='nca',]
  b<-null_mod(b)
  rand_nca_sp[[i]]<-get_mat(b,bin='T')
  
  b<-a[a$site=='mad',]
  b<-null_mod(b)
  rand_mad_sp[[i]]<-get_mat(b,bin='T')
  
  b<-a[a$site=='haw',]
  b<-null_mod(b)
  rand_haw_sp[[i]]<-get_mat(b,bin='T')
  
  b<-a[a$site=='jap',]
  b<-null_mod(b)
  rand_jap_sp[[i]]<-get_mat(b,bin='T')
  
  print (i) }


    #4. Modularity####
#using Newman's algorithm 

  #4.1 Observed modularity
mat_sp <- list(pa_vir_sp, pa_mad_sp, pa_mari_sp, pa_nca_sp, pa_haw_sp, pa_jap_sp)
list_mod <- list()
set.seed(1)
  for(m in 1:6){
    mat <- mat_sp[[m]]
    mod_mat <- computeModules(mat)
    list_mod[[m]] <- mod_mat
  }

mod_obs <- data.frame(mod = sapply(list_mod, function(x) x@likelihood),
                      site = c("vir","mad","mari","nca","haw","jap"),
                      obs = "obs") #store in a df the observed modularity

  #4.2 Modularity of random matrices
#modularity 
rand_mat_sp <- list(rand_vir_sp, rand_mad_sp, rand_mari_sp, rand_nca_sp, rand_haw_sp, rand_jap_sp)
nm_n <- length(rand_mat_sp[[1]])

rand_mod <- unlist(lapply(rand_mat_sp, lapply, computeModules)) #apply computeModules on each matrix in every list (i.e. site) of the larger list

mod_null <- data.frame(mod = sapply(rand_mod, function(x) x@likelihood),
                       site = rep(c("vir","mad","mari","nca","haw","jap"), each=nm_n))


#plot
quantiles_95 <- function(x) {
  r <- c(mean(x), quantile(x, probs=c(0.05)),quantile(x, probs=c(0.95)))
  names(r) <- c("y","ymin","ymax")
  r
}

mod_plot <- ggplot(data=mod_null, aes(x=factor(site, levels = c("haw","mad","mari","nca","jap","vir")), y=mod, fill=site)) +
  stat_summary(fun.data = quantiles_95, geom ="errorbar", width=0.08) +
  stat_summary(fun = mean, geom="point",size=3.3,aes(shape = "Null distributions", color = "Null distributions" )) + 
  geom_point(data = mod_obs, aes(y = mod, x=factor(site, levels = c("haw","mad","mari","nca","jap","vir")), shape = "Observed values", color = "Observed values" ),size=3.3) + 
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


mod_plot

#modularity with body ratio >0.25 & >0.5
mod_null_0.25 <- mod_null %>% mutate(null = "0.25")
mod_null_0.5 <- mod_null %>% mutate  (null = "0.5")

mod_null_sup <- rbind(mod_null_0.25, mod_null_0.5) 
mod_null_sup$null <- factor(mod_null_sup$null,levels = c("0.25","0.5"))

mod_plot_supp <- ggplot(data=mod_null_sup, aes(x=factor(site, levels = c("haw","mad","mari","nca","jap","vir")), y=mod, color = null)) +
  stat_summary(fun.data = quantiles_95, geom ="errorbar", width=0.08) +
  stat_summary(fun = mean, geom="point",size=3.3) + 
  geom_point(data = mod_obs, aes(y = mod, x=factor(site, levels = c("haw","mad","mari","nca","jap","vir")), shape = "Observed values"), size=3.3, color = "black") + 
  scale_color_manual(name = "Null distributions", values = c("grey26","grey60"))+
  scale_shape_manual(name = NULL, values = 19) +
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
  theme(legend.position = "right",
        legend.box = NULL)



      #5. Network role - CZ values ####
sites <- c("vir", "mad","mari","nca","haw","jap")
list_metamod <- list()
set.seed(2)
for(m in 1:6){
    mat <- mat_sp[[m]]
    metamod_mat <- metaComputeModules(mat, N = 100) #metaComputemodules to generate the algorithme 100 times and return the most modular 
    cz_df <- czvalues(metamod_mat, weighted = FALSE, level = "higher") %>% 
      data.frame(.) %>%
      rownames_to_column(., var = "species") %>%
      mutate(site = sites[m])
    list_metamod[[m]] <- cz_df
  }

cz_full <- do.call(rbind, list_metamod) %>%
  mutate(role = case_when(z <= 2.5 & c <= 0.62 ~ "periph",
                          z <= 2.5 & c > 0.62 ~"connect",
                          z > 2.5 & c > 0.62 ~ "nethub",
                          z > 2.5 & c <= 0.62 ~ "modhub")) 
 
#to get percentage of species per role
cz_full %>% group_by(role) %>% dplyr::summarize (n = n()) %>%
  mutate(per = n/sum(n)*100) 


#plot
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

  #6. Combine plots and save ####
mod.plot.cb <- ggarrange(mod_plot, netrole_plot, 
          labels = c("A","B"),
          font.label = list(size = 12, face = "bold"),
          ncol = 1, nrow = 2, align = "v",
          heights = c(1,2))
mod.plot.cb 

ggsave("output/figures/modularity & network role_binarynet_Newman.png", plot = mod.plot.cb, units="in", width=9, height=11, dpi=300)

ggsave("output/figures/supp_mod_body ratio 0.25.png", plot = mod.plot.cb, units="in", width=9, height=11, dpi=300)

