#######################################
#Modularity analysis and network role# 
#######################################

library(tidyverse)
library(bipartite)
library(picante)
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


  #3. Modularity measure ####
#Qualitative webs - Single algorithme run
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

obs_mod <- c(mod_mad_sp@likelihood, mod_nca_sp@likelihood, mod_mari_sp@likelihood, mod_haw_sp@likelihood, mod_vir_sp@likelihood,mod_jap_sp@likelihood) %>%
  as.data.frame() %>%
  dplyr::rename(., mod =".") %>%
  add_column(site = c("mad","nca","mari","haw","vir","jap")) %>%
  add_column(obs = "obs")

nb_mod <- c(dim(mod_mad_sp@modules)[1]-1, dim(mod_nca_sp@modules)[1]-1, dim(mod_mari_sp@modules)[1]-1, dim(mod_haw_sp@modules)[1]-1, dim(mod_vir_sp@modules)[1]-1, dim(mod_jap_sp@modules)[1]-1) %>%
  as.data.frame() %>%
  dplyr::rename(., nb_mod =".") %>%
  add_column(site = c("mad","nca","mari","haw","vir","jap")) %>%
  add_column(obs = "obs")

dim(mod_mad_sp@modules)[1]-1

plotModuleWeb(mod_mad_sp)

  #4. Null models
#Curveball algorithme of Giovanni
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
} #function

p <- 100
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

  #5. Plot
quantiles_95 <- function(x) {
  r <- c(mean(x), quantile(x, probs=c(0.05)),quantile(x, probs=c(0.95)))
  names(r) <- c("y","ymin","ymax")
  r
}


mod_plot2 <- ggplot(data=mod_null3, aes(x=factor(site, levels = c("haw","mad","mari","nca","jap","vir")), y=mod, fill=site)) +
  geom_point(data=obs_mod, size=3, aes(shape="Observed values", color ="Observed values"))+
  stat_summary(fun.data = quantiles_95, geom ="errorbar", width=0.08) +
  stat_summary(fun.y = mean, geom="point",size=3.3,aes(shape = "Null distributions", color = "Null distributions" )) + 
  #scale_fill_manual(values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
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
                     na.translate = F) +
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

ggsave(ggsave("output/figures/network role plot_facet.png", plot = netplot_facet, units="in", width=11, height=7, dpi=300)
)


  #7. Combine plots and save
mod.plot.cb <- ggarrange(mod_plot2, netrole_plot, 
          labels = c("A","B"),
          font.label = list(size = 12, face = "bold"),
          ncol = 1, nrow = 2, align = "v",
          heights = c(1,2))
ggsave("output/figures/modularity & network role.png", plot = mod.plot.cb, units="in", width=9, height=11, dpi=300)






