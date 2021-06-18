####################################
#Node overlap and segregation - nos#
####################################

#packages
library(tidyverse); library(reshape2); library(nos); library(ggpubr); library(ggridges)

    #1. load data
a <- read.csv('data/foodweb_final.csv',header = T)
sites <- unique(a$site)
a <- a %>% select(., site, cons_sp, lev1, w,cons_size)


#function to obtain the expected overlap
get_pot_net<-function(b){ #not the fastest way, but it works...
  pot<-c()
  for (i in 1:dim(b)[1]){
    bs<-b[i,5]
    for (j in 1:dim(b)[1]){
      bs_<-b[j,5]
      ratio<-bs/bs_
      if (i>=j && ratio>0.8 && ratio<1.2){
        pot<-rbind(pot,c(b[j,2],b[i,1]))#,ratio))  
      }
    }
  }
  pot<-unique(pot)
  return (pot)}

    #2. nos values for each site and 3 interaction strength thresholds (0, 0.25 & 0.5)
sites<-c("haw","mad","mari","nca","jap","vir")
nos_res<-list()
for (site in sites){
  for (tre in c(0,0.25,0.5)){
    b<-a[a$site==site,]
    b<-b[b$w>tre,]
    b$cons_sp<-as.character(b$cons_sp)
    b$lev1<-as.character(paste0('prey_',b$lev1))
    nnn<-NOSM_POT_dir(b[,c(3,2)],get_pot_net(b), perc = 1, sl = 0)
    res_in<-data.frame(value = nnn$ov_in, nos = 'in', threshold = tre, site = site)
    res_out<-data.frame(value = nnn$ov_out, nos = 'out', threshold = tre, site = site)
    nos_res<-rbind(nos_res,res_in,res_out)
    print(c(site,tre))
  }
}


nos_res <- nos_res %>% mutate (nos = as.character(nos),
                               threshold = as.factor(threshold),
                               site = as.factor(site))

#modularity as the sd(NOS) for each goup and interction strength threshold
nos_res %>% filter(nos == "in") %>% group_by(site, threshold) %>%
  summarise_at(vars(value), list (mod = sd))

  #3. plots
site_names<-c("Hawaii","Madagascar","Marshall Islands","New Caledonia","Okinawa","West Indies")
plots<-list()
for (n in 1:6){
  loc_data<-nos_res[nos_res$site==sites[n],]
  sub_plot <- ggplot(data = subset(loc_data, nos %in% "in"), mapping = aes(x = value, fill = threshold, color = threshold)) +
  geom_density(alpha = 0.1, size = 0.3, aes(y = ..density..)) +
  scale_fill_manual(name = "Interaction strength threshold", values = c("#67e6dc","darkslategray4","black")) + 
  scale_color_manual(name = "Interaction strength threshold",values = c("#67e6dc","darkslategray4","black")) + 
  scale_x_continuous(expand = c(0,0.05)) +
  labs(title = site_names[n],
       y = "", 
       x = "NOS") +
  theme_pubr() +
  theme(plot.title = element_text(size = 6, colour = "black", face = "bold", hjust = 0.5),
        axis.text = element_text(size=6, colour = "black"),
        axis.ticks = element_line(colour = "black", size = 0.1),
        axis.title = element_text(size=7),
        axis.line = element_line(colour = "black", size=0.2)) +
  guides(color = F, fill =F) 
 plots[[n]]<-sub_plot}


NOS_in.plot <- ggpubr::ggarrange(plotlist=plots,
                  ncol = 2, nrow = 3,
                  common.legend = TRUE, legend="bottom") 
NOS_in.plot

ggsave("fig4_NOS_in_plot.pdf", width = 8.7, height = 12, units = "cm", dpi = 300)






