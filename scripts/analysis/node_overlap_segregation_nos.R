####################################
#Node overlap and segregation - nos#
####################################
library(tidyverse)
library(reshape2)
library(nos)
library(ggpubr)
library(ggridges)

get_pot_net<-function(b){ #not the fastest way, but it works...
  pot<-c()
  for (i in 1:dim(b)[1]){
    bs<-b[i,4]
    for (j in 1:dim(b)[1]){
      bs_<-b[j,4]
      ratio<-bs/bs_
      if (i>=j && ratio>0.8 && ratio<1.2){
        pot<-rbind(pot,c(b[j,2],b[i,1]))#,ratio))  
      }
    }
  }
  pot<-unique(pot)
  return (pot)}

a<-read.csv('data/foodweb_final.csv',header = T)
sites<-unique(a$site)
a <- a %>% select(., site, cons_class, cons_fam, cons_sp, cons_size, lev1, lev2, w) %>% unique()
#a<-aggregate(w~site+cons_class+cons_fam+cons_sp+cons_size+lev1+lev2, a, sum)

sites<-c("haw","mad","mari","nca","vir","jap")
#value nos   threshold site 
nos_res<-list()
for (site in sites){
  for (tre in c(0,0.25,0.5)){
    b<-a[a$site==site,c(4,7,8,5)]
    b<-b[b$w>tre,]
    b[,1]<-as.character(b[,1])
    b[,2]<-as.character(paste0('prey_',b[,2]))
    nnn<-NOSM_POT_dir(b[,c(2,1)],get_pot_net(b), perc = 1, sl = 0)
    #nnn<-NOSM_bip(b[,c(2,1)], perc = 1, sl = 0) use this to get the "old" results
    res_in<-data.frame(value = nnn$ov_in, nos = 'in', threshold = tre, site = site)
    res_out<-data.frame(value = nnn$ov_out, nos = 'out', threshold = tre, site = site)
    nos_res<-rbind(nos_res,res_in,res_out)
    print(c(site,tre))
    }
  }

nos_res$nos<-as.character(nos_res$nos)
nos_res$threshold<-as.character(nos_res$threshold)
nos_res$site<-as.character(nos_res$site)
site_names<-c("Hawaii","Madagascar","Marshall Islands","New Caledonia","West Indies","Okinawa")

nos_vir <- nos_res %>% filter(site == "vir", nos == "in") 
sd(nos_vir$value)
nos_nca <- nos_res %>% filter(site == "nca", nos == "in") 
sd(nos_nca$value)
nos_mari <- nos_res %>% filter(site == "mari", nos == "in") 
sd(nos_mari$value)
nos_mad <- nos_res %>% filter(site == "mad", nos == "in") 
sd(nos_mad$value)
nos_haw <- nos_res %>% filter(site == "haw", nos == "in") 
sd(nos_haw$value)
nos_jap <- nos_res %>% filter(site == "jap", nos == "in") 
sd(nos_jap$value)

plots<-list()
for (n in 1:6){
  loc_data<-nos_res[nos_res$site==sites[n],]
  sub_plot <- ggplot(data = subset(loc_data, nos %in% "in"), mapping = aes(x = value, fill = threshold, color = threshold)) +
  geom_density(alpha = 0.1, size = 0.3, aes(y = ..density..)) +
  scale_fill_manual(values = c("#67e6dc","darkslategray4","black")) + #c("grey70", "grey35","black"))
  scale_color_manual(values = c("#67e6dc","darkslategray4","black")) + #c("gold", "coral","darkorchid4")) #67e6dc
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




NOS_in.plot <- ggpubr::ggarrange(plotlist=plots,#hawNOS_p, madNOS_p, mariNOS_p, ncaNOS_p, japNOS_p, virNOS_p,
                  ncol = 2, nrow = 3,
                  common.legend = TRUE, legend="bottom") 

ggsave("fig4_NOS_in_plot.pdf", width = 8.7, height = 12, units = "cm", dpi = 300)






