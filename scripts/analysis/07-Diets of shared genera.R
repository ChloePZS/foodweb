###########################################
#Diets of shared genera across all 5 sites#
###########################################

library(tidyverse)


#1. Import clean dataset
data_ISfull <- read.csv("data/data_ISfull.csv") %>% separate(., fish_sp, into=c("genus","sp")) %>%
  mutate(fish_sp = paste(genus,sp, sep=" "))

data_ISfull %>% filter(genus =="Scarus")#Ok changed to labridae


#2. Get the genus that are in common accross all 5 regions
data_commgen <- data_ISfull %>% filter(genus == "Synodus" | genus =="Sargocentron" | genus =="Neoniphon" | genus =="Cephalopholis" | genus =="Acanthurus")

unique(data_commgen$fish_sp) #38 species 

genussp <- data_commgen %>% group_by(genus, site_code, fish_sp) %>% count(grp6)

data_vir_gen_sum %>% filter(genus == "Cephalopholis")

data_commgen %>% filter(site_code =="vir" & genus == "Cephalopholis")

rm(a)


#So try from the matrices so standardized interaction strength
acanth <- as.data.frame(as.table(vir_ISmatrix_gen2[,5])) %>% mutate (genus = "Acanthurus", site ="vir") %>% 
  rbind(as.data.frame(as.table(mari_ISmatrix_gen2[,5])) %>% mutate (genus = "Acanthurus", site ="mari")) %>%
  rbind(as.data.frame(as.table(haw_ISmatrix_gen2[,5])) %>% mutate (genus = "Acanthurus", site ="haw")) %>% 
  rbind(as.data.frame(as.table(mad_ISmatrix_gen2[,5])) %>% mutate (genus = "Acanthurus", site ="mad")) %>% 
  rbind(as.data.frame(as.table(nca_ISmatrix_gen2[,5])) %>% mutate (genus = "Acanthurus", site ="nca")) %>%
  filter(Freq != 0)

#Synodus
synodus <- as.data.frame(as.table(vir_ISmatrix_gen2[,253])) %>% mutate (genus = "Synodus", site ="vir") %>% 
  rbind(as.data.frame(as.table(mari_ISmatrix_gen2[,253])) %>% mutate (genus = "Synodus", site ="mari")) %>%
  rbind(as.data.frame(as.table(haw_ISmatrix_gen2[,253])) %>% mutate (genus = "Synodus", site ="haw")) %>% 
  rbind(as.data.frame(as.table(mad_ISmatrix_gen2[,253])) %>% mutate (genus = "Synodus", site ="mad")) %>% 
  rbind(as.data.frame(as.table(nca_ISmatrix_gen2[,253])) %>% mutate (genus = "Synodus", site ="nca")) %>%
  filter(Freq != 0)

#Sargocentron
sargo <- as.data.frame(as.table(vir_ISmatrix_gen2[,224])) %>% mutate (genus = "Sargocentron", site ="vir") %>% 
  rbind(as.data.frame(as.table(mari_ISmatrix_gen2[,224])) %>% mutate (genus = "Sargocentron", site ="mari")) %>%
  rbind(as.data.frame(as.table(haw_ISmatrix_gen2[,224])) %>% mutate (genus = "Sargocentron", site ="haw")) %>% 
  rbind(as.data.frame(as.table(mad_ISmatrix_gen2[,224])) %>% mutate (genus = "Sargocentron", site ="mad")) %>% 
  rbind(as.data.frame(as.table(nca_ISmatrix_gen2[,224])) %>% mutate (genus = "Sargocentron", site ="nca")) %>%
  filter(Freq != 0)

#Neoniphon
neoniphon <- as.data.frame(as.table(vir_ISmatrix_gen2[,167])) %>% mutate (genus = "Neoniphon", site ="vir") %>% 
  rbind(as.data.frame(as.table(mari_ISmatrix_gen2[,167])) %>% mutate (genus = "Neoniphon", site ="mari")) %>%
  rbind(as.data.frame(as.table(haw_ISmatrix_gen2[,167])) %>% mutate (genus = "Neoniphon", site ="haw")) %>% 
  rbind(as.data.frame(as.table(mad_ISmatrix_gen2[,167])) %>% mutate (genus = "Neoniphon", site ="mad")) %>% 
  rbind(as.data.frame(as.table(nca_ISmatrix_gen2[,167])) %>% mutate (genus = "Neoniphon", site ="nca")) %>%
  filter(Freq != 0)

#Cephalopholis
cephalopholis <- as.data.frame(as.table(vir_ISmatrix_gen2[,48])) %>% mutate (genus = "Cephalopholis", site ="vir") %>% 
  rbind(as.data.frame(as.table(mari_ISmatrix_gen2[,48])) %>% mutate (genus = "Cephalopholis", site ="mari")) %>%
  rbind(as.data.frame(as.table(haw_ISmatrix_gen2[,48])) %>% mutate (genus = "Cephalopholis", site ="haw")) %>% 
  rbind(as.data.frame(as.table(mad_ISmatrix_gen2[,48])) %>% mutate (genus = "Cephalopholis", site ="mad")) %>% 
  rbind(as.data.frame(as.table(nca_ISmatrix_gen2[,48])) %>% mutate (genus = "Cephalopholis", site ="nca")) %>%
  filter(Freq != 0)


#Set specific colors or each prey
genus_com <- rbind(acanth, neoniphon, cephalopholis, synodus, sargo)
colfunc4 <- colorRampPalette(c("darkorchid4","steelblue","turquoise1","seagreen3", "gold","orange","brown2","lightpink2")) #,"slateblue4", 
plot(rep(1,35),col=(colfunc4(35)),pch=15,cex=2) #plot to see the colours


prey <- data.frame(Var1 = unique(genus_com$Var1[order(genus_com$Var1)]), 
                   colors = colfunc4(24)) #24


prey$colors <- as.character(prey$colors)
prey$Var1 <- as.character(prey$Var1) 

manual_scale_colors <- setNames(prey$colors, prey$Var1)


#Barplots
ggplot(data = subset(acanth), aes(x=site, y=Freq, fill = Var1))+
  geom_bar(stat="identity", color="white")+
  scale_fill_manual(name  = "Prey items", values = manual_scale_colors) + 
  labs(y = "Interaction strength", x = "") + 
  ggtitle("a) Acanthurus") +
  theme_minimal() 



ggplot(data = subset(synodus), aes(x=site, y=Freq, fill = Var1))+
  geom_bar(stat="identity",  color="white")+
  scale_fill_manual(values = manual_scale_colors) +
  labs(y = "Interaction strength", x = "") + 
  ggtitle("d) Synodus") +
  theme_minimal()


ggplot(data = subset(neoniphon), aes(x=site, y=Freq, fill = Var1))+
  geom_bar(stat="identity", colour = "white")+
  scale_fill_manual(values = manual_scale_colors) +
  labs(y = "Interaction strength", x = "") + 
  ggtitle("b) Neoniphon") +
  theme_minimal()

ggplot(data = subset(sargo), aes(x=site, y=Freq, fill = Var1))+
  geom_bar(stat="identity", colour = "white") +
  scale_fill_manual(values = manual_scale_colors) +
  labs(y = "Interaction strength", x = "") + 
  ggtitle("c) Sargocentron") +
  theme_minimal()

ggplot(data = subset(cephalopholis), aes(x=site, y=Freq, fill = Var1))+
  geom_bar(stat="identity", colour = "white") +
  scale_fill_manual(values = manual_scale_colors) +
  labs(y = "Interaction strength", x = "") + 
  ggtitle("e) Cephalopholis") +
  theme_minimal()


#Facet plot with genus and site but don't see much .... don't why on 0-2 scale !
genus_com %>%glimpse()

ggplot(data = subset(genus_com), aes(x="", y=Freq, fill = Var1))+
  geom_bar(stat="identity", colour = "white") +
  facet_grid(genus~site) +
  #scale_fill_manual(name  = "Prey items", values = manual_scale_colors)
  scale_fill_discrete()


#Get species name of each genera
gen_com_sp <- data_ISfull %>% filter(genus =="Acanthurus" | genus =="Synodus" | genus =="Cephalopholis" | genus == "Sargocentron" | genus == "Neoniphon") %>%
  group_by(genus,fish_sp, site_code) %>% count(fish_sp)


#Maybe could check same species in the indo-pacific
# Synodus variegatus , Sargocentron diadema, Neoniphon sammara , Cephalopholis argus 
syn.var <- as.data.frame(as.table(vir_ISmatrix_sp2[,597])) %>% mutate (fish_sp = "Synodus variegatus", site ="vir") %>% 
  rbind(as.data.frame(as.table(mari_ISmatrix_sp2[,597])) %>% mutate (fish_sp = "Synodus variegatus", site ="mari")) %>%
  rbind(as.data.frame(as.table(haw_ISmatrix_sp2[,597])) %>% mutate (fish_sp = "Synodus variegatus", site ="haw")) %>% 
  rbind(as.data.frame(as.table(mad_ISmatrix_sp2[,597])) %>% mutate (fish_sp = "Synodus variegatus", site ="mad")) %>% 
  rbind(as.data.frame(as.table(nca_ISmatrix_sp2[,597])) %>% mutate (fish_sp = "Synodus variegatus", site ="nca")) %>%
  filter(Freq != 0)

sar.dia <- as.data.frame(as.table(vir_ISmatrix_sp2[,511])) %>% mutate (fish_sp = "Sargocentron diadema", site ="vir") %>% 
  rbind(as.data.frame(as.table(mari_ISmatrix_sp2[,511])) %>% mutate (fish_sp = "Sargocentron diadema", site ="mari")) %>%
  rbind(as.data.frame(as.table(haw_ISmatrix_sp2[,511])) %>% mutate (fish_sp = "Sargocentron diadema", site ="haw")) %>% 
  rbind(as.data.frame(as.table(mad_ISmatrix_sp2[,511])) %>% mutate (fish_sp = "Sargocentron diadema", site ="mad")) %>% 
  rbind(as.data.frame(as.table(nca_ISmatrix_sp2[,511])) %>% mutate (fish_sp = "Sargocentron diadema", site ="nca")) %>%
  filter(Freq != 0)



####Try with ggplot2 network####
genus_com %>% mutate(shares = case_when(Var1 == Var1 ~ paste(site, sep = "-")))

cols <- c("Var1", "site")
z <- reshape2::dcast(genus_com, genus + Freq ~ site, value.var = "Var1", fun.aggregate =function(x) paste(x[1])) 

z[z == "NA"] <- NA

z <- z %>% mutate(shares = case_when(mad == mari ~ "mad-mari",
                                     mad == haw ~ "mad-haw")) #deosn't work coz noeed to be on the same rows ..;


write.csv(genus_com, "output/genus_com.csv")

genus_com <- read.csv2("output/genus_com.csv")

genus_com$site <- as.factor(genus_com$site)
ggplot(data=genus_com,  aes(from_id = Var1, to_id = site)) +
  geom_net(aes(colour =  shares, shape = site),
           lwd=3, labelon = FALSE, alpha = 0.25, ealpha = 0.25, arrowsize = 0.3, linewidth = 0.3,
           vjust = 0.5, hjust = 0.5)+
  facet_grid(.~genus) +
  theme_net() 




####Try MDS####
acanth.mat <- rbind(vir_ISmatrix_gen2[,5], mari_ISmatrix_gen2[,5], mad_ISmatrix_gen2[,5], nca_ISmatrix_gen2[,5], haw_ISmatrix_gen2[,5])
rownames(acanth.mat) <- c("vir","mari","mad","nca","haw")

d <- dist(t(acanth.mat)) # euclidean distances between the rows
fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
fit # view results

# plot solution
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Metric MDS", type="n")
text(x, y, labels = row.names(acanth.mat), cex=.7) 


gut_mds <- metaMDS(acanth.mat, k = 2)

gut_mds$points
ordiplot(gut_mds,type="n")
orditorp(gut_mds,display="species",col="red",air=0.01)
orditorp(gut_mds,display="sites",col=c(rep("green",5),rep("blue",5)),
         air=0.01,cex=1.25)




#install.packages("vegan")
library(vegan)
set.seed(2)
community_matrix=matrix(
  sample(1:100,300,replace=T),nrow=10,
  dimnames=list(paste("community",1:10,sep=""),paste("sp",1:30,sep="")))

example_NMDS=metaMDS(community_matrix, # Our community-by-species matrix
                     k=2) # The number of reduced dimensions
treat=c(rep("Treatment1",5),rep("Treatment2",5))
ordiplot(example_NMDS,type="n")
ordihull(example_NMDS,groups=treat,draw="polygon",col="grey90",label=F)
orditorp(example_NMDS,display="species",col="red",air=0.01)
orditorp(example_NMDS,display="sites",col=c(rep("green",5),rep("blue",5)),
         air=0.01,cex=1.25)
