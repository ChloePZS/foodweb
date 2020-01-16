####################################
#Node overlap and segregation - nos#
####################################

library(tidyverse)
library(nos)
library(extrafont)
library(ggpubr)
library(ggridges)

#Try with only one matrix
m <- freqMat_2_edge(haw_ISmatrix_sp_std, bip = TRUE, sp_nam = TRUE) 
#returns a df, each row is an interaction prey/pred (pair of nodes between the two sets of nodes)
#in the first col are the preys and in the second columns are the predators eating that prey

nos_res <- NOSM_bip(m, perc = 1, sl = 0) 
#returns a list of two elements, first is the node overlap values of the in-nodes and the second of the out-nodes
res <- summary(nos_res)
#provides with in, out and average values of NOS and MOD
#test statistic with Z and p-value obtained from normal distribution
#non random network structure of Z signif different from 0. 

#With one matrix data exploration
a <- haw_ISmatrix_sp_std
a <- a * (a > 0.50)
m <- freqMat_2_edge(a, bip = TRUE, sp_nam = TRUE) 
#returns a df, each row is a node pair
#in the first col the preys and in the second columns is the number of sp eating that prey
#each node is assigned to a number

nos_res <- NOSM_bip(m, perc = 1, sl = 0) 
#returns a list of two elements, first is the node overlap values of the in-nodes and the second of the out-nodes
res <- summary(nos_res, y = 3)
hist(nos_res$ov_in,20,freq=T) 
hist(nos_res$ov_out,20,freq=T) 
h <- hist(c(nos_res$ov_out, nos_res$ov_in), breaks = 20, plot = F) #frq= TRUE for the counts and frq=FALSE to have probability densities
h$counts <- h$counts/sum(h$counts) * 100 #to get relative frequency (which is actually like densty by a factor 10)
h$density <- h$density/10
plot(h, freq = T, xlab = "NOS", ylab = "Pairs of nodes (%)")


res_tot <- c(nos_res$ov_out, nos_res$ov_in) #nb of pair of nodes in total, pairs of preys + pairs of consumers


# 1. Pdf output nos_in and nos_out values ####
#Loop for each matrix
mmmm <- c("data/haw_ISmatrix_sp_std_grp6.csv","data/mad_ISmatrix_sp_std_grp6.csv","data/mari_ISmatrix_sp_std_grp6.csv",
          "data/nca_ISmatrix_sp_std_grp6.csv","data/jap_ISmatrix_sp_std_grp6.csv","data/vir_ISmatrix_sp_std_grp6.csv")
pdf('output/nos.pdf',width = 10,height = 25)
par(mfrow=c(5,2))
for (i in mmmm){
  a <- read.csv(i, header=TRUE, row.names=1, sep=",")
  m <- freqMat_2_edge(a, bip = TRUE, sp_nam = TRUE) #bip for bipartite
  nos_res <- NOSM_bip(m, perc = 1, sl = 0)
  res <- summary(nos_res)
  NOS_In<-res[1]
  NOS_Out<-res[2]
  NOS<-res[3]
  MOD_In<-res[4] 
  MOD_Out<-res[5] 
  MOD<-res[6]
  hist(nos_res$ov_in,20,freq=T,xlab='overlap',main=paste(i,'\n','nos_in: ',NOS_In,' mod_in: ',MOD_In))
  hist(nos_res$ov_out,20,freq=T,xlab='overlap',main=paste('nos_out: ',NOS_Out,' mod_out: ',MOD_Out))
  print (c(summary(nos_res)[3],summary(nos_res)[6]))}


dev.off()
graphics.off()

#With threshold on interaction strength
mmmm <- c("data/haw_ISmatrix_sp_std_grp6.csv","data/mad_ISmatrix_sp_std_grp6.csv","data/mari_ISmatrix_sp_std_grp6.csv",
          "data/nca_ISmatrix_sp_std_grp6.csv","data/vir_ISmatrix_sp_std_grp6.csv")

pdf('output/nos_50.pdf',width = 10,height = 25)
par(mfrow=c(3,2))
for (i in mmmm){
  a <- read.csv(i, header=TRUE, row.names=1, sep=",")
  a <-a*(a > 0.25)
  m <- freqMat_2_edge(a, bip = TRUE, sp_nam = TRUE) #bip for bipartite
  nos_res <- NOSM_bip(m, perc = 1, sl = 0)
  res <- summary(nos_res)
  NOS_In<-res[1]
  NOS_Out<-res[2]
  NOS<-res[3]
  MOD_In<-res[4] 
  MOD_Out<-res[5] 
  MOD<-res[6]
  hist(nos_res$ov_in,20,freq=T,xlab='overlap',main=paste(i,'\n','nos_in: ',NOS_In,' mod_in: ',MOD_In))
  hist(nos_res$ov_out,20,freq=T,xlab='overlap',main=paste('nos_out: ',NOS_Out,' mod_out: ',MOD_Out))
  print (c(summary(nos_res)[3],summary(nos_res)[6]))}


  #hist(nos_res$ov_in,20,freq=T,xlab='overlap',main=paste(i,'\n','nos_in: ',NOS_In,' mod_in: ',MOD_In))
  #hist(nos_res$ov_out,20,freq=T,xlab='overlap',main=paste('nos_out: ',NOS_Out,' mod_out: ',MOD_Out))
  #print (c(summary(nos_res)[3],summary(nos_res)[6]))}


dev.off()

#Other grps
mmmm <- c("data/haw_ISmatrix_sp_std_grp1.csv","data/mad_ISmatrix_sp_std_grp1.csv","data/mari_ISmatrix_sp_std_grp1.csv",
          "data/nca_ISmatrix_sp_std_grp1.csv","data/vir_ISmatrix_sp_std_grp1.csv")
pdf('output/nos_grp1.pdf',width = 10,height = 25)
par(mfrow=c(3,2))
for (i in mmmm){
  a <- read.csv(i, header=TRUE, row.names=1, sep=",")
  m <- freqMat_2_edge(a, bip = TRUE, sp_nam = TRUE) #bip for bipartite
  nos_res <- NOSM_bip(m, perc = 1, sl = 0)
  res <- summary(nos_res)
  NOS_In<-res[1]
  NOS_Out<-res[2]
  NOS<-res[3]
  MOD_In<-res[4] 
  MOD_Out<-res[5] 
  MOD<-res[6]
  hist(nos_res$ov_in,20,freq=T,xlab='overlap',main=paste(i,'\n','nos_in: ',NOS_In,' mod_in: ',MOD_In))
  hist(nos_res$ov_out,20,freq=T,xlab='overlap',main=paste('nos_out: ',NOS_Out,' mod_out: ',MOD_Out))
  print (c(summary(nos_res)[3],summary(nos_res)[6]))}


dev.off()


#Percentage of pairs of nodes

for(i in mmmm){
    a <- read.csv(i, header=TRUE, row.names=1, sep=",")
    m <- freqMat_2_edge(a, bip = TRUE, sp_nam = TRUE) #bip for bipartite
    nos_res <- NOSM_bip(m, perc = 1, sl = 0)
    res_tot <- c(nos_res$ov_in, nos_res$ov_out)
df <- data_frame("region" = paste(substr(i, start = 6, stop = 8)),
           '% positive' = round(length(which(res_tot > 0.5))/length(res_tot)*100, digits = 2),
           '% negative' = round(length(which(res_tot < -0.5))/length(res_tot)*100, digits = 2))
print(df)
}

#get values
for(i in mmmm){
  a <- read.csv(i, header=TRUE, row.names=1, sep=",")
  m <- freqMat_2_edge(a, bip = TRUE, sp_nam = TRUE) #bip for bipartite
  nos_res <- NOSM_bip(m, perc = 1, sl = 0)
  res_tot <- c(nos_res$ov_in, nos_res$ov_out)
  print(summary(nos_res)[4])
}

# 2. Get nice graph with ggplot2  ####
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


#2. Get the NOS measures
vir_NOS <- freqMat_2_edge(vir_ISmatrix_sp_std, bip = TRUE, sp_nam = TRUE) %>%
  NOSM_bip(., perc = 1, sl = 0)
summary(vir_NOS)

mari_NOS <- freqMat_2_edge(mari_ISmatrix_sp_std, bip = TRUE, sp_nam = TRUE) %>%
  NOSM_bip(., perc = 1, sl = 0)
summary(mari_NOS)

haw_NOS <- freqMat_2_edge(haw_ISmatrix_sp_std, bip = TRUE, sp_nam = TRUE) %>%
  NOSM_bip(., perc = 1, sl = 0)
summary(haw_NOS)

mad_NOS <- freqMat_2_edge(mad_ISmatrix_sp_std, bip = TRUE, sp_nam = TRUE) %>%
  NOSM_bip(., perc = 1, sl = 0)
summary(mad_NOS)

nca_NOS <- freqMat_2_edge(nca_ISmatrix_sp_std, bip = TRUE, sp_nam = TRUE) %>%
  NOSM_bip(., perc = 1, sl = 0)
summary(nca_NOS)

#Combine into one dataframe
NOS_df <- data.frame("nos" = c(vir_NOS$ov_in, vir_NOS$ov_out),
                     "site" = "vir") %>%
  rbind(data.frame("nos" = c(mari_NOS$ov_in, mari_NOS$ov_out),
                   "site" = "mari")) %>%
  rbind(data.frame("nos" = c(haw_NOS$ov_in, haw_NOS$ov_out),
             "site" = "haw")) %>%
  rbind(data.frame("nos" = c(mad_NOS$ov_in, mad_NOS$ov_out),
             "site" = "mad")) %>%
  rbind(data.frame("nos" = c(nca_NOS$ov_in, nca_NOS$ov_out),
                   "site" = "nca")) %>%
  mutate_if(is.factor, as.character)
  

#3. Get the plots
graphics.off()
#base R
h <- hist(c(nca_NOS$ov_in, nca_NOS$ov_out), breaks = 20, plot =F)
h$counts <- h$counts/sum(h$counts) * 100 #to get relative frequency (which is actually like densty by a factor 10)
plot(h, freq = T, xlab = "NOS", ylab = "Pairs of nodes (%)", main="New Caledodonia")


#facet plot
#fonts()
site.labs <- c("Hawaii", "Madagascar", "Marshall Islands","New Caledonia","West Indies")
names(site.labs) <- c("haw","mad","mari","nca","vir")

ggplot(data=NOS_df, aes(x=nos)) +
  geom_histogram(aes(y = stat(width*density)*100, fill = site, color= site),bins = 30, position="identity", alpha=0.55) + 
  scale_fill_manual(values = c("darkorchid4","turquoise3","#7ACF5A","gold","coral1")) +
  scale_color_manual(values = c("darkorchid4","turquoise3","#7ACF5A","gold","coral1")) +
  facet_wrap(. ~ site, scales="free_y", labeller = labeller(site = site.labs)) +
  labs(y = "Pair of nodes (%)\n", x = "\nNOS") + #Add line break "\n" to make space between axis and txt 
  theme_grey(base_size=12, base_family = "Arial") + 
  theme(legend.position = "none",
        strip.text.x = element_text(size=10, face="bold"),
        strip.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = 'white'),
        panel.border = element_rect(colour = "black", fill=NA, size=0.3)) 
##SOLVED Mauvaise échelle sur y-axis --> the solution is to use aes(y = stat(width*density)). This converts the density back into a percentage. 

#With freqpoly
ggplot(data=NOS_df, aes(x=nos)) +
geom_freqpoly(aes(y = stat(density), color= site), binwidth = 0.1) + 
  scale_color_manual(values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  labs(y = "Pair of nodes (%)", x = "Node Overlap and Segregation") +
  theme_classic()


#NICE - Individual plots (to have colors for the title and subtitle with values of NOS and MOD)
#Set either to overall NOS and MOD values, or 'in' or 'out values
hawNOS_plot <- ggplot(data = NULL, mapping = aes(x = haw_NOS$ov_in)) +
  geom_histogram(aes(y=..count../sum(..count..) * 100), fill = "darkorchid4", colour = "darkorchid4", bins = 30, position="identity", alpha=0.55) + 
  ylim(0,30) +
  labs(title = "Hawaii",
       y = "Node pairs (%)", 
       x = "",
       subtitle = paste("NOS = ", round(summary(haw_NOS)[1], digits = 2), "   Mod = ", round(summary(haw_NOS)[4], digits = 2))) + #Add line break "\n" to make space between axis and txt 
  theme_classic(base_size=12, base_family = "Arial") +
  theme(plot.title = element_text(size = 11, colour = "darkorchid4", face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 10, colour = "black", hjust = 0.5, face="bold"),
        legend.position = "none")


virNOS_plot <- ggplot(data = NULL, mapping = aes(x = vir_NOS$ov_in)) +
  geom_histogram(aes(y=..count../sum(..count..) * 100), fill="coral1", colour = "coral1", bins = 30, position="identity", alpha=0.55) + 
  ylim(0,30) +
  labs(title = "West Indies",
       y = "Node pairs (%)", 
       x = "NOS",
       subtitle = paste("NOS = ", round(summary(vir_NOS)[1], digits = 2), "   Mod = ", round(summary(vir_NOS)[4], digits = 2))) + #Add line break "\n" to make space between axis and txt 
  theme_classic(base_size=12, base_family = "Arial") +
  theme(plot.title = element_text(size = 11, colour = "coral1", face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 10, colour = "black", hjust = 0.5, face="bold"),
        legend.position = "none")

madNOS_plot <- ggplot(data = NULL, mapping = aes(x = mad_NOS$ov_in)) +
  geom_histogram(aes(y=..count../sum(..count..) * 100), fill="turquoise3", colour = "turquoise3", bins = 30, position="identity", alpha=0.55) + 
  ylim(0,30) +
  labs(title = "Madagascar",
       y = "", 
       x = "",
       subtitle = paste("NOS = ", round(summary(mad_NOS)[1], digits = 2), "   Mod = ", round(summary(mad_NOS)[4], digits = 2))) + #Add line break "\n" to make space between axis and txt 
  theme_classic(base_size=12, base_family = "Arial") +
  theme(plot.title = element_text(size = 11, colour = "turquoise3", face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 10, colour = "black", hjust = 0.5, face="bold"),
        legend.position = "none")

mariNOS_plot <- ggplot(data = NULL, mapping = aes(x = mari_NOS$ov_in)) +
  geom_histogram(aes(y=..count../sum(..count..) * 100), fill="#7ACF5A", colour = "#7ACF5A", bins = 30, position="identity", alpha=0.55) + 
  ylim(0,30) +
  labs(title = "Marshall Islands",
       y = "Node pairs (%)", 
       x = "",
       subtitle = paste("NOS = ", round(summary(mari_NOS)[1], digits = 2), "    Mod = ", round(summary(mari_NOS)[4], digits = 2))) + #Add line break "\n" to make space between axis and txt 
  theme_classic(base_size=12, base_family = "Arial") +
  theme(plot.title = element_text(size = 11, colour = "#7ACF5A", face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 10, colour = "black", hjust = 0.5, face="bold"),
        legend.position = "none")

ncaNOS_plot <- ggplot(data = NULL, mapping = aes(x = nca_NOS$ov_in)) +
  geom_histogram(aes(y=..count../sum(..count..) * 100), fill="gold", colour = "gold", bins = 30, position="identity", alpha=0.55) + 
  ylim(0,60) +
  labs(title = "New Caledonia",
       y = "", 
       x = "NOS",
       subtitle = paste("NOS = ", round(summary(nca_NOS)[1], digits = 2), "   Mod = ", round(summary(nca_NOS)[4], digits = 2))) + #Add line break "\n" to make space between axis and txt 
  theme_classic(base_size=12, base_family = "Arial") +
  theme(plot.title = element_text(size = 11, colour = "gold", face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 10, colour = "black", hjust = 0.5, face="bold"),
        legend.position = "none")



#Combine plots on a single page
ggpubr::ggarrange(hawNOS_plot, madNOS_plot, mariNOS_plot, ncaNOS_plot, virNOS_plot,
          ncol = 2, nrow = 3) %>%
  annotate_figure(., top = text_grob("NOS_in resources between predators"))


#NOS_out
mmmm <- c("data/haw_ISmatrix_sp_std_grp6.csv","data/mad_ISmatrix_sp_std_grp6.csv","data/mari_ISmatrix_sp_std_grp6.csv",
          "data/nca_ISmatrix_sp_std_grp6.csv","data/vir_ISmatrix_sp_std_grp6.csv")
par(mfrow=c(3,2))
for (i in mmmm){
  a <- read.csv(i, header=TRUE, row.names=1, sep=",")
  m <- freqMat_2_edge(a, bip = TRUE, sp_nam = TRUE) #bip for bipartite
  nos_res <- NOSM_bip(m, perc = 1, sl = 0)
  res <- summary(nos_res)
  NOS_In<-res[1]
  NOS_Out<-res[2]
  NOS<-res[3]
  MOD_In<-res[4] 
  MOD_Out<-res[5] 
  MOD<-res[6]
  h <- hist(nos_res$ov_in,20, plot = F)
  h$counts <- h$counts/sum(h$counts) * 100 #to get relative frequency (which is actually like densty by a factor 10)
  plot(h, freq = T, col = "grey", xlab = "NOS", ylab = "Node pairs (%)", main=paste(substr(i, start = 6, stop = 8), " "))
  title(main = paste('NOS_out =',round(NOS_Out, digits=2),' Mod_out =',round(MOD_Out, digits =2)), line  = 0.5, cex.main = 1)
  mtext("NOS_out values", outer = TRUE, cex = 1.2, side = 3, line = -1.4)
}



#3. Supplementary material ####

#1. Interaction strength threshold
# a > 0.25, 0.5 , ...

mmmm <- c("data/haw_ISmatrix_sp_std_grp6.csv","data/mad_ISmatrix_sp_std_grp6.csv","data/mari_ISmatrix_sp_std_grp6.csv",
          "data/nca_ISmatrix_sp_std_grp6.csv","data/vir_ISmatrix_sp_std_grp6.csv")
par(mfrow=c(3,2))
for (i in mmmm){
  a <- read.csv(i, header=TRUE, row.names=1, sep=",")
  a <- a*(a > 0.5)
  m <- freqMat_2_edge(a, bip = TRUE, sp_nam = TRUE) #bip for bipartite
  nos_res <- NOSM_bip(m, perc = 1, sl = 0)
  res <- summary(nos_res)
  NOS_In<-res[1]
  NOS_Out<-res[2]
  NOS<-res[3]
  MOD_In<-res[4] 
  MOD_Out<-res[5] 
  MOD<-res[6]
  h <- hist(nos_res$ov_in,20, plot = F)
  h$counts <- h$counts/sum(h$counts) * 100 #to get relative frequency (which is actually like densty by a factor 10)
  plot(h, freq = T, col = "grey", xlab = "NOS", ylab = "Node pairs (%)", main=paste(substr(i, start = 6, stop = 8), " "))
  title(main = paste('NOS_in =',round(NOS_In, digits=2),' Mod_in =',round(MOD_In, digits =2)), line  = 0.5, cex.main = 1)
  mtext("NOS_in values & Interaction strength > 0.5", outer = TRUE, cex = 1.2, side = 3, line = -1.4)
}


#Try by removing random links --> need to replace non-zero links by 0
#Need to check how much links removed with IS threshold to remove randomly a similar proportions of links
#haw, mad, mari, nca, vir
#Fun to get the % of links removed by the threshold
for (i in mmmm){
  a <- read.csv(i, header=TRUE, row.names=1, sep=",")
  b <- a*(a > 0.25)
  c <- 100-(length(which(b != 0))/length(which(a != 0))*100)
  print(c)
}

length(m)
length(which(m != 0))
length(which(m > 0.5))
length(which(m*(m > 0.5) != 0))
length(which(m*(m > 0.5) == 0))

#Test with one matrice, but doesn't work inthe loop
#ex 1
m <- nca_ISmatrix_sp_std
m[sample(which(m != 0), 150, replace = FALSE)] <- 0

#ex 2 : set a proportions of replaced values 
df <- unlist(m)
n <- length(which(df != 0)) * 0.80
df[sample(which(df != 0), n)] <- 0

length(which(df != 0))
length(which(m != 0))


#ex 3 : with binomial distribution, but replace 80% overall matrix values, where as I need to replace 80% of the non-zero values
dd=dim(m)
pr=80/100 #overall
df1<-m
df1[matrix(rbinom(prod(dd), size=1, prob=pr)==1, nrow=dd[1])] <- 0

length(which(df1 != 0))



par(mfrow = c(3,2))
for(i in mmmm){
  a <- read.csv(i, header=TRUE, row.names=1, sep=",")
  #df <- unlist(a)
  #n <- length(df) * 0.20
  #df[sample(df, n)] <- 0
  #a[sample(which(a != 0), 150, replace = FALSE)] <- 0 #doesn't work
  m <- freqMat_2_edge(a, bip = TRUE, sp_nam = TRUE) #bip for bipartite
  nos_res <- NOSM_bip(m, perc = 1, sl = 0)
  res <- summary(nos_res)
  NOS_In<-res[1]
  NOS_Out<-res[2]
  NOS<-res[3]
  MOD_In<-res[4] 
  MOD_Out<-res[5] 
  MOD<-res[6]
  h <- hist(nos_res$ov_in,20, plot = F)
  h$counts <- h$counts/sum(h$counts) * 100 #to get relative frequency (which is actually like densty by a factor 10)
  plot(h, freq = T, col = "grey", xlab = "NOS", ylab = "Node pairs (%)", main=paste(substr(i, start = 6, stop = 8), " "))
  title(main = paste('NOS_in =',round(NOS_In, digits=2),' Mod_in =',round(MOD_In, digits =2)), line  = 0.5, cex.main = 1)
  mtext("NOS_in values & random insertion 0", outer = TRUE, cex = 1.2, side = 3, line = -1.4)
}

#Other solution --> matrices created manually the for in loop
m <- mari_ISmatrix_sp_std

m_nca <- unlist(m)
n <- length(which(m_nca != 0)) * 0.60
m_nca[sample(which(m_nca != 0), n)] <- 0

m_vir <- unlist(m)
n <- length(which(m_vir != 0)) * 0.70
m_vir[sample(which(m_vir != 0), n)] <- 0

m_mari <- unlist(m)
n <- length(which(m_mari != 0)) * 0.60
m_mari[sample(which(m_mari != 0), n)] <- 0

m_mad <- unlist(m)
n <- length(which(m_mad != 0)) * 0.80
m_mad[sample(which(m_mad != 0), n)] <- 0

m_haw <- unlist(m)
n <- length(which(m_haw != 0)) * 0.75
m_haw[sample(which(m_haw != 0), n)] <- 0

aaaa <- list(m_haw, m_mad, m_mari, m_nca, m_vir)

par(mfrow = c(3,2))
for(i in aaaa){
  m <- freqMat_2_edge(i, bip = TRUE, sp_nam = TRUE) #bip for bipartite
  nos_res <- NOSM_bip(m, perc = 1, sl = 0)
  res <- summary(nos_res)
  NOS_In<-res[1]
  NOS_Out<-res[2]
  NOS<-res[3]
  MOD_In<-res[4] 
  MOD_Out<-res[5] 
  MOD<-res[6]
  h <- hist(nos_res$ov_in,20, plot = F)
  h$counts <- h$counts/sum(h$counts) * 100 #to get relative frequency (which is actually like densty by a factor 10)
  plot(h, freq = T, col = "grey", xlab = "NOS", ylab = "Node pairs (%)", main="")
  title(main = paste('NOS_in =', round(NOS_In, digits=2),' Mod_in =',round(MOD_In, digits =2)), line  = 0.5, cex.main = 1)
  mtext("NOS_in values & random insertion  of 0 - prop 0.25", outer = TRUE, cex = 1.2, side = 3, line = -1.4)
  print(summary(nos_res)[8])
}

#length(which(m_vir != 0))
#length(which(vir_ISmatrix_sp_std != 0)) #it works, i have more 0 in the new matrices


#2. With other groups ####
#Grp1 
mmmm <- c("data/haw_ISmatrix_sp_std_grp1.csv","data/mad_ISmatrix_sp_std_grp1.csv","data/mari_ISmatrix_sp_std_grp1.csv",
          "data/nca_ISmatrix_sp_std_grp1.csv","data/vir_ISmatrix_sp_std_grp1.csv")
par(mfrow=c(3,2))
for (i in mmmm){
  a <- read.csv(i, header=TRUE, row.names=1, sep=",")
  m <- freqMat_2_edge(a, bip = TRUE, sp_nam = TRUE) #bip for bipartite
  nos_res <- NOSM_bip(m, perc = 1, sl = 0)
  res <- summary(nos_res)
  NOS_In<-res[1]
  NOS_Out<-res[2]
  NOS<-res[3]
  MOD_In<-res[4] 
  MOD_Out<-res[5] 
  MOD<-res[6]
  h <- hist(nos_res$ov_in,20, plot = F)
  h$counts <- h$counts/sum(h$counts) * 100 #to get relative frequency (which is actually like densty by a factor 10)
  plot(h, freq = T, col = "grey", xlab = "NOS", ylab = "Node pairs (%)", main=paste(substr(i, start = 6, stop = 8), " "))
  title(main = paste('NOS_in =',round(NOS_In, digits=2),' Mod_in =',round(MOD_In, digits =2)), line  = 0.5, cex.main = 1)
  mtext("Prey grouping 1", outer = TRUE, cex = 1.2, side = 3, line = -1.4)
}


#Prey class
mmmm <- c("data/haw_ISmatrix_sp_std_item_class.csv","data/mad_ISmatrix_sp_std_item_class.csv","data/mari_ISmatrix_sp_std_item_class.csv",
          "data/nca_ISmatrix_sp_std_item_class.csv","data/vir_ISmatrix_sp_std_item_class.csv")
par(mfrow=c(3,2))
for (i in mmmm){
  a <- read.csv(i, header=TRUE, row.names=1, sep=",")
  m <- freqMat_2_edge(a, bip = TRUE, sp_nam = TRUE) #bip for bipartite
  nos_res <- NOSM_bip(m, perc = 1, sl = 0)
  res <- summary(nos_res)
  NOS_In<-res[1]
  NOS_Out<-res[2]
  NOS<-res[3]
  MOD_In<-res[4] 
  MOD_Out<-res[5] 
  MOD<-res[6]
  h <- hist(nos_res$ov_in,20, plot = F)
  h$counts <- h$counts/sum(h$counts) * 100 #to get relative frequency (which is actually like densty by a factor 10)
  plot(h, freq = T, col = "grey", xlab = "NOS", ylab = "Pairs of nodes (%)", main=paste(substr(i, start = 6, stop = 8), " "))
  title(main = paste('NOS_in =',round(NOS_In, digits=2),' Mod_in =',round(MOD_In, digits =2)), line  = 0.5, cex.main = 1)
  mtext("Prey class", outer = TRUE, cex = 1.2, side = 3, line = -1.4)
  print(summary(nos_res)[8])
}



#4. Trying other plot instead of having all distribution
#only with "in" values (overlap/seg in resources btw consumers####
#Combine into one dataframe
NOS_in_df <- data.frame("nos_in" = c(vir_NOS$ov_in),
                     "site" = "vir") %>%
  rbind(data.frame("nos_in" = c(mari_NOS$ov_in),
                   "site" = "mari")) %>%
  rbind(data.frame("nos_in" = c(haw_NOS$ov_in),
                   "site" = "haw")) %>%
  rbind(data.frame("nos_in" = c(mad_NOS$ov_in),
                   "site" = "mad")) %>%
  rbind(data.frame("nos_in" = c(nca_NOS$ov_in),
                   "site" = "nca")) %>%
  mutate_if(is.factor, as.character)

NOS_out_df <- data.frame("nos_out" = c(vir_NOS$ov_out),
                        "site" = "vir") %>%
  rbind(data.frame("nos_out" = c(mari_NOS$ov_out),
                   "site" = "mari")) %>%
  rbind(data.frame("nos_out" = c(haw_NOS$ov_out),
                   "site" = "haw")) %>%
  rbind(data.frame("nos_out" = c(mad_NOS$ov_out),
                   "site" = "mad")) %>%
  rbind(data.frame("nos_out" = c(nca_NOS$ov_out),
                   "site" = "nca")) %>%
  mutate_if(is.factor, as.character)

tapply(NOS_in_df$nos_in, NOS_in_df$site, sd) #sd gives MOD while mean gives NOS 


#Easy simple barplot with mean +/- sd
NOS_in_summary <- NOS_in_df %>% group_by(site) %>%
  summarise(mean_nos = mean(nos_in),
            mod = sd(nos_in),
            nval = n())

ggplot(NOS_in_summary, aes(x=as.factor(site), y=mean_nos))+
  geom_col()+
  geom_errorbar(aes(ymin=mean_nos-mod, ymax=mean_nos+mod), width=0.1) +
  theme_classic()


#Function to get the summary of a df
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

NOS_in_summary <- data_summary(NOS_in_df, varname="nos_in", 
                    groupnames="site")

#Barplot with sd
ggplot(NOS_in_summary, aes(x=as.factor(site), y=nos_in)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=nos_in-sd, ymax=nos_in+sd), width=.1,
                position=position_dodge(.9)) +
  theme_classic2()


#Functions to get customized boxplot & crossrange plots
min.mean.sd.max <- function(x) {
  r <- c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}


mean_SD <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}


#Simple pointrange with mean and sd
ggplot(NOS_in_df, aes(x=as.factor(site), y=nos_in, color=as.factor(site))) + 
  geom_boxplot() +
  stat_summary(fun.data = mean_SD, geom = "pointrange")


#Violin plot --> show the distribution of the values
ggplot(NOS_in_df, aes(x=as.factor(site), y=nos_in)) + 
  geom_violin(color = "grey50", fill = "grey50", alpha = 0.4, size = 0.7) +
  stat_summary(fun.data = mean_SD, geom = "point", size = 1.5, color = "black") +
  stat_summary(fun.data = stat_box_data, geom = "text", hjust = 0.5, vjust = 0.8, color="black", size = 3) +
  #geom_boxplot(width=0.07) +
  #stat_summary(fun.data = mean_SD, geom = "pointrange") +
  scale_x_discrete(labels= site.lab) +
  labs(x="", y="Overlap/Segregation in resources between predators\n") +
  theme_pubr()+
  theme(legend.position = "none", 
        panel.background = element_rect(colour = "black", size=0.5),
        axis.title = element_text(size=13.5),
        axis.text = element_text(size=12, colour = "black"),
        axis.text.x = element_text(size=12))


#Ridgelines
ggplot(NOS_in_df, aes(x=nos_in, y=as.factor(site))) + 
  geom_density_ridges(color = "grey40", fill = "grey50", alpha = 0.4, size = 0.7) +
  #stat_density_ridges(quantile_lines = TRUE) +
  scale_y_discrete(labels= site.lab) +
  labs(x="\nNOS - Resources between predators", y="Food webs\n") +
  theme_pubr()+
  theme(legend.position = "none", 
        panel.background = element_rect(colour = "black", size=0.5),
        axis.title = element_text(size=13.5),
        axis.text = element_text(size=12, colour = "black"),
        axis.text.x = element_text(size=12))

#Plot with boxes with min, max, mean and sd values). Good coz give idea of the distiribution, show mean NOS.
#Tick labels
site.lab <- c("Hawaii","Madagascar","Marshall Islands","New Caledonia","Virgin Islands")
names(site.lab) <-c("haw","mad","mari","nca","vir")

#Function to annote text on top of boxplots
stat_box_data <- function(y, upper_limit = max(NOS_in_df$nos_in) * 1.35) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('Mod =', round(sd(y), 2), '\n')
    )
  )
}

ggplot(NOS_in_df, aes(x=as.factor(site), y=nos_in, fill=as.factor(site), color = as.factor(site), alpha = 0.6)) + 
  stat_summary(fun.data = min.mean.sd.max , geom = "boxplot", width = 0.5) +
  stat_summary(fun.data = stat_box_data, geom = "text", hjust = 0.5, vjust = 0.8, color="black") +
  scale_fill_manual(values=c("darkorchid4","turquoise3","#7ACF5A","gold","coral1"))+
  scale_color_manual(values=c("darkorchid4","turquoise3","#7ACF5A","gold","coral1"))+
  scale_x_discrete(labels= site.lab) +
  labs(x="", y="Overlap/Segregation in resources between predators\n") +
  theme_pubr()+
  theme(legend.position = "none", 
        panel.background = element_rect(colour = "black", size=0.5),
        axis.title = element_text(size=13.5),
        axis.text = element_text(size=11, colour = "black"),
        axis.text.x = element_text(size=13.5))

#In all gray without colors
NOS_plot <- ggplot(NOS_in_df, aes(x=as.factor(site), y=nos_in, fill=as.factor(site))) + 
  stat_summary(fun.data = min.mean.sd.max , geom = "boxplot", width = 0.5, fill = "grey50", color = "grey50", alpha = 0.7) +
  stat_summary(geom = "crossbar", width=0.45, fatten=1.5, color="white", fun.data = function(x){ return(c(y=mean(x), ymin=mean(x), ymax=mean(x))) })  +
  stat_summary(fun.data = stat_box_data, geom = "text", hjust = 0.5, vjust = 0.8, color="black", size = 3) +
  scale_x_discrete(labels= site.lab) +
  labs(x="", y="Overlap/Segregation in resources between predators\n") +
  theme_pubr()+
  theme(legend.position = "none", 
        panel.background = element_rect(colour = "black", size=0.5),
        axis.title = element_text(size=13.5),
        axis.text = element_text(size=12, colour = "black"),
        axis.text.x = element_text(size=12))

NOS_out_plot <- ggplot(NOS_out_df, aes(x=as.factor(site), y=nos_out, fill=as.factor(site))) + 
  stat_summary(fun.data = min.mean.sd.max , geom = "boxplot", width = 0.5, fill = "grey50", color = "grey50", alpha = 0.7) +
  stat_summary(geom = "crossbar", width=0.45, fatten=1.5, color="white", fun.data = function(x){ return(c(y=mean(x), ymin=mean(x), ymax=mean(x))) })  +
  stat_summary(fun.data = stat_box_data, geom = "text", hjust = 0.5, vjust = 0.8, color="black", size = 3) +
  scale_x_discrete(labels= site.lab) +
  labs(x="", y="Overlap/Segregation in predators between resources\n") +
  theme_pubr()+
  theme(legend.position = "none", 
        panel.background = element_rect(colour = "black", size=0.5),
        axis.title = element_text(size=13.5),
        axis.text = element_text(size=12, colour = "black"),
        axis.text.x = element_text(size=12))


#saving plot
png("output/figures/NOS_out_simple plot(grey).png", units="in", width=10, height=5, res=300)
print(NOS_out_plot)
dev.off()



#Original boxplot
ggplot(NOS_in_df, aes(x=as.factor(site), y=nos_in, color=as.factor(site))) + 
  geom_boxplot()

#Plots with jittered points - bof...
ggplot(NOS_in_df, aes(x=as.factor(site), y=nos_in)) + 
  geom_jitter(position=position_jitter(0.1), shape = 20, size = 1, alpha = 0.1, color = "grey") +
  stat_summary(fun.data=mean_SD, 
               geom="pointrange", color=c("darkorchid4","turquoise3","#7ACF5A","gold","coral1"),
               size=1)  +
  theme_pubr()


#Final plot
irNOS_p <- ggplot(data = subset(vir_NOS_full, nos %in% "in"), mapping = aes(x = value, fill = threshold, color = threshold)) +
  geom_density(alpha = 0.1, size = 0.3, aes(y = ..density..)) +
  scale_fill_manual(values = c("#67e6dc","darkslategray4","black")) + #c("grey70", "grey35","black"))
  scale_color_manual(values = c("#67e6dc","darkslategray4","black")) + #c("gold", "coral","darkorchid4")) #67e6dc
  scale_x_continuous(expand = c(0,0.05)) +
  labs(title = "West Indies",
       y = "", 
       x = "NOS") +
  theme_pubr() +
  theme(plot.title = element_text(size = 6, colour = "black", face = "bold", hjust = 0.5),
        axis.text = element_text(size=6, colour = "black"),
        axis.ticks = element_line(colour = "black", size = 0.1),
        axis.title = element_text(size=7),
        axis.line = element_line(colour = "black", size=0.2)) +
  guides(color = F, fill =F) 
# annotate(geom = "text", x = 0, y = 2.8, label = line1, color = "#67e6dc", size=4, fontface = 2) +
# annotate(geom = "text", x = 0, y = 2.6, label = line2, color = "darkslategray4", size=4, fontface = 2) +
# annotate(geom = "text", x = 0, y = 2.4, label = line3, color = "black", size=4, fontface = 2) 


line1 <- paste("NOS = ", round(summary(vir_NOS)[1], digits = 2), "   Mod = ", round(summary(vir_NOS)[4], digits = 2))
line2 <- paste("NOS = ", round(summary(vir_NOS_0.25)[1], digits = 2), "   Mod = ", round(summary(vir_NOS_0.25)[4], digits = 2))
line3 <- paste("NOS = ", round(summary(vir_NOS_0.5)[1], digits = 2), "   Mod = ", round(summary(vir_NOS_0.5)[4], digits = 2))



# #With % of node pairs, By calling geom_density with the stat = 'bin', the same stat as geom_histogram, instead of the default stat = 'density' for geom_density
# ggplot(data = subset(vir_NOS_full, nos %in% "out"), mapping = aes(x = value, fill = threshold, color = threshold)) +
#   geom_density(alpha = 0.1, size = 1, stat = "bin", aes(y = stat(width*density)*100)) +
#   scale_fill_manual(values = c("#67e6dc","darkslategray4", "black")) + #c("grey70", "grey35","black"))
#   scale_color_manual(values = c("#67e6dc","darkslategray4","black")) + #c("gold", "coral","darkorchid4")) #67e6dc
#   scale_x_continuous(expand = c(0,0.05)) +
#   labs(title = "West Indies",
#        y = "Node pairs (%)\n", 
#        x = "\nNOS") +
#   theme_pubr() +
#   theme(plot.title = element_text(size = 11, colour = "black", face = "bold", hjust = 0.5),
#         axis.text = element_text(size=11, colour = "black")) 
# 
# #other option, doesnt work, Count*n is the density !
# c <- vir_NOS_full %>% group_by(threshold) %>% count(threshold)
# vir_NOS_full <- merge(vir_NOS_full, c, by = "threshold", all.x = TRUE)
# 
# ggplot(data = subset(vir_NOS_full, nos %in% "out"), mapping = aes(x = value, y = (..count..)/n, fill = threshold, color = threshold)) +
#   geom_density(alpha = 0.1, size = 1) +  scale_fill_manual(values = c("#67e6dc","darkslategray4", "black")) + #c("grey70", "grey35","black"))
#   scale_color_manual(values = c("#67e6dc","darkslategray4","black")) + #c("gold", "coral","darkorchid4")) #67e6dc
#   scale_x_continuous(expand = c(0,0.05)) +
#   labs(title = "West Indies",
#        y = "Node pairs (%)\n", 
#        x = "\nNOS") +
#   theme_pubr() +
#   theme(plot.title = element_text(size = 11, colour = "black", face = "bold", hjust = 0.5),
#         axis.text = element_text(size=11, colour = "black")) 
  

#Try plot with proportions of NOS values >< . But not great what is important is the mean NOS value and MOD(dispersion around the mean)
plyr::ddply(NOS_in_df, "site", summarise,
      pos_N = length(which(nos_in > 0.7))/length(nos_in)*100,
      neg_N = length(which(nos_in < -0.7))/length(nos_in)*100) 


nos_per <- plyr::ddply(NOS_in_df, "site", summarise,
            pos_N = length(which(nos_in == 1))/length(nos_in)*100,
            neg_N = length(which(nos_in == -1))/length(nos_in)*100) 

nos_per <- tidyr::gather(nos_per, "pos_N","neg_N", 2:3) %>%
  rename(type = pos_N, value  = neg_N)


ggplot(nos_per, aes(x=as.factor(site), y=value, fill = type)) +
  geom_bar(stat = "identity", position=position_dodge())



