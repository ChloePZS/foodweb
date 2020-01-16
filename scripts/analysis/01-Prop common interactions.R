###################################
#Proportion interactions in commun#
###################################

library(tidyverse)
library(picante)
library(RColorBrewer)
library(reshape2)
library(ggpubr)

#1. Import  final matrices
vir_ISmatrix2 <- read.csv("data/vir_ISmatrix_fam2.csv", sep=",", row.names = 1) %>%
  as.matrix(.)
mari_ISmatrix2 <- read.csv("data/mari_ISmatrix_fam2.csv", sep=",", row.names = 1) %>%
  as.matrix(.)
nca_ISmatrix2 <- read.csv("data/nca_ISmatrix_fam2.csv", sep=",", row.names = 1) %>%
  as.matrix(.)
haw_ISmatrix2 <- read.csv("data/haw_ISmatrix_fam2.csv", sep=",", row.names = 1) %>%
  as.matrix(.)
mad_ISmatrix2 <- read.csv("data/mad_ISmatrix_fam2.csv", sep=",", row.names = 1) %>%
  as.matrix(.)

#2. Transform into qualitative matrices
pa_vir <- decostand(vir_ISmatrix2, method="pa")
pa_mari <- decostand(mari_ISmatrix2, method="pa")
pa_nca <- decostand(nca_ISmatrix2, method="pa")
pa_haw <- decostand(haw_ISmatrix2, method="pa")
pa_mad <- decostand(mad_ISmatrix2, method="pa")

pa_sum <- pa_vir + pa_mari + pa_nca + pa_haw + pa_mad #get the sum of the qualitative matrices 

#####3. Sum of qualitative interactions for the one in commun####

sum(sum(pa_vir[which(pa_sum>2)], pa_mari[which(pa_sum>2)], pa_nca[which(pa_sum>2)],
        pa_haw[which(pa_sum>2)], pa_mad[which(pa_sum>2)])) / sum(sum(colSums(pa_vir)), 
                                                                 sum(colSums(pa_nca)), 
                                                                 sum(colSums(pa_haw)), 
                                                                 sum(colSums(pa_nca)), 
                                                                 sum(colSums(pa_mad))) #colSums here would be number of interactions

sum(vir_ISmatrix2[which(pa_sum>2)], mari_ISmatrix2[which(pa_sum>2)], nca_ISmatrix2[which(pa_sum>2)],
    haw_ISmatrix2[which(pa_sum>2)], mad_ISmatrix2[which(pa_sum>2)]) / sum(sum(colSums(vir_ISmatrix2)), 
                                                                          sum(colSums(nca_ISmatrix2)), 
                                                                          sum(colSums(haw_ISmatrix2)), 
                                                                          sum(colSums(mari_ISmatrix2)), 
                                                                          sum(colSums(mad_ISmatrix2))) #here colSums would be the number of families 


sum(sum(pa_vir[which(pa_sum>3)], pa_mari[which(pa_sum>3)], pa_nca[which(pa_sum>3)],
        pa_haw[which(pa_sum>3)], pa_mad[which(pa_sum>3)])) / sum(sum(colSums(pa_vir)), 
                                                                 sum(colSums(pa_nca)), 
                                                                 sum(colSums(pa_haw)), 
                                                                 sum(colSums(pa_nca)), 
                                                                 sum(colSums(pa_mad)))

sum(vir_ISmatrix2[which(pa_sum>3)], mari_ISmatrix2[which(pa_sum>3)], nca_ISmatrix2[which(pa_sum>3)],
    haw_ISmatrix2[which(pa_sum>3)], mad_ISmatrix2[which(pa_sum>3)]) / sum(sum(colSums(vir_ISmatrix2)), sum(colSums(nca_ISmatrix2)), sum(colSums(haw_ISmatrix2)), sum(colSums(mari_ISmatrix2)), sum(colSums(mad_ISmatrix2)))


sum(sum(pa_vir[which(pa_sum>4)], pa_mari[which(pa_sum>4)], pa_nca[which(pa_sum>4)],
        pa_haw[which(pa_sum>4)], pa_mad[which(pa_sum>4)])) / sum(sum(colSums(pa_vir)), 
                                                                 sum(colSums(pa_nca)), 
                                                                 sum(colSums(pa_haw)), 
                                                                 sum(colSums(pa_nca)), 
                                                                 sum(colSums(pa_mad)))

sum(mari_ISmatrix2[which(pa_sum>4)], nca_ISmatrix2[which(pa_sum>4)],
    haw_ISmatrix2[which(pa_sum>4)], mad_ISmatrix2[which(pa_sum>4)]) / sum(sum(colSums(nca_ISmatrix2)), sum(colSums(haw_ISmatrix2)), sum(colSums(mari_ISmatrix2)), sum(colSums(mad_ISmatrix2)))


#Function of the metric
#function 
chloe <- function(m1, m2, m3, m4, m5, Sum, n) {
  sum(m1[which(Sum>(n-1))],m2[which(Sum>(n-1))], m3[which(Sum>(n-1))], m4[which(Sum>(n-1))], m5[which(Sum>(n-1))]) / sum(sum(colSums(m1)), sum(colSums(m2)), sum(colSums(m3)), sum(colSums(m4)), sum(colSums(m5)))
}#eo function

prop_int <-c(chloe(vir_ISmatrix2, nca_ISmatrix2, mari_ISmatrix2, mad_ISmatrix2, haw_ISmatrix2, Sum= pa_sum, n=5),  
             chloe(vir_ISmatrix2, nca_ISmatrix2, mari_ISmatrix2, mad_ISmatrix2, haw_ISmatrix2, Sum= pa_sum, n=4)  ,
             chloe(vir_ISmatrix2, nca_ISmatrix2, mari_ISmatrix2, mad_ISmatrix2, haw_ISmatrix2, Sum= pa_sum, n=3),
             chloe(vir_ISmatrix2, nca_ISmatrix2, mari_ISmatrix2, mad_ISmatrix2, haw_ISmatrix2, Sum= pa_sum, n=2))

####4. Permutations - Randomized matrices####
### null model
#Fixed colSums, and keep original link weights

#create randomized matrices

p <- 100

rand_vir <- lapply(1:p, function(x) randomizeMatrix(vir_ISmatrix2, null.model = "frequency"))
rand_mari <- lapply(1:p, function(x) randomizeMatrix(mari_ISmatrix2, null.model = "frequency"))
rand_nca <- lapply(1:p, function(x) randomizeMatrix(nca_ISmatrix2, null.model = "frequency"))
rand_mad <- lapply(1:p, function(x) randomizeMatrix(mad_ISmatrix2, null.model = "frequency"))
rand_haw <- lapply(1:p, function(x) randomizeMatrix(haw_ISmatrix2, null.model = "frequency"))


#Fonction to get the index for each of the p permutations
nm_interactions2 <- lapply(1:p, function (z) {
  
  chloe <- function(m1, m2, m3, m4, m5, Sum, n) {
    
    sum(m1[which(Sum>(n-1))],m2[which(Sum>(n-1))], m3[which(Sum>(n-1))], m4[which(Sum>(n-1))], m5[which(Sum>(n-1))]) / 
      sum(sum(colSums(m1)), sum(colSums(m2)), sum(colSums(m3)), sum(colSums(m4)), sum(colSums(m5)))
  }#eo function
  
  chloe(rand_vir[[z]], rand_nca[[z]], rand_mad[[z]], rand_mari[[z]], rand_haw[[z]] , Sum = pa_sum, n = 2)   
  
})#eo lapply


nm_interactions2 <- do.call(rbind, nm_interactions2)

####5. Df and plots ####
#Dataframe with observed and random values
df_nm_interactions2 <- data.frame(nm_interactions2) %>% mutate(nb_int = 2) %>% dplyr::rename(value = nm_interactions2)
df_nm_interactions3 <- data.frame(nm_interactions3) %>% mutate(nb_int = 3) %>% dplyr::rename(value = nm_interactions3)
df_nm_interactions4 <- data.frame(nm_interactions4) %>% mutate(nb_int = 4) %>% dplyr::rename(value = nm_interactions4)
df_nm_interactions5 <- data.frame(nm_interactions5) %>% mutate(nb_int = 5) %>% dplyr::rename(value = nm_interactions5)

rand_int <- rbind(df_nm_interactions5, df_nm_interactions4, df_nm_interactions3, df_nm_interactions2) %>% mutate(nb_int = factor(nb_int, levels = c(2,3,4,5)))
rand_int$data <- "rand"

obs_int <- data.frame(prop_int) %>% dplyr::rename(value = prop_int) %>% mutate (nb_int = c(5,4,3,2),
                                                                     data = "obs")

df <- rbind(rand_int, obs_int)

#Plot
prop.plot <- ggplot(rand_int, aes(x=nb_int, y=value, fill=as.factor(nb_int), color=as.factor(nb_int))) +
  geom_boxplot(outlier.shape = NA) + 
  scale_fill_manual(values = RColorBrewer::brewer.pal(9, "Greys")[c(5:8)]) +
  scale_color_manual(values = RColorBrewer::brewer.pal(9, "Greys")[c(5:8)]) +
  stat_summary(geom = "crossbar", width=0.65, fatten=0, color="white", fun.data = function(x){ return(c(y=median(x), ymin=median(x), ymax=median(x))) })  +
  geom_point(data = obs_int, aes(x=as.factor(nb_int), y=value), size=4, shape=8) +
  labs( x = "\nNumber of shared interactions", y = "Proportion of interactions\n") +
  theme_classic() +
  theme(legend.position = "none", 
        panel.background = element_rect(colour = "black", size=0.5),
        axis.title = element_text(size=13),
        axis.text = element_text(size=11, colour = "black"))

#plot(rep(1,5), col=RColorBrewer::brewer.pal(9, "Greys")[5:9],pch=19,cex=2) #to check for the colors in the palette



#####6. Correlation matrice quantitative et qualitative  #####
#--> Idée que les intéractions les plus importantes sont aussi les communes

#Get the matrices into long format
#Let"s try to have a whole df to do a facet plot
#Get the matrices into long format
df_mariIS <- melt(mari_ISmatrix2)
df_pa_sum <- melt(pa_sum) %>%
  dplyr::rename(nb_int = value)
#merge 
df_mariIS2 <- left_join(df_mariIS, df_pa_sum) %>% filter(nb_int != 0) %>% mutate(site = "mari")
df_mariIS2$nb_int <- as.factor(df_mariIS2$nb_int)
df_mariIS2$nb_int <- as.numeric(df_mariIS2$nb_int)

#Get the matrices into long format
df_hawIS <- melt(haw_ISmatrix2)
df_pa_sum <- melt(pa_sum) %>%
  dplyr::rename(nb_int = value)
#merge 
df_hawIS2 <- left_join(df_hawIS, df_pa_sum) %>% filter(nb_int != 0) %>% mutate(site = "haw")
df_hawIS2$nb_int <- as.factor(df_hawIS2$nb_int)

#Get the matrices into long format
df_madIS <- melt(mad_ISmatrix2)
df_pa_sum <- melt(pa_sum) %>%
  dplyr::rename(nb_int = value)
#merge 
df_madIS2 <- left_join(df_madIS, df_pa_sum) %>% filter(nb_int != 0) %>% mutate(site =  "mad")
df_madIS2$nb_int <- as.factor(df_madIS2$nb_int)

#Get the matrices into long format
df_ncaIS <- melt(nca_ISmatrix2)
df_pa_sum <- melt(pa_sum) %>%
  dplyr::rename(nb_int = value)
#merge 
df_ncaIS2 <- left_join(df_ncaIS, df_pa_sum) %>% filter(nb_int != 0) %>% mutate(site = "nca")
df_ncaIS2$nb_int <- as.factor(df_ncaIS2$nb_int)

#Get the matrices into long format
df_virIS <- melt(vir_ISmatrix2)
df_pa_sum <- melt(pa_sum) %>%
  dplyr::rename(nb_int = value)
#merge 
df_virIS2 <- left_join(df_virIS, df_pa_sum) %>% filter(nb_int != 0) %>% mutate(site = "vir")
df_virIS2$nb_int <- as.factor(df_virIS2$nb_int)


#full data set
df_allIS <- rbind(df_hawIS2, df_madIS2, df_mariIS2, df_ncaIS2, df_virIS2) 
df_allIS$site <- as.factor(df_allIS$site)
df_allIS$nb_int <- as.factor(df_allIS$nb_int)
df_allIS <- df_allIS %>% mutate (site_name = case_when(site == "haw" ~ "Hawaii",
                                                       site == "mad" ~ "Madagascar",
                                                       site == "mari" ~ "Marshall Islands",
                                                       site == "nca"~ "New Caledonia",
                                                       site == "vir" ~ "West Indies"))


#df_allIS %>% filter(value > 0.5) %>% group_by(site) %>% summarize(mean(as.numeric(nb_int))) #mean number of shared interactions for very strong IS > 0.5

                          

                                 
#Plot with mean_se or mean_IC/ mean_se already existed --> Good final boxplot ! 
df_allIS$nb_int <- as.factor(df_allIS$nb_int)

IS.plot <- ggplot(data=df_allIS, aes(x = nb_int, y=value, color = nb_int)) + 
stat_summary(fun.y = mean, geom="point",size=2) + 
stat_summary(fun.data = mean_se, geom ="errorbar", size=0.3, width=0.3) +
stat_summary(fun.y=mean, geom="line", color="black", size=0.05, aes(group=1))+
scale_color_manual(values = RColorBrewer::brewer.pal(9, "Greys")[4:8]) +
labs(x = "\nNumber of shared interactions", y="Interaction strength\n") + 
facet_wrap(. ~ site_name)+
theme(strip.text.x = element_text(size=9, face="bold"),
      strip.background = element_rect(fill = 'white'),
      panel.background = element_rect(fill = 'white'),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line.x = element_line(colour = "black",size=0.5, lineend = "butt"),
      axis.line.y = element_line(colour = "black", size=0.5),
      axis.title = element_text(size=13),
      axis.text = element_text(size=10, colour = "black")) +
guides(color=FALSE)

#Function to pslot with IC
min.mean.IC.max <- function(x) {
  r <- c(mean(x)-1.96*sd(x)/sqrt(length(x)), mean(x)-1.96*sd(x)/sqrt(length(x)), mean(x), mean(x) +1.96*sd(x)/sqrt(length(x)), mean(x)+1.96*sd(x)/sqrt(length(x)))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}


mean_IC <- function(x){
  r <- c(mean(x), mean(x)-1.96*sd(x)/sqrt(length(x)), mean(x) +1.96*sd(x)/sqrt(length(x)))
  names(r) <- c("y","ymin","ymax")
  r
}



#Combine the two plots
ggarrange(prop.plot, IS.plot, 
          labels = c("A","B"),
          font.label = list(size = 12, face = "bold"),
          ncol = 2, nrow = 1,
          widths = c(1.5,2))




#####GEt the interactions shared to 5 and 4 sites ####
write.csv(df_pa_sum %>% filter(nb_int ==5 | nb_int ==4), "shared interactions.csv")



chloe2 <- function(m1, m2, m3, m4, m5, Sum, n) {
  sum(m1[which(Sum>(n-1))],m2[which(Sum>(n-1))], m3[which(Sum>(n-1))], m4[which(Sum>(n-1))], m5[which(Sum>(n-1))]) / length(which(colMaxs(Sum) == n)) * length(which(rowMaxs(Sum) == n))
}#eo function



##Other try
fam_nb_int <- setNames(melt(pa_sum),c("prey","family","nb_int"))
fam_nb_int_mari <- setNames(melt(pa_mari),c("prey","family","mari"))
fam_nb_int_nca <- setNames(melt(pa_nca),c("prey","family","nca"))
fam_nb_int_mad <- setNames(melt(pa_mad),c("prey","family","mad"))
fam_nb_int_vir <- setNames(melt(pa_vir),c("prey","family","vir"))
fam_nb_int_haw <- setNames(melt(pa_haw),c("prey","family","haw"))

fam_nb_int <- fam_nb_int %>% left_join(fam_nb_int_haw) %>%
              left_join(fam_nb_int_mari) %>%
              left_join(fam_nb_int_nca) %>%
              left_join(fam_nb_int_mad) %>% 
              left_join(fam_nb_int_vir)
