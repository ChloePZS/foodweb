####################################
#Analysis on shared interactions#
####################################

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
jap_ISmatrix2 <- read.csv("data/jap_ISmatrix_fam2.csv", sep=",", row.names = 1) %>%
  as.matrix(.)

  #2. Transform into qualitative matrices
pa_vir <- decostand(vir_ISmatrix2, method="pa")
pa_mari <- decostand(mari_ISmatrix2, method="pa")
pa_nca <- decostand(nca_ISmatrix2, method="pa")
pa_haw <- decostand(haw_ISmatrix2, method="pa")
pa_mad <- decostand(mad_ISmatrix2, method="pa")
pa_jap <- decostand(jap_ISmatrix2, method="pa")

pa_sum <- pa_jap +  pa_vir + pa_mari + pa_nca + pa_haw + pa_mad  #get the sum of the qualitative matrices 

  #3. Calculation of the index for the proportions of shared interactions
#Function of the metric
chloe <- function(m1, m2, m3, m4, m5, m6, Sum, n) {
  sum(m1[which(Sum>(n-1))],m2[which(Sum>(n-1))], m3[which(Sum>(n-1))], m4[which(Sum>(n-1))], m5[which(Sum>(n-1))], m6[which(Sum>(n-1))]) / 
    sum(sum(colSums(m1)), sum(colSums(m2)), sum(colSums(m3)), sum(colSums(m4)), sum(colSums(m5)), sum(colSums(m6)))
}

#sum of 
prop_int <-c(chloe(vir_ISmatrix2, nca_ISmatrix2, mari_ISmatrix2, mad_ISmatrix2, haw_ISmatrix2, jap_ISmatrix2, Sum= pa_sum, n=6),
             chloe(vir_ISmatrix2, nca_ISmatrix2, mari_ISmatrix2, mad_ISmatrix2, haw_ISmatrix2, jap_ISmatrix2, Sum= pa_sum, n=5),  
             chloe(vir_ISmatrix2, nca_ISmatrix2, mari_ISmatrix2, mad_ISmatrix2, haw_ISmatrix2,jap_ISmatrix2, Sum= pa_sum, n=4)  ,
             chloe(vir_ISmatrix2, nca_ISmatrix2, mari_ISmatrix2, mad_ISmatrix2, haw_ISmatrix2, jap_ISmatrix2, Sum= pa_sum, n=3),
             chloe(vir_ISmatrix2, nca_ISmatrix2, mari_ISmatrix2, mad_ISmatrix2, haw_ISmatrix2,jap_ISmatrix2, Sum= pa_sum, n=2))

  #4. Permutations ####
#Null model with fixed colSums, and keeping original link weights

#create randomized matrices
p <- 100

rand_vir <- lapply(1:p, function(x) randomizeMatrix(vir_ISmatrix2, null.model = "frequency"))
rand_mari <- lapply(1:p, function(x) randomizeMatrix(mari_ISmatrix2, null.model = "frequency"))
rand_nca <- lapply(1:p, function(x) randomizeMatrix(nca_ISmatrix2, null.model = "frequency"))
rand_mad <- lapply(1:p, function(x) randomizeMatrix(mad_ISmatrix2, null.model = "frequency"))
rand_haw <- lapply(1:p, function(x) randomizeMatrix(haw_ISmatrix2, null.model = "frequency"))
rand_jap <- lapply(1:p, function(x) randomizeMatrix(jap_ISmatrix2, null.model = "frequency"))

#Fonction to get the index for each of the p permutations and for n number of networks with shared interactions (from 2 to 5)
nm_interactions6 <- lapply(1:p, function (z) {
  
  chloe <- function(m1, m2, m3, m4, m5, m6, Sum, n) {
    
    sum(m1[which(Sum>(n-1))],m2[which(Sum>(n-1))], m3[which(Sum>(n-1))], m4[which(Sum>(n-1))], m5[which(Sum>(n-1))], m6[which(Sum>(n-1))]) / 
      sum(sum(colSums(m1)), sum(colSums(m2)), sum(colSums(m3)), sum(colSums(m4)), sum(colSums(m5)), sum(colSums(m6)))
  }#eo function
  
  chloe(rand_vir[[z]], rand_nca[[z]], rand_mad[[z]], rand_mari[[z]], rand_haw[[z]] , rand_jap[[z]], Sum = pa_sum, n = 6)   
  
})#eo lapply


nm_interactions6 <- do.call(rbind, nm_interactions6)

  #5. Df and plot ####
#Dataframe with observed and random values
df_nm_interactions2 <- data.frame(nm_interactions2) %>% mutate(nb_int = 2) %>% dplyr::rename(value = nm_interactions2)
df_nm_interactions3 <- data.frame(nm_interactions3) %>% mutate(nb_int = 3) %>% dplyr::rename(value = nm_interactions3)
df_nm_interactions4 <- data.frame(nm_interactions4) %>% mutate(nb_int = 4) %>% dplyr::rename(value = nm_interactions4)
df_nm_interactions5 <- data.frame(nm_interactions5) %>% mutate(nb_int = 5) %>% dplyr::rename(value = nm_interactions5)
df_nm_interactions6 <- data.frame(nm_interactions6) %>% mutate(nb_int = 6) %>% dplyr::rename(value = nm_interactions6)


rand_int <- rbind(df_nm_interactions6, df_nm_interactions5, df_nm_interactions4, df_nm_interactions3, df_nm_interactions2) %>% mutate(nb_int = factor(nb_int, levels = c(2,3,4,5,6)))
rand_int$data <- "rand"

obs_int <- data.frame(prop_int) %>% dplyr::rename(value = prop_int) %>% mutate (nb_int = c(6,5,4,3,2),
                                                                                data = "obs")
#df <- rbind(rand_int, obs_int)

#Z scores
Z3 <- obs_int$value[obs_int$nb_int == 3] - mean(rand_int$value[rand_int$nb_int == 3]) / sd(rand_int$value[rand_int$nb_int == 3])
Z6 <- obs_int$value[obs_int$nb_int == 6] - mean(rand_int$value[rand_int$nb_int == 6]) / sd(rand_int$value[rand_int$nb_int == 6])


#Plot of proportions of shared interactions
prop.plot <- ggplot(rand_int, aes(x=nb_int, y=value)) +
  stat_summary(fun.y = mean, geom="point",size=1) + 
  stat_summary(fun.data  = quantiles_95,
               geom="errorbar", size=0.3, width=0.08, color = "black") +
  stat_summary(fun.y=mean, geom="line", color="black", size=0.2, aes(group=1, linetype = "Null distributions"))+
  geom_point(data = obs_int, aes(x=as.factor(nb_int), y=value), size=1, shape=19, color = "black") +
  geom_line(data = obs_int, aes(x=as.factor(nb_int), y=value, group=1,linetype ="Observed values"), size=0.2, color = "black") +
  scale_linetype_manual(name = "", values = c("Observed values" = "solid", "Null distributions" = "dashed"))+
  labs( x = "Number of food webs", y = "Proportion of shared interactions") +
  theme_classic() +
  theme(legend.position = c(0.7, 0.8),
        legend.box = NULL,
        legend.text = element_text(colour="black", size=6),
        panel.background = element_rect(colour = "black", size=0.1, fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=7),
        axis.text = element_text(size=6, colour = "black"),
        axis.line = element_line(size=0.1, colour = "black"),
        axis.ticks = element_line(colour = "black", size = 0.1)) 



  #6. Correlation between quantitative and qualitative matrices  #####
#Marshall Islands
#Get the matrices into long format
df_mariIS <- melt(mari_ISmatrix2)
df_pa_sum <- melt(pa_sum) %>%
  dplyr::rename(nb_int = value)
#merge 
df_mariIS2 <- left_join(df_mariIS, df_pa_sum) %>% filter(nb_int != 0) %>% mutate(site = "mari")
df_mariIS2$nb_int <- as.factor(df_mariIS2$nb_int)
df_mariIS2$nb_int <- as.numeric(df_mariIS2$nb_int)

#Hawaii
df_hawIS <- melt(haw_ISmatrix2)
df_pa_sum <- melt(pa_sum) %>%
  dplyr::rename(nb_int = value)
df_hawIS2 <- left_join(df_hawIS, df_pa_sum) %>% filter(nb_int != 0) %>% mutate(site = "haw")
df_hawIS2$nb_int <- as.factor(df_hawIS2$nb_int)

#Madagascar
df_madIS <- melt(mad_ISmatrix2)
df_pa_sum <- melt(pa_sum) %>%
  dplyr::rename(nb_int = value)
df_madIS2 <- left_join(df_madIS, df_pa_sum) %>% filter(nb_int != 0) %>% mutate(site =  "mad")
df_madIS2$nb_int <- as.factor(df_madIS2$nb_int)

#New Caledonia
df_ncaIS <- melt(nca_ISmatrix2)
df_pa_sum <- melt(pa_sum) %>%
  dplyr::rename(nb_int = value)
df_ncaIS2 <- left_join(df_ncaIS, df_pa_sum) %>% filter(nb_int != 0) %>% mutate(site = "nca")
df_ncaIS2$nb_int <- as.factor(df_ncaIS2$nb_int)

#West Indies
df_virIS <- melt(vir_ISmatrix2)
df_pa_sum <- melt(pa_sum) %>%
  dplyr::rename(nb_int = value)
df_virIS2 <- left_join(df_virIS, df_pa_sum) %>% filter(nb_int != 0) %>% mutate(site = "vir")
df_virIS2$nb_int <- as.factor(df_virIS2$nb_int)

#Japan
df_japIS <- melt(jap_ISmatrix2)
df_pa_sum <- melt(pa_sum) %>%
  dplyr::rename(nb_int = value)
df_japIS2 <- left_join(df_japIS, df_pa_sum) %>% filter(nb_int != 0) %>% mutate(site = "jap")
df_japIS2$nb_int <- as.factor(df_japIS2$nb_int)

#full data set
df_allIS <- rbind(df_hawIS2, df_madIS2, df_mariIS2, df_ncaIS2, df_virIS2, df_japIS2) 
df_allIS$site <- as.factor(df_allIS$site)
df_allIS$nb_int <- as.factor(df_allIS$nb_int)
df_allIS <- df_allIS %>% mutate (site_name = case_when(site == "haw" ~ "Hawaii",
                                                       site == "mad" ~ "Madagascar",
                                                       site == "mari" ~ "Marshall Islands",
                                                       site == "nca"~ "New Caledonia",
                                                       site == "vir" ~ "West Indies",
                                                       site == "jap" ~ "Okinawa"))



#Plot with mean_se  
df_allIS$nb_int <- as.factor(df_allIS$nb_int)

IS.plot <- ggplot(data=df_allIS, aes(x = nb_int, y=value)) + 
  stat_summary(fun.y = mean, geom="point",size=1, color = "black") + 
  stat_summary(fun.data = mean_se, geom ="errorbar", size=0.2, width=0.3) +
  stat_summary(fun.y=mean, geom="line", color="black", size=0.2, aes(group=1))+
  labs(x = "Number of food webs", y="Strength of shared interactions") + 
  facet_wrap(. ~ site_name)+
  theme(strip.text.x = element_text(size=6, face="bold"),
        strip.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(colour = "black",size=0.1, lineend = "butt"),
        axis.line.y = element_line(colour = "black", size=0.1),
        axis.ticks = element_line(colour = "black", size = 0.1),
        axis.title = element_text(size=7),
        axis.text = element_text(size=6, colour = "black")) 


  #7. Combine the two plots and save
shared.int.plot <- ggarrange(prop.plot, IS.plot, 
          labels = c("A","B"),
          font.label = list(size = 8, face = "bold"),
          ncol = 2, nrow = 1,
          widths = c(1.5,2))
ggsave("output/figures/fig.2 - shared interactions plot.pdf", plot = shared.int.plot, width = 17.8, height = 7.5, units = "cm", dpi = 300)

