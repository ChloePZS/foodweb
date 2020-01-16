###################################
#Proportion interactions in commun#
###################################

library(tidyverse)
library(vegan)
library(picante)
library(RColorBrewer)
library(wesanderson)
library(reshape2)

#1. Import  final matrices
vir_ISmatrix2 <- read.csv("data/vir_ISmatrix2.csv", sep=",", row.names = 1) %>%
  as.matrix(.)
mari_ISmatrix2 <- read.csv("data/mari_ISmatrix2.csv", sep=",", row.names = 1) %>%
  as.matrix(.)
nca_ISmatrix2 <- read.csv("data/nca_ISmatrix2.csv", sep=",", row.names = 1) %>%
  as.matrix(.)
haw_ISmatrix2 <- read.csv("data/haw_ISmatrix2.csv", sep=",", row.names = 1) %>%
  as.matrix(.)
mad_ISmatrix2 <- read.csv("data/mad_ISmatrix2.csv", sep=",", row.names = 1) %>%
  as.matrix(.)

#2. Transform into qualitative matrices
pa_vir <- decostand(vir_ISmatrix2, method="pa")
pa_mari <- decostand(mari_ISmatrix2, method="pa")
pa_nca <- decostand(nca_ISmatrix2, method="pa")
pa_haw <- decostand(haw_ISmatrix2, method="pa")
pa_mad <- decostand(mad_ISmatrix2, method="pa")

pa_sum <- pa_vir + pa_mari + pa_nca + pa_haw + pa_mad


#3. Sum of qualitative interactions for the one in commun

sum(vir_ISmatrix2[which(pa_sum>3)], mari_ISmatrix2[which(pa_sum>3)], nca_ISmatrix2[which(pa_sum>3)],
    haw_ISmatrix2[which(pa_sum>3)], mad_ISmatrix2[which(pa_sum>3)]) / sum(sum(colSums(vir_ISmatrix2)), sum(colSums(nca_ISmatrix2)), sum(colSums(haw_ISmatrix2)), sum(colSums(mari_ISmatrix2)), sum(colSums(mad_ISmatrix2)))


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

#4. Permutations - Randomized matrices
### null model

#create randomized matrices

p <- 10000

rand_vir <- lapply(1:p, function(x) randomizeMatrix(vir_ISmatrix2, null.model = "richness"))
rand_mari <- lapply(1:p, function(x) randomizeMatrix(mari_ISmatrix2, null.model = "richness"))
rand_nca <- lapply(1:p, function(x) randomizeMatrix(nca_ISmatrix2, null.model = "richness"))
rand_mad <- lapply(1:p, function(x) randomizeMatrix(mad_ISmatrix2, null.model = "richness"))
rand_haw <- lapply(1:p, function(x) randomizeMatrix(haw_ISmatrix2, null.model = "richness"))


library(parallel)


#Fonction to get the index for each of the p permutations
nm_interactions2 <- lapply(1:p, function (z) {
  
  chloe <- function(m1, m2, m3, m4, m5, Sum, n) {
    
    sum(m1[which(Sum>(n-1))],m2[which(Sum>(n-1))], m3[which(Sum>(n-1))], m4[which(Sum>(n-1))], m5[which(Sum>(n-1))]) / sum(sum(colSums(m1)), sum(colSums(m2)), sum(colSums(m3)), sum(colSums(m4)), sum(colSums(m5)))
  }#eo function
  
  chloe(rand_vir[[z]], rand_nca[[z]], rand_mad[[z]], rand_mari[[z]], rand_haw[[z]] , Sum= pa_sum, n=2)   
  
})#eo lapply


nm_interactions2 <- do.call(rbind, nm_interactions2)

boxplot(nm_interactions5)
mean(nm_interactions5)
hist(nm_interactions5)

#get IC5 and IC95

IC95_rand <- c(mean(nm_interactions5) + 1.96*sd(nm_interactions5)/sqrt(p),
               mean(nm_interactions4) + 1.96*sd(nm_interactions4)/sqrt(p),
               mean(nm_interactions3) + 1.96*sd(nm_interactions3)/sqrt(p),
               mean(nm_interactions2) + 1.96*sd(nm_interactions2)/sqrt(p))

IC5_rand <- c(mean(nm_interactions5) - 1.96*sd(nm_interactions5)/sqrt(p),
              mean(nm_interactions4) - 1.96*sd(nm_interactions4)/sqrt(p),
              mean(nm_interactions3) - 1.96*sd(nm_interactions3)/sqrt(p),
              mean(nm_interactions2) - 1.96*sd(nm_interactions2)/sqrt(p))

ci_rand <- c(1.96*sd(nm_interactions5)/sqrt(p),1.96*sd(nm_interactions4)/sqrt(p),1.96*sd(nm_interactions3)/sqrt(p),1.96*sd(nm_interactions2)/sqrt(p))

se_rand <- c(sd(nm_interactions5)/sqrt(p),sd(nm_interactions4)/sqrt(p),sd(nm_interactions3)/sqrt(p),sd(nm_interactions2)/sqrt(p))

sd_rand <- c(sd(nm_interactions5),sd(nm_interactions4), sd(nm_interactions3), sd(nm_interactions2))

#mean(nm_interactions5) + 1.96*sd(nm_interactions5)/sqrt(p) #IC95
#mean(nm_interactions5) - 1.96*sd(nm_interactions5)/sqrt(p) #IC5

#Create a data frame for final boxplot figure
nb_int <- c(5,4,3,2,5,4,3,2)
data <- rep(c("rand","true"), each=4)
value <- c(mean(nm_interactions5), mean(nm_interactions4),mean(nm_interactions3),mean(nm_interactions2), prop_int)
IC95 <- c(IC95_rand, NA,NA,NA,NA)
IC5 <- c(IC5_rand, NA,NA,NA,NA)
se <- c(se_rand, NA,NA,NA,NA)
sd <- c(sd_rand, NA,NA,NA,NA)
ci <- c(ci_rand, NA,NA,NA,NA)

df_prop_int <- data.frame(nb_int, data, value, se, sd, ci, IC95, IC5,
                          stringsAsFactors = FALSE)
df_prop_int$nb_int <- as.factor(df_prop_int$nb_int)

df_prop_true <- df_prop_int %>% filter(data=="true") %>% select(value, nb_int, data)

df_prop_rand <- df_prop_int %>% filter(data=="rand")
glimpse(df_prop_rand)
df_prop_rand$nb_int <- as.factor(df_prop_rand$nb_int)

#other try df
rand_int$data <- "rand"

df_nm_interactions2 <- data.frame(nm_interactions2) %>% mutate(nb_int = 2) %>% rename(value = nm_interactions2)
df_nm_interactions3 <- data.frame(nm_interactions3) %>% mutate(nb_int = 3) %>% rename(value = nm_interactions3)
df_nm_interactions4 <- data.frame(nm_interactions4) %>% mutate(nb_int = 4) %>% rename(value = nm_interactions4)
df_nm_interactions5 <- data.frame(nm_interactions5) %>% mutate(nb_int = 5) %>% rename(value = nm_interactions5)

rand_int <- rbind(df_nm_interactions5, df_nm_interactions4, df_nm_interactions3, df_nm_interactions2) %>% mutate(nb_int = factor(nb_int, levels = c(2,3,4,5)))

df <- rbind(rand_int, df_prop_true)

#boxplot

ggplot(df, aes(x = nb_int, y=value))+
  geom_point(mapping = aes(x = nb_int, y=value, group = data == "true"), shape = 8) 
+
  geom_boxplot(mapping = aes(x = nb_int, y=value, group = data=="rand", fill=nb_int), outlier.shape = NA) + 
  scale_fill_manual(values=wes_palette(n=4, name="Darjeeling2")) + 
  theme_classic()


#Good plots
ggplot(rand_int, aes(x=nb_int, y=value, fill=nb_int)) +
  geom_boxplot(outlier.shape = NA) + 
  scale_fill_viridis_d(option ="D") +
  stat_summary(fun.y = mean, geom = "point", mapping = aes(x=nb_int, y=value), color="black", size = 2.5, shape=19) +
  geom_point(data = df_prop_true, aes(x=nb_int, y=value), size=3.5, shape=8)+
  labs( x = "Number of shared interactions", y = "Proportion of interactions") +
  theme_classic() +
 theme(legend.position = "none") 

ggplot(rand_int, aes(x=nb_int, y=value, fill=nb_int)) +
  geom_boxplot(outlier.shape = NA) + 
  scale_fill_manual(values=wes_palette(n=4, name="Darjeeling2")) +
  stat_summary(fun.y = mean, geom = "point", mapping = aes(x=nb_int, y=value), color="black", size = 2.5, shape=19) +
  geom_point(data = df_prop_true, aes(x=nb_int, y=value), size=3.5, shape=8)+
  labs( x = "Shared interactions", y = "Proportion of interactions") +
  theme_classic() +
  theme(legend.position = "none") 


ggplot(rand_int, aes(x=nb_int, y=value)) +
  geom_boxplot(outlier.shape = NA, fill="gray") +
  stat_summary(fun.y = mean, geom = "point", mapping = aes(x=nb_int, y=value, color=nb_int), size = 2.5) +
  scale_color_manual(values=wes_palette(n=4, name="Darjeeling2")) +
  geom_point(data = df_prop_true, aes(x=nb_int, y=value, color=nb_int), size=3, shape=8) +
  scale_color_manual(values=wes_palette(n=4, name="Darjeeling2")) +
  theme_classic() +
  labs( x = "Number of sites", y = "Proportion of interactions") + 
  theme(legend.position="none")


#Nina's try 
ggplot(df_prop_rand, aes(x=nb_int, y=value)) +
  geom_point() +
  geom_errorbar(aes(ymin=(IC5), ymax=(IC95), x=nb_int, y=(value)), width=.1, data = df_prop_rand) +
  facet_wrap(~nb_int, scales = "free") +
  geom_point(data = df_prop_int, aes(x=nb_int, y=value))



#6. Correlation matrice quantitative et qualitative --> Idée que les intéractions les plus importantes sont aussi les communes

boxplot(vir_ISmatrix2 ~ pa_sum, outline = F)
boxplot(nca_ISmatrix2 ~ pa_sum, outline = F)
boxplot(mad_ISmatrix2 ~ pa_sum, outline = F)
boxplot(haw_ISmatrix2 ~ pa_sum, outline = F)
boxplot(mari_ISmatrix2 ~ pa_sum, outline = F)

#Get the matrices into long format
df_virIS <- melt(vir_ISmatrix2)
df_pa_sum <- melt(pa_sum) %>%
  rename(nb_int = value)
#merge 
df_virIS2 <- left_join(df_virIS, df_pa_sum) %>% filter(nb_int != 0) %>% mutate(site = "vir")
df_virIS2$nb_int <- as.factor(df_virIS2$nb_int)

#Boxplot
ggplot(df_virIS2, aes(x = nb_int, y=value, fill=nb_int))+
  geom_boxplot(outlier.shape = NA) +
  scale_fill_brewer(palette = "PuBuGn") +
  labs(x = "Number of shared interactions", y="Interaction strength") +
  guides(fill=FALSE) +
  ggtitle ("a) Virgin Islands") +
  theme_classic()



#Let"s try to have a whole df to do a facet plot
#Get the matrices into long format
df_mariIS <- melt(mari_ISmatrix2)
df_pa_sum <- melt(pa_sum) %>%
  rename(nb_int = value)
#merge 
df_mariIS2 <- left_join(df_mariIS, df_pa_sum) %>% filter(nb_int != 0) %>% mutate(site = "mari")
df_mariIS2$nb_int <- as.factor(df_mariIS2$nb_int)

#Get the matrices into long format
df_hawIS <- melt(haw_ISmatrix2)
df_pa_sum <- melt(pa_sum) %>%
  rename(nb_int = value)
#merge 
df_hawIS2 <- left_join(df_hawIS, df_pa_sum) %>% filter(nb_int != 0) %>% mutate(site = "haw")
df_hawIS2$nb_int <- as.factor(df_hawIS2$nb_int)

#Get the matrices into long format
df_madIS <- melt(mad_ISmatrix2)
df_pa_sum <- melt(pa_sum) %>%
  rename(nb_int = value)
#merge 
df_madIS2 <- left_join(df_madIS, df_pa_sum) %>% filter(nb_int != 0) %>% mutate(site = "mad")
df_madIS2$nb_int <- as.factor(df_madIS2$nb_int)

#Get the matrices into long format
df_ncaIS <- melt(nca_ISmatrix2)
df_pa_sum <- melt(pa_sum) %>%
  rename(nb_int = value)
#merge 
df_ncaIS2 <- left_join(df_ncaIS, df_pa_sum) %>% filter(nb_int != 0) %>% mutate(site = "nca")
df_ncaIS2$nb_int <- as.factor(df_ncaIS2$nb_int)

#full data set
df_allIS <- rbind(df_hawIS2, df_madIS2, df_mariIS2, df_ncaIS2, df_virIS2) 
df_allIS$site <- as.factor(df_allIS$site)
df_allIS$nb_int <- as.factor(df_allIS$nb_int)
df_allIS <- df_allIS %>% mutate (site_name = case_when(site == "haw" ~ "Hawaï",
                                                       site == "mad" ~ "Madagascar",
                                                       site == "mari" ~ "Marshall Islands",
                                                       site == "nca"~ "New Caledonia",
                                                       site == "vir" ~ "West Indies")

#boxplot
ggplot(df_allIS, aes(x = nb_int, y=value, fill = nb_int)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun.y=mean, geom="line", aes(group=1)) + 
  stat_summary(fun.y=mean, geom="point", size = 0.8) +
  scale_fill_brewer(palette = "PuBuGn") +
  labs(x = "Number of shared interactions", y="Interaction strength") + 
  facet_wrap(. ~ site_name)+
  theme(strip.text.x = element_text(size=10, face="bold"),
        panel.background = element_blank())+
  guides(fill=FALSE)


#Plot with IC
min.mean.IC.max <- function(x) {
  r <- c(mean(x)-1.96*sd(x)/sqrt(length(x)), mean(x)-1.96*sd(x)/sqrt(length(x)), mean(x), mean(x) +1.96*sd(x)/sqrt(length(x)), mean(x)+1.96*sd(x)/sqrt(length(x)))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

ggplot(df_allIS, aes(x = nb_int, y=value, fill = nb_int)) +
  stat_summary(fun.data = min.mean.IC.max, geom="boxplot") + 
  scale_fill_brewer(palette = "PuBuGn") +
  labs(x = "Number of shared interactions", y="Interaction strength") + 
  facet_wrap(. ~ site_name)+
  theme(strip.text.x = element_text(size=10, face="bold"),
        panel.background = element_blank())+
  guides(fill=FALSE)


#Plot with mean_se or mean_IC/ mean_se already existed 
ggplot(df_allIS, aes(x = nb_int, y=value, color = nb_int)) +
  stat_summary(fun.y = mean, geom="point",size=2) + 
  stat_summary(fun.data = mean_se, geom ="errorbar", size=0.4) +
  stat_summary(fun.y=mean, geom="line", color="black", size=0.1, aes(group=1))+
  scale_color_viridis_d(option ="D") +
  labs(x = "Number of shared interactions", y="Interaction strength") + 
  facet_wrap(. ~ site_name)+
  theme(strip.text.x = element_text(size=10, face="bold"),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(colour = "black", 
                             size=0.5, 
                             lineend = "butt"),
        axis.line.y = element_line(colour = "black", 
                           size=0.5)) +
  guides(color=FALSE)

?mean_se

mean_IC <- function(x){
  r <- c(mean(x), mean(x)-1.96*sd(x)/sqrt(length(x)), mean(x) +1.96*sd(x)/sqrt(length(x)))
  names(r) <- c("y","ymin","ymax")
  r
}


#min.mean.q95.max <- function(x) {
  r <- c(quantile(x, probs=0.25)-1.5*(quantile(x, probs = 0.75)-quantile(x, probs = 0.25)), mean(x)-1.96*sd(x)/sqrt(length(x)), mean(x), mean(x) +1.96*sd(x)/sqrt(length(x)), quantile(x, probs=0.75)+1.5*(quantile(x, probs = 0.75)-quantile(x, probs = 0.25)))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

