####################################
#Analysis on shared interactions#
####################################

library(tidyverse)
library(picante)
library(RColorBrewer)
library(reshape2)
library(ggpubr)

a<-read.csv('data/foodweb_final.csv',header = T)
sites<-unique(a$site)
a <- a %>% select(., site, cons_class, cons_fam, cons_sp, cons_size, lev1, lev2, w) %>% unique()
#a<-aggregate(w~site+cons_class+cons_fam+cons_sp+cons_size+lev1+lev2, a, sum)

quantiles_95 <- function(x) {
  r <- c(mean(x), quantile(x, probs=c(0.05)),quantile(x, probs=c(0.95)))
  names(r) <- c("y","ymin","ymax")
  r
}


null_mod<-function(x){      #x is the dataset for a given locality, i.e. see b above; function returns a randomized version of b, that you can then use in all the analyses
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

# #to generate a null dataset:
# null_b<-null_mod(b)
get_mat<-function(l,fun='mean',bin='F'){
  if (fun=='mean'){
    l_mat<-dcast(l,lev2~cons_fam,value.var="w",fun.aggregate = mean,fill=0,drop=F)} 
  else {
    l_mat<-dcast(l,lev2~cons_fam,value.var="w",fun.aggregate = sum, fill=0,drop=F)}
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

###generate matrix of the same size for comparison (i.e. each matrix includes all consumers and prey across all sites)
b<-a
b$w<-b$w*(b$site=='vir')
vir_ISmatrix2 <- get_mat(b,fun='sum')
b<-a
b$w<-b$w*(b$site=='mari')
mari_ISmatrix2 <- get_mat(b)
b<-a
b$w<-b$w*(b$site=='nca')
nca_ISmatrix2 <-  get_mat(b)
b<-a
b$w<-b$w*(b$site=='haw')
haw_ISmatrix2 <-  get_mat(b)
b<-a
b$w<-b$w*(b$site=='mad')
mad_ISmatrix2 <-  get_mat(b)
b<-a
b$w<-b$w*(b$site=='jap')
jap_ISmatrix2 <-  get_mat(b)

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
#new null model

#create randomized matrices; also for the previous code, it would be much better to have a single
#list indexed by locality including the observed and the null matrices
rand_vir <- list()
rand_mari <- list()
rand_nca <- list()
rand_mad <- list()
rand_haw <- list()
rand_jap <- list()
for (i in 1:100){
  b<-a
  nl<-null_mod(b[b$site=='vir',])
  b[b$site=='vir',]<-nl
  b$w<-b$w*(b$site=='vir')
  rand_vir[[i]]<-get_mat(b,fun='sum')

  b<-a
  nl<-null_mod(b[b$site=='mari',])
  b[b$site=='mari',]<-nl
  b$w<-b$w*(b$site=='mari')
  rand_mari[[i]]<-get_mat(b)
  
  b<-a
  nl<-null_mod(b[b$site=='nca',])
  b[b$site=='nca',]<-nl
  b$w<-b$w*(b$site=='nca')
  rand_nca[[i]]<-get_mat(b)
  
  b<-a
  nl<-null_mod(b[b$site=='mari',])
  b[b$site=='mari',]<-nl
  b$w<-b$w*(b$site=='mari')
  rand_mari[[i]]<-get_mat(b)
  
  b<-a
  nl<-null_mod(b[b$site=='mad',])
  b[b$site=='mad',]<-nl
  b$w<-b$w*(b$site=='mad')
  rand_mad[[i]]<-get_mat(b)
  
  b<-a
  nl<-null_mod(b[b$site=='haw',])
  b[b$site=='haw',]<-nl
  b$w<-b$w*(b$site=='haw')
  rand_haw[[i]]<-get_mat(b)

  b<-a
  nl<-null_mod(b[b$site=='jap',])
  b[b$site=='jap',]<-nl
  b$w<-b$w*(b$site=='jap')
  rand_jap[[i]]<-get_mat(b)
 
  print (i) }


p <- 100

#Function to get the index for each of the p permutations and for n number of networks with shared interactions (from 2 to 5)
nm_interactions2 <- lapply(1:p, function (z) {
  chloe <- function(m1, m2, m3, m4, m5, m6, Sum, n) {
    sum(m1[which(Sum>(n-1))],m2[which(Sum>(n-1))], m3[which(Sum>(n-1))], m4[which(Sum>(n-1))], m5[which(Sum>(n-1))], m6[which(Sum>(n-1))]) / 
      sum(sum(colSums(m1)), sum(colSums(m2)), sum(colSums(m3)), sum(colSums(m4)), sum(colSums(m5)), sum(colSums(m6)))
  }#eo function
  chloe(rand_vir[[z]], rand_nca[[z]], rand_mad[[z]], rand_mari[[z]], rand_haw[[z]] , rand_jap[[z]], Sum = pa_sum, n = 2)   
})#eo lapply

nm_interactions3 <- lapply(1:p, function (z) {
  chloe <- function(m1, m2, m3, m4, m5, m6, Sum, n) {
    sum(m1[which(Sum>(n-1))],m2[which(Sum>(n-1))], m3[which(Sum>(n-1))], m4[which(Sum>(n-1))], m5[which(Sum>(n-1))], m6[which(Sum>(n-1))]) / 
      sum(sum(colSums(m1)), sum(colSums(m2)), sum(colSums(m3)), sum(colSums(m4)), sum(colSums(m5)), sum(colSums(m6)))
  }#eo function
  chloe(rand_vir[[z]], rand_nca[[z]], rand_mad[[z]], rand_mari[[z]], rand_haw[[z]] , rand_jap[[z]], Sum = pa_sum, n = 3)   
})#eo lapply

nm_interactions4 <- lapply(1:p, function (z) {
  chloe <- function(m1, m2, m3, m4, m5, m6, Sum, n) {
    sum(m1[which(Sum>(n-1))],m2[which(Sum>(n-1))], m3[which(Sum>(n-1))], m4[which(Sum>(n-1))], m5[which(Sum>(n-1))], m6[which(Sum>(n-1))]) / 
      sum(sum(colSums(m1)), sum(colSums(m2)), sum(colSums(m3)), sum(colSums(m4)), sum(colSums(m5)), sum(colSums(m6)))
  }#eo function
  chloe(rand_vir[[z]], rand_nca[[z]], rand_mad[[z]], rand_mari[[z]], rand_haw[[z]] , rand_jap[[z]], Sum = pa_sum, n = 4)   
})#eo lapply

nm_interactions5 <- lapply(1:p, function (z) {
  chloe <- function(m1, m2, m3, m4, m5, m6, Sum, n) {
    sum(m1[which(Sum>(n-1))],m2[which(Sum>(n-1))], m3[which(Sum>(n-1))], m4[which(Sum>(n-1))], m5[which(Sum>(n-1))], m6[which(Sum>(n-1))]) / 
      sum(sum(colSums(m1)), sum(colSums(m2)), sum(colSums(m3)), sum(colSums(m4)), sum(colSums(m5)), sum(colSums(m6)))
  }#eo function
  chloe(rand_vir[[z]], rand_nca[[z]], rand_mad[[z]], rand_mari[[z]], rand_haw[[z]] , rand_jap[[z]], Sum = pa_sum, n = 5)   
})#eo lapply


nm_interactions6 <- lapply(1:p, function (z) {
  chloe <- function(m1, m2, m3, m4, m5, m6, Sum, n) {
      sum(m1[which(Sum>(n-1))],m2[which(Sum>(n-1))], m3[which(Sum>(n-1))], m4[which(Sum>(n-1))], m5[which(Sum>(n-1))], m6[which(Sum>(n-1))]) / 
      sum(sum(colSums(m1)), sum(colSums(m2)), sum(colSums(m3)), sum(colSums(m4)), sum(colSums(m5)), sum(colSums(m6)))
  }#eo function
    chloe(rand_vir[[z]], rand_nca[[z]], rand_mad[[z]], rand_mari[[z]], rand_haw[[z]] , rand_jap[[z]], Sum = pa_sum, n = 6)   
  })#eo lapply


nm_interactions2 <- do.call(rbind, nm_interactions2)
nm_interactions3 <- do.call(rbind, nm_interactions3)
nm_interactions4 <- do.call(rbind, nm_interactions4)
nm_interactions5 <- do.call(rbind, nm_interactions5)
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
z2 <- (obs_int$value[obs_int$nb_int == 2] - mean(rand_int$value[rand_int$nb_int == 2])) / sd(rand_int$value[rand_int$nb_int == 2])
z3 <- (obs_int$value[obs_int$nb_int == 3] - mean(rand_int$value[rand_int$nb_int == 3])) / sd(rand_int$value[rand_int$nb_int == 3])
z4 <- (obs_int$value[obs_int$nb_int == 4] - mean(rand_int$value[rand_int$nb_int == 4])) / sd(rand_int$value[rand_int$nb_int == 4])
z5 <- (obs_int$value[obs_int$nb_int == 5] - mean(rand_int$value[rand_int$nb_int == 5])) / sd(rand_int$value[rand_int$nb_int == 5])
z6 <- (obs_int$value[obs_int$nb_int == 6] - mean(rand_int$value[rand_int$nb_int == 6])) / sd(rand_int$value[rand_int$nb_int == 6])


#Plot of proportions of shared interactions
prop.plot <- ggplot(rand_int, aes(x=nb_int, y=value)) +
  stat_summary(fun = mean, geom="point",size=1) + 
  stat_summary(fun.data  = quantiles_95,
               geom="errorbar", size=0.3, width=0.08, color = "black") +
  stat_summary(fun = mean, geom="line", color="black", size=0.3, aes(group=1, linetype = "Null distributions"))+
  geom_point(data = obs_int, aes(x=as.factor(nb_int), y=value), size=1, shape=19, color = "black") +
  geom_line(data = obs_int, aes(x=as.factor(nb_int), y=value, group=1,linetype ="Observed values"), size=0.3, color = "black") +
  scale_linetype_manual(name = "", values = c("Observed values" = "solid", "Null distributions" = "dashed"),
                        guide = guide_legend(reverse=TRUE))+
  labs( x = "Number of food webs", y = "Proportion of shared interactions") +
  theme_classic() +
  theme(legend.position = c(0.7, 0.8),
        legend.box = NULL,
        legend.text = element_text(colour="black", size=8),
        panel.background = element_rect(colour = "black", size=0.1, fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size=11, colour = "black"),
        axis.line = element_line(size=0.4, colour = "black"),
        axis.ticks = element_line(colour = "black", size = 0.3)) 

prop.plot

#6. Correlation between quantitative and qualitative matrices  #####
#Marshall Islands
#Get the matrices into long format
df_mariIS <- melt(mari_ISmatrix2)
df_pa_sum <- melt(pa_sum) %>%
  dplyr::rename(nb_int = value)
df_mariIS2 <- left_join(df_mariIS, df_pa_sum) %>% filter(nb_int != 0) %>% mutate(site = "mari")
df_mariIS2$nb_int <- as.factor(df_mariIS2$nb_int)

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


######null model
sites<-c("haw","mad","mari","nca","vir","jap")
all_mats<-list(rand_haw,rand_mad,rand_mari,rand_nca,rand_vir,rand_jap) #you can reuse the null matrices generated before
nm_n<-length(all_mats[[1]]) #number of null matrices
all_mats[1]
null_vals<-c()
for (n in 1:6){
  for (m in 1:nm_n){#see how I synthesized your code; check if ok
    mat<-all_mats[[n]][[m]]
    df_mat <- melt(mat)
    df_pa_sum <- melt(pa_sum) %>%
      dplyr::rename(nb_int = value)
    df_mat <- left_join(df_mat, df_pa_sum) %>% filter(nb_int != 0) %>% mutate(site = sites[n])
    mean_ov<-aggregate(df_mat$value~df_mat$nb_int+df_mat$site,FUN='mean')
    null_vals<-rbind(null_vals,mean_ov)
  }}

colnames(null_vals)<-c('nb_int','site','value',"data")

####plot example
pdf('null_interaction_strength_ov.pdf',width=12,height=8)
par(mfrow=c(2,3))
for (n in 1:6){
  loc_vals<-null_vals[null_vals$site==sites[n],]
  mean_vals<-aggregate(loc_vals$null_ov ~loc_vals$nb_int,FUN = 'mean')
  sd_vals<-aggregate(loc_vals$null_ov~loc_vals$nb_int,FUN = 'sd')
  x<-mean_vals[,1]
  y<-mean_vals[,2]
  y.sd<-sd_vals[,2]
  plot(x,y,las=1,xlab='food web n',ylab='strength of shared interactions',main=sites[n],ylim=c(min(y-y.sd),max(y+y.sd)),pch=16)
  lines(mean_vals[,1],mean_vals[,2])
  arrows(x0=x, y0=y-y.sd, x1=x, y1=y+y.sd, code=3, angle=90, length=0.05)
}
dev.off()

##################Chloe, add the null values to your plot as you like
df_allIS <- df_allIS %>% mutate(data = "obs")
null_vals <- null_vals %>% mutate(data = "null",
                                  nb_int = as.factor(nb_int)) 

test <- plyr::rbind.fill(df_allIS, null_vals) %>%
  mutate (site_name = case_when(site == "haw" ~ "Hawaii",
                                site == "mad" ~ "Madagascar",
                                site == "mari" ~ "Marshall Islands",
                                site == "nca"~ "New Caledonia",
                                site == "vir" ~ "West Indies",
                                site == "jap" ~ "Okinawa"),
          data = as.factor(data)) 


#Plot with mean_se  
df_allIS$nb_int <- as.factor(df_allIS$nb_int)

hhh<-df_allIS[df_allIS$site=='haw',]
b<-aggregate(hhh$value~hhh$nb_int,FUN='mean')
plot(as.numeric(b[,1]),as.numeric(b[,2]))


IS.plot <- ggplot(data=test, aes(x = nb_int, y=value, group = data)) + 
  geom_line(aes(linetype = data), color="black", size=0.3, stat="summary", fun = 'mean')+
  stat_summary(fun = mean, geom="point",size=1, color = "black") + 
  stat_summary(fun.data = mean_se, geom ="errorbar", size=0.3, width=0.2) +
  scale_linetype_manual(values = c("dashed","solid"), labels = c("Null distributions","Observed values"),
                        guide = guide_legend(reverse=TRUE))+
  labs(x = "Number of food webs", y="Strength of shared interactions", linetype = "") + 
  facet_wrap(. ~ site_name)+
  theme(strip.text.x = element_text(size=9, face="bold"),
        strip.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(colour = "black",size=0.3, lineend = "butt"),
        axis.line.y = element_line(colour = "black", size=0.3),
        axis.ticks = element_line(colour = "black", size = 0.3),
        axis.title = element_text(size=12),
        axis.text = element_text(size=11, colour = "black"),
        legend.text = element_text(colour="black", size=8),
        legend.key = element_rect(fill = NA),
        legend.position = c(0.13,0.89),
        legend.box = NULL) 

IS.plot

  #7. Combine the two plots and save
shared.int.plot <- ggarrange(prop.plot, IS.plot, 
          labels = c("A","B"),
          font.label = list(size = 8, face = "bold"),
          ncol = 2, nrow = 1,
          widths = c(1.5,2))
shared.int.plot
#ggsave("output/figures/fig2.pdf", plot = shared.int.plot, width = 17.8, height = 7.5, units = "cm", dpi = 300)

ggsave("output/figures/fig2_shared interactions.png", plot = shared.int.plot, units="in", width=10, height=4, dpi=300)
