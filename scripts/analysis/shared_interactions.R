#################################
#Analysis on shared interactions#
#################################

#packages
library(tidyverse); library(data.table); library(plyr); library(reshape2); library(ggpubr)

    #1. Load data ####
a <- read.csv('data/foodweb_final.csv',header = T)
sites <- unique(a$site)
a <- a %>% select(., site, cons_class, cons_fam, cons_sp, cons_size, lev1, lev2, w) 

    #2. Generate matrices ####
#matrix of the same size and for comparison (i.e. each matrix includes all consumers and prey across all sites)
#at the family level and using "lev2" prey grouping (i.e. main classification)
get_mat<-function(l,fun='mean',bin='F'){ #function for generating matrices
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

#weighted matrices
vir_mat_fam <- get_mat(a[a$site=='vir',],fun='sum')
mari_mat_fam <- get_mat(a[a$site=='mari',])
nca_mat_fam <- get_mat(a[a$site=='nca',])
haw_mat_fam <- get_mat(a[a$site=='haw',])
mad_mat_fam <- get_mat(a[a$site=='mad',])
jap_mat_fam <- get_mat(a[a$site=='jap',])
  
#binary matrices
pa_vir <- get_mat(a[a$site=='vir',],fun='sum', bin = 'T') ; pa_vir[is.na(pa_vir)] <- 0
pa_mari <- get_mat(a[a$site=='mari',], bin = 'T'); pa_mari[is.na(pa_mari)] <- 0
pa_nca <- get_mat(a[a$site=='nca',], bin = 'T'); pa_nca[is.na(pa_nca)] <- 0
pa_haw <- get_mat(a[a$site=='haw',], bin = 'T') ; pa_haw[is.na(pa_haw)] <- 0
pa_mad <- get_mat(a[a$site=='mad',], bin = 'T'); pa_mad[is.na(pa_mad)] <- 0
pa_jap <- get_mat(a[a$site=='jap',], bin = 'T'); pa_jap[is.na(pa_jap)] <- 0

pa_sum <- pa_jap +  pa_vir + pa_mari + pa_nca + pa_haw + pa_mad  #get the sum of the binary matrices 

    #3. Null model and random matrices ####
#null model function
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

#create randomized matrices; also for the previous code, it would be much better to have a single
#list indexed by locality including the observed and the null matrices
rand_vir <- list()
rand_mari <- list()
rand_nca <- list()
rand_mad <- list()
rand_haw <- list()
rand_jap <- list()

for (i in 1:1000){
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

    #4. Proportion of shared interactions ####
#function for the metric
shared_int <- function(m1, m2, m3, m4, m5, m6, Sum, n) {
  sum(m1[which(Sum>(n-1))],m2[which(Sum>(n-1))], m3[which(Sum>(n-1))], m4[which(Sum>(n-1))], m5[which(Sum>(n-1))], m6[which(Sum>(n-1))]) / 
    sum(sum(colSums(m1)), sum(colSums(m2)), sum(colSums(m3)), sum(colSums(m4)), sum(colSums(m5)), sum(colSums(m6)))
}

  #4.1 observed proportions of shared interactions 
obs_int <-c(shared_int(vir_mat_fam, nca_mat_fam, mari_mat_fam, mad_mat_fam, haw_mat_fam, jap_mat_fam, Sum= pa_sum, n=6),
             shared_int(vir_mat_fam, nca_mat_fam, mari_mat_fam, mad_mat_fam, haw_mat_fam, jap_mat_fam, Sum= pa_sum, n=5),  
             shared_int(vir_mat_fam, nca_mat_fam, mari_mat_fam, mad_mat_fam, haw_mat_fam,jap_mat_fam, Sum= pa_sum, n=4)  ,
             shared_int(vir_mat_fam, nca_mat_fam, mari_mat_fam, mad_mat_fam, haw_mat_fam, jap_mat_fam, Sum= pa_sum, n=3),
             shared_int(vir_mat_fam, nca_mat_fam, mari_mat_fam, mad_mat_fam, haw_mat_fam,jap_mat_fam, Sum= pa_sum, n=2)) %>%
            data.frame(value = ., nb_int = c(seq(6,2)), dat = "obs") 


  #4.2 random matrices
#function to get the metric for each of the p randomizations and for n number of networks with shared interactions (from 2 to 6)
p <- 1000
nm_int2 <- lapply(1:p, function (z) {
  shared_int <- function(m1, m2, m3, m4, m5, m6, Sum, n) {
    sum(m1[which(Sum>(n-1))],m2[which(Sum>(n-1))], m3[which(Sum>(n-1))], m4[which(Sum>(n-1))], m5[which(Sum>(n-1))], m6[which(Sum>(n-1))]) / 
      sum(sum(colSums(m1)), sum(colSums(m2)), sum(colSums(m3)), sum(colSums(m4)), sum(colSums(m5)), sum(colSums(m6)))
  }#eo function
  shared_int(rand_vir[[z]], rand_nca[[z]], rand_mad[[z]], rand_mari[[z]], rand_haw[[z]] , rand_jap[[z]], Sum = pa_sum, n = 2)   
})#eo lapply

nm_int3 <- lapply(1:p, function (z) {
  shared_int <- function(m1, m2, m3, m4, m5, m6, Sum, n) {
    sum(m1[which(Sum>(n-1))],m2[which(Sum>(n-1))], m3[which(Sum>(n-1))], m4[which(Sum>(n-1))], m5[which(Sum>(n-1))], m6[which(Sum>(n-1))]) / 
      sum(sum(colSums(m1)), sum(colSums(m2)), sum(colSums(m3)), sum(colSums(m4)), sum(colSums(m5)), sum(colSums(m6)))
  }#eo function
  shared_int(rand_vir[[z]], rand_nca[[z]], rand_mad[[z]], rand_mari[[z]], rand_haw[[z]] , rand_jap[[z]], Sum = pa_sum, n = 3)   
})#eo lapply

nm_int4 <- lapply(1:p, function (z) {
  shared_int <- function(m1, m2, m3, m4, m5, m6, Sum, n) {
    sum(m1[which(Sum>(n-1))],m2[which(Sum>(n-1))], m3[which(Sum>(n-1))], m4[which(Sum>(n-1))], m5[which(Sum>(n-1))], m6[which(Sum>(n-1))]) / 
      sum(sum(colSums(m1)), sum(colSums(m2)), sum(colSums(m3)), sum(colSums(m4)), sum(colSums(m5)), sum(colSums(m6)))
  }#eo function
  shared_int(rand_vir[[z]], rand_nca[[z]], rand_mad[[z]], rand_mari[[z]], rand_haw[[z]] , rand_jap[[z]], Sum = pa_sum, n = 4)   
})#eo lapply

nm_int5 <- lapply(1:p, function (z) {
  shared_int <- function(m1, m2, m3, m4, m5, m6, Sum, n) {
    sum(m1[which(Sum>(n-1))],m2[which(Sum>(n-1))], m3[which(Sum>(n-1))], m4[which(Sum>(n-1))], m5[which(Sum>(n-1))], m6[which(Sum>(n-1))]) / 
      sum(sum(colSums(m1)), sum(colSums(m2)), sum(colSums(m3)), sum(colSums(m4)), sum(colSums(m5)), sum(colSums(m6)))
  }#eo function
  shared_int(rand_vir[[z]], rand_nca[[z]], rand_mad[[z]], rand_mari[[z]], rand_haw[[z]] , rand_jap[[z]], Sum = pa_sum, n = 5)   
})#eo lapply

nm_int6 <- lapply(1:p, function (z) {
  shared_int <- function(m1, m2, m3, m4, m5, m6, Sum, n) {
      sum(m1[which(Sum>(n-1))],m2[which(Sum>(n-1))], m3[which(Sum>(n-1))], m4[which(Sum>(n-1))], m5[which(Sum>(n-1))], m6[which(Sum>(n-1))]) / 
      sum(sum(colSums(m1)), sum(colSums(m2)), sum(colSums(m3)), sum(colSums(m4)), sum(colSums(m5)), sum(colSums(m6)))
  }#eo function
    shared_int(rand_vir[[z]], rand_nca[[z]], rand_mad[[z]], rand_mari[[z]], rand_haw[[z]], rand_jap[[z]], Sum = pa_sum, n = 6)   
  })#eo lapply

nm_intlist<- list("2" = nm_int2,"3" = nm_int3, "4" = nm_int4, "5" = nm_int5,"6" = nm_int6)

rand_int <- rbindlist(nm_intlist, idcol = "nb_int") %>% 
  gather(., -nb_int, key = "", value = "value") %>% 
  select(., nb_int, value) %>%
  mutate(., dat = "rand") #store proportions of random matrices into a df

  #4.3 Zscores & plot for proportions of shared interactions
#Z scores
df_int <- rbind(obs_int, rand_int)
ddply(df_int, .(nb_int), summarize, 
      z_score = ((value[dat=="obs"] - mean(value[dat=="rand"]))/sd(value[dat=="rand"])))

#plot
quantiles_95 <- function(x) {
  r <- c(mean(x), quantile(x, probs=c(0.05)),quantile(x, probs=c(0.95)))
  names(r) <- c("y","ymin","ymax")
  r
}

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

prop.plot + ggtitle("null model with body size ratio 0.5-1.2")

    #5. Strength of shared interactions ####
#correlation between quantitative and qualitative matrices 
  #5.1 get the matrices into a long format
df_pa_sum <- melt(pa_sum) %>%
  dplyr::rename(nb_int = value) %>% mutate(., nb_int = as.factor(nb_int)) #for the matrix of the sum of all binary matrices

#function for all observed matrices
obs_mats <- list(haw_mat_fam, mad_mat_fam, mari_mat_fam, nca_mat_fam, vir_mat_fam, jap_mat_fam) 
list_df <- list() #create a list to save my dataframes
for (n in 1:6){
  mat<-obs_mats[[n]]
  df_mat <- melt(mat)
  df_pa_sum <- melt(pa_sum) %>%
    dplyr::rename(nb_int = value)
  df_mat <- full_join(df_mat, df_pa_sum) %>% filter(nb_int != 0) %>% mutate(site = sites[n])
  list_df[[n]] <- df_mat
}

df_allIS <- do.call(rbind, list_df) %>% 
  mutate(site = as.factor(site), nb_int = as.factor(nb_int), data = "obs") 

  #5.2 random matrices
sites<-c("haw","mad","mari","nca","vir","jap")
all_mats<-list(rand_haw,rand_mad,rand_mari,rand_nca,rand_vir,rand_jap) #same null matrices as before
nm_n<-length(all_mats[[1]]) #number of null matrices
all_mats[1]
null_vals<-c()
for (n in 1:6){
  for (m in 1:nm_n){
    mat<-all_mats[[n]][[m]]
    df_mat <- melt(mat)
    df_pa_sum <- melt(pa_sum) %>%
      dplyr::rename(nb_int = value)
    df_mat <- full_join(df_mat, df_pa_sum) %>% filter(nb_int != 0) %>% mutate(site = sites[n])
    mean_ov<-aggregate(df_mat$value~df_mat$nb_int+df_mat$site,FUN='mean')
    null_vals<-rbind(null_vals,mean_ov)
  }}

colnames(null_vals)<-c('nb_int','site','value')
null_vals <- null_vals %>% mutate(data = "null",
                                  nb_int = as.factor(nb_int))

  #5.3 plot 
IS_df <- plyr::rbind.fill(df_allIS, null_vals) %>%
  mutate (site_name = case_when(site == "haw" ~ "Hawaii",
                                site == "mad" ~ "Madagascar",
                                site == "mari" ~ "Marshall Islands",
                                site == "nca"~ "New Caledonia",
                                site == "vir" ~ "West Indies",
                                site == "jap" ~ "Okinawa"),
          data = as.factor(data)) 

IS.plot <- ggplot(data=IS_df, aes(x = nb_int, y=value, group = data)) + 
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

    #6. Combine the two plots ####
shared.int.plot <- ggarrange(prop.plot, IS.plot, 
          labels = c("A","B"),
          font.label = list(size = 8, face = "bold"),
          ncol = 2, nrow = 1,
          widths = c(1.5,2))

shared.int.plot 

ggsave("output/figures/fig2_sharedint.png", plot = shared.int.plot, units="in", width=10, height=4, dpi=300)
