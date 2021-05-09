########################
#Trophic specialization#
########################

#packages
library(tidyverse); library(plotrix); library(picante)

    #1. Load data ####
a <- read.csv('data/foodweb_final.csv',header = T)
sites <- unique(a$site)
a <- a %>% select(., site, cons_class, cons_fam, cons_sp, cons_size, lev1, lev2, w) 


    #2. Generate matrices ####
#use lev2 prey classification and species level
get_mat<-function(l,fun='mean',bin='F'){
  if (fun=='mean'){
    l_mat<-dcast(l,lev2~cons_sp,value.var="w",fun.aggregate = mean,fill=0,drop=T)} 
  else {
    l_mat<-dcast(l,lev2~cons_sp,value.var="w",fun.aggregate = sum, fill=0,drop=T)}
  #use fun.aggregate = sum for vir, mean for the others
  row.names(l_mat)<-l_mat[,1]
  l_mat<-l_mat[,2:dim(l_mat)[2]]
  l_mat<-t(t(l_mat)/colSums(l_mat))
  if (bin!='F'){l_mat<-1*(l_mat>0)}
  l_mat[is.nan(l_mat)] <- 0
  return (l_mat)
}  

#weighted matrices
vir_mat_sp <- get_mat(a[a$site=='vir',],fun='sum')
mari_mat_sp <- get_mat(a[a$site=='mari',])
nca_mat_sp <- get_mat(a[a$site=='nca',])
haw_mat_sp <- get_mat(a[a$site=='haw',])
mad_mat_sp <- get_mat(a[a$site=='mad',])
jap_mat_sp <- get_mat(a[a$site=='jap',])

matW_sp <- list(vir_mat_sp, mad_mat_sp, mari_mat_sp, nca_mat_sp, haw_mat_sp, jap_mat_sp)

#binary matrices
pa_vir_sp <- get_mat(a[a$site=='vir',],fun='sum', bin = 'T')
pa_mari_sp <- get_mat(a[a$site=='mari',], bin = 'T')
pa_nca_sp <- get_mat(a[a$site=='nca',], bin = 'T')
pa_haw_sp <- get_mat(a[a$site=='haw',], bin = 'T')
pa_mad_sp <- get_mat(a[a$site=='mad',], bin = 'T')
pa_jap_sp <- get_mat(a[a$site=='jap',], bin = 'T')

matB_sp <- list(pa_vir_sp, pa_mad_sp, pa_mari_sp, pa_nca_sp, pa_haw_sp, pa_jap_sp)


    #3. Generate matrices with interaction strength thresholds ####
#list for all weighted matrices with treshold of 0(original matrix), 0.25 and 0.5
list_matW <- list()
for(m in 1:6){
  mat <- matW_sp[[m]]
  mat_0.25 <- mat * (mat > 0.25)
  mat_0.5 <- mat * (mat > 0.5)
  list_matW[[m]] <- list(matW_sp = matW_sp[[m]], mat_0.25 = mat_0.25, mat_0.5 = mat_0.5)
}
names(list_matW) <- c("vir","mad","mari","nca","haw","jap")

#create a binary version of all matrices
list_matB <- lapply(list_matW, lapply, decostand, method = "pa") 

    #4. Proportion of resources used by each consumers ####
#function for the prop
prop_spe <- function (x){
  unname(apply(x,2, function(y) sum(y != 0)))/nrow(x)
}

list_prop <- lapply(list_matB, lapply, prop_spe) 

#tropic specialization across all regions
prop_spe <- data.frame(mean_prop = c(mean(unlist(lapply(list_prop, '[[', 1))),
                                     mean(unlist(lapply(list_prop, '[[', 2))),
                                     mean(unlist(lapply(list_prop, '[[', 3)))),
                       se = c(std.error(unlist(lapply(list_prop, '[[', 1))),
                                   std.error(unlist(lapply(list_prop, '[[', 2))),
                                   std.error(unlist(lapply(list_prop, '[[', 3)))), 
                       tre = c("0","0.25","0.5"))
prop_spe

#trophic specialization by regions
prop_spe_site <- data.frame(mean_prop = unlist(lapply(list_prop, lapply, mean)),
                            se = unlist(lapply(list_prop, lapply, std.error)),
                            site = rep(c("vir","mad","mari","nca","haw","jap"), each=3),
                            tre = rep(c("0","0.25","0.5"), times = 3)) 
prop_spe_site

