######################################
#Cluster proies pour décision groupes#
######################################

#Prepare the dataset
data_ISfull_grp <- read.csv("data/data_ISfull_grp.csv", sep = ";") %>%
  mutate_if(is.factor, as.character)

data_ISfull_grp$item[is.na(data_ISfull_grp$item)] <- data_ISfull_grp$item_raw[is.na(data_ISfull_grp$item)]

#Get the variable for interaction strength with volper and itemfreq
data_sp_prey <- data_ISfull_grp  %>%
  plyr::ddply(.(fish_sp, item, site_code), summarise, volper = sum(item_volper), itemfreq = mean(item_freq)) %>%
  mutate(volfreq = volper/100,
    IS = coalesce(volfreq, itemfreq)) 

data_sp_prey2 <- data_sp_prey %>% 
  plyr::ddply(.(fish_sp, item), summarise, mean.IS = mean(IS))


#Create the matrix and standardize
data_ISmatrix_sp <- reshape2::acast(data_sp_prey2, item ~ fish_sp, value.var = "mean.IS") 
data_ISmatrix_sp[is.na(data_ISmatrix_sp)] <- 0  

data_ISmatrix_sp_std <- apply(data_ISmatrix_sp, 2, function(x) x/sum(x)) #standardized matrix, colSums = 1
colSums(data_ISmatrix_sp_std)


#Clustering
psych::iclust(data_ISmatrix_sp_std)


#CA : correspondance analysis
library("FactoMineR")
library("factoextra")


res.ca <- CA(data_ISmatrix_sp_std, ncp = 5, graph = FALSE)
fviz_ca_row(res.ca, repel = TRUE)
