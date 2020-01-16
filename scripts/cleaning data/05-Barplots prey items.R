#####################
#Barplots food items#
#####################

library(tidyverse)
library(plyr)
library(ggplot2)

#Getting one dataset per site
haw <- data_full2 %>% filter(site_code=="haw")
vir <- data_full2 %>% filter(site_code=="vir")
mari <- data_full2 %>% filter(site_code=="mari")
mad <- data_full2 %>% filter(site_code=="mad")
nca <- data_full2 %>% filter(site_code=="nca")

####Grp3####
mad.grp3 <- data_full2 %>% 
  group_by(site_code) %>%
  dplyr::count(grp3) 

sum <- plyr::ddply(mad.grp3, c("site_code"), summarise, sum=sum(n))

mad.grp3 <- mad.grp3 %>% left_join(sum, by="site_code")

mad.grp3 <- mad.grp3 %>% mutate(per = n*100/sum) %>%
  filter(!is.na(grp3))


# Basic barplot
ggplot(subset(mad.grp3, grp3 %in% "Algae"), aes(y=per, x=site_code)) +
  geom_bar(stat="identity") + 
  theme_minimal() +
  ggtitle("Algae") 
  
#Barplot with one plot for each subet --> facetting
ggplot(data = mad.grp3, aes(site_code, per, fill=site_code)) + 
  geom_bar(stat="identity") + facet_wrap(.~grp3) 


####Grp1####
mad.grp1 <- data_full2 %>% 
  group_by(site_code) %>%
  dplyr::count(grp1) 

sum <- ddply(mad.grp1, c("site_code"), summarise, sum=sum(n))

mad.grp1 <- mad.grp1 %>% left_join(sum, by="site_code")

mad.grp1 <- mad.grp1 %>% mutate(per = n*100/sum) %>%
  filter(!is.na(grp1))

#Barplot with one plot for each subet --> facetting
ggplot(data = mad.grp1, aes(site_code, per, fill=site_code)) + 
  geom_bar(stat="identity") + facet_wrap(.~grp1) 
  

####Class####
mad.class <- data_full2 %>% 
  group_by(site_code) %>%
  dplyr::count(item_class) 

sum <- ddply(mad.class, c("site_code"), summarise, sum=sum(n))

mad.class <- mad.class %>% left_join(sum, by="site_code")

mad.class <- mad.class %>% mutate(per = n*100/sum) %>%
  filter(!is.na(item_class))

#Barplot with one plot for each subet --> facetting
ggplot(data = mad.class, aes(site_code, per, fill=site_code)) + 
  geom_bar(stat="identity") + facet_wrap(.~item_class) 



####Phylum####
mad.phylum <- data_full2 %>% 
  group_by(site_code) %>%
  dplyr::count(item_phylum) 

sum <- ddply(mad.phylum, c("site_code"), summarise, sum=sum(n))

mad.phylum <- mad.phylum %>% left_join(sum, by="site_code")

mad.phylum <- mad.phylum %>% mutate(per = n*100/sum) %>%
  filter(!is.na(item_phylum))

#Barplot with one plot for each subet --> facetting
ggplot(data = mad.phylum, aes(site_code, per, fill=site_code)) + 
  geom_bar(stat="identity") + facet_wrap(.~item_phylum) 


####Grp5####
mad.grp5 <- data_full2 %>% 
  group_by(site_code) %>%
  dplyr::count(grp5) 

sum <- ddply(mad.grp5, c("site_code"), summarise, sum=sum(n))

mad.grp5 <- mad.grp5 %>% left_join(sum, by="site_code")

mad.grp5 <- mad.grp5 %>% mutate(per = n*100/sum) %>%
  filter(!is.na(grp5))

#Barplot with one plot for each subet --> facetting
ggplot(data = mad.grp5, aes(site_code, per, fill=site_code)) + 
  geom_bar(stat="identity") + facet_wrap(.~grp5) 


#####Grp6#####
#Results dependent on the level of ID of the prey items so not good ! 
#Need to have the % of interactions from the matrix
nb.grp6 <- data_full2 %>% 
  group_by(site_code) %>%
  dplyr::count(grp6) 

sum <- ddply(nb.grp6, c("site_code"), summarise, sum=sum(n))

nb.grp6 <- nb.grp6 %>% left_join(sum, by="site_code")

nb.grp6 <- nb.grp6 %>% mutate(per = n*100/sum) %>%
  filter(!is.na(grp6))

#Barplot with one plot for each subet --> facetting
ggplot(data = nb.grp6, aes(site_code, per, fill=site_code)) + 
  geom_bar(stat="identity") + facet_wrap(.~grp6) 


#With matrix info
sum(vir.matrix.fam)
rowSums(vir.matrix.fam)

vir.grp6 <- as.data.frame(rowSums(vir.matrix.fam))
vir.grp6 <- tibble::rownames_to_column(vir.grp6)
colnames(vir.grp6)[2] <- "nb"
vir.grp6 <- vir.grp6 %>% mutate(site = "vir", sum = sum(nb), per = 100*nb/sum)

mari.grp6 <- as.data.frame(rowSums(mari.matrix.fam))
mari.grp6 <- tibble::rownames_to_column(mari.grp6)
colnames(mari.grp6)[2] <- "nb"
mari.grp6 <- mari.grp6 %>% mutate(site = "mari", sum = sum(nb), per = 100*nb/sum)

haw.grp6 <- as.data.frame(rowSums(haw.matrix.fam))
haw.grp6 <- tibble::rownames_to_column(haw.grp6)
colnames(haw.grp6)[2] <- "nb"
haw.grp6 <- haw.grp6 %>% mutate(site = "haw", sum = sum(nb), per = 100*nb/sum)

mari.grp6 <- as.data.frame(rowSums(mari.matrix.fam))
mari.grp6 <- tibble::rownames_to_column(mari.grp6)
colnames(mari.grp6)[2] <- "nb"
mari.grp6 <- mari.grp6 %>% mutate(site = "mari", sum = sum(nb), per = 100*nb/sum)

mad.grp6 <- as.data.frame(rowSums(mad.matrix.fam))
mad.grp6 <- tibble::rownames_to_column(mad.grp6)
colnames(mad.grp6)[2] <- "nb"
mad.grp6 <- mad.grp6 %>% mutate(site = "mad", sum = sum(nb), per = 100*nb/sum)

nca.grp6 <- as.data.frame(rowSums(nca.matrix.fam))
nca.grp6 <- tibble::rownames_to_column(nca.grp6)
colnames(nca.grp6)[2] <- "nb"
nca.grp6 <- nca.grp6 %>% mutate(site = "nca", sum = sum(nb), per = 100*nb/sum)

nb.grp6 <- rbind(vir.grp6, mari.grp6, haw.grp6, mad.grp6, nca.grp6)%>%
  rename(grp6 = rowname)


#Barplot with one plot for each subet --> facetting
ggplot(data = nb.grp6, aes(site, per, fill=site)) + 
  geom_bar(stat="identity") + facet_wrap(.~grp6) 

plyr::ddply(data_full2, ~site_code, summarize, 
            n_fish_sp = length(unique(fish_sp)),
            n_fish_fam = length(unique(family)))

