#####################
#Barplots food items#
#####################

library(tidyverse)
library(plyr)
library(ggplot2)

#Getting one dataset per site
haw <- data_clean2 %>% filter(site_code=="haw")
vir <- data_clean2 %>% filter(site_code=="vir")
mari <- data_clean2 %>% filter(site_code=="mari")
mad <- data_clean2 %>% filter(site_code=="mad")

    ####Grp3####
nb.grp3 <- data_clean2 %>% 
  group_by(site_code) %>%
  dplyr::count(grp3) 

sum <- ddply(nb.grp3, c("site_code"), summarise, sum=sum(n))

nb.grp3 <- nb.grp3 %>% left_join(sum, by="site_code")

nb.grp3 <- nb.grp3 %>% mutate(per = n*100/sum) %>%
  filter(!is.na(grp3))


# Basic barplot
ggplot(subset(nb.grp3, grp3 %in% "Algae"), aes(y=per, x=site_code)) +
  geom_bar(stat="identity") + 
  theme_minimal() +
  ggtitle("Algae") 
  
#Barplot with one plot for each subet --> facetting
ggplot(data = nb.grp3, aes(site_code, per, fill=site_code)) + 
  geom_bar(stat="identity") + facet_wrap(.~grp3) 


####Grp1####
nb.grp1 <- data_clean2 %>% 
  group_by(site_code) %>%
  dplyr::count(grp1) 

sum <- ddply(nb.grp1, c("site_code"), summarise, sum=sum(n))

nb.grp1 <- nb.grp1 %>% left_join(sum, by="site_code")

nb.grp1 <- nb.grp1 %>% mutate(per = n*100/sum) %>%
  filter(!is.na(grp1))

#Barplot with one plot for each subet --> facetting
ggplot(data = nb.grp1, aes(site_code, per, fill=site_code)) + 
  geom_bar(stat="identity") + facet_wrap(.~grp1) 
  

####Class####
nb.class <- data_clean2 %>% 
  group_by(site_code) %>%
  dplyr::count(item_class) 

sum <- ddply(nb.class, c("site_code"), summarise, sum=sum(n))

nb.class <- nb.class %>% left_join(sum, by="site_code")

nb.class <- nb.class %>% mutate(per = n*100/sum) %>%
  filter(!is.na(item_class))

#Barplot with one plot for each subet --> facetting
ggplot(data = nb.class, aes(site_code, per, fill=site_code)) + 
  geom_bar(stat="identity") + facet_wrap(.~item_class) 



