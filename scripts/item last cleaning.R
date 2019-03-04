library(readxl)
library(tidyverse)
library(dplyr)
library(tidyr)
library(data.table)

data_raw <- read_excel("data/dietdata2.0.xlsx", sheet = "Sheet1", trim_ws=TRUE)

data <- data_raw %>% 
  select(site_code,class,family_cor,fish_sp,genus_cor,sp_cor,item_type,
         item_kingdom, item_phylum , item_class,item_ord_cor,item_fam_cor,
         item_cor,item_gen_cor, item_sp_cor,nb_sample,nb_guts ,    
         nb_item ,     item_freq ,   item_volper,item_massper, item_numper,
         mean_SL  ,    min_SL  ,max_SL, min_TL ,max_TL,time,source) 

data$item_spname <- paste(data$item_gen_cor, data$item_sp_cor, " ") #new variable with sp name

unique(data$item_sp_cor)


?ddply
names(data)

data$item_type <- as.character(data$item_type)
data$item_kingdom <- as.character(data$item_kingdom)  
data$item_phylum <- as.character(data$item_phylum)
data$item_class <- as.character(data$item_class)
data$item_gen_cor <- as.character(data$item_gen_cor)
data$item_sp_cor <- as.character(data$item_sp_cor)
data$item_ord_cor <- as.character(data$item_ord_cor)
data$item_fam_cor <- as.character(data$item_fam_cor)
data$item_cor <- as.character(data$item_cor)

data$site_code <- as.character(data$site_code)

    #Check the % items ID per site

table(data$item_phylum %in% data$item_cor, by=data$site_code)


data$item_sp_cor[data$item_sp_cor=="sp"] <- NA


#haw
haw <- data%>% filter(site_code=="haw")

haw_tax <- haw %>% select(7:15) %>%
  summarise_all(funs(sum(!is.na(.))))

haw_per <- round(haw_tax*100/968, 2)

#mad
mad <- data%>% filter(site_code=="mad")

mad_tax <- mad %>% select(7:15) %>%
  summarise_all(funs(sum(!is.na(.))))

mad_per <- round(mad_tax*100/3246, 2)

#vir
vir <- data%>% filter(site_code=="vir")

vir_tax <- vir %>% select(7:15) %>%
  summarise_all(funs(sum(!is.na(.))))

vir_per <- round(vir_tax*100/3392, 2)


#mari
mari <- data%>% filter(site_code=="mari")

mari_tax <- mari %>% select(7:15) %>%
  summarise_all(funs(sum(!is.na(.))))

mari_per <- round(mari_tax*100/1154, 2)

tax <- bind_rows(mad_per, haw_per, vir_per, mari_per) %>%
  rownames_to_column(var="site_code") %>%
  mutate(site_code = recode(site_code, "1" = "mad" , "2" = "haw","3" = "vir" , "4"="mari"))

      ##Cleaning !!

#create a another data for removing items (just in case...)
datatest <- data
datatest$item_spname <- trimws(datatest$item_spname, which="both") #removing leading ad trailing whitespace

datatest$item_spname[datatest$item_spname=="NA NA"] <- NA #replace NA NA by NA

str(datatest$item_spname)


#datatest$item_cor <- data$item_spname #get rid of the item_cor variable as was a mix of all items col

 datatest <- datatest %>% select(-item_spname) %>%
   rename(item_spname = item_cor)
 

    ##Discard unrelevant items row
datatest$item_type <- as.factor(datatest$item_type) 
unique(datatest$item_type)
str(datatest$item_type)
levels(datatest$item_type)[2] <- NA
class(datatest$item_cor)

unique(datatest$item_type)
str(datatest$item_type)
levels(datatest$item_fam_cor)[205] <- NA
class(datatest$item_cor)

unique(datatest$item_type)
str(datatest$item_type)
levels(datatest$item_ord_cor)[87] <- NA
class(datatest$item_cor)


datatest <- filter(datatest,!is.na(item_type)) #Remove items for which the type is NA  
datatest <- filter(datatest,!is.na(item_cor) , item_kingdom %in% c("Animalia","Plantae")) #Remove row for which item = Na and kingdom Animalia or Plantae
datatest <- filter(datatest %>% filter(!item_cor=="Gurry")) #remove gurry items
datatest <- filter(datatest, !item_cor=="Animalia" & !item_cor=="Plantae")

datatest_cor <- datatest %>%
  mutate(item_cor=recode(item_cor,"Fish"="Actinopterygii")) %>% #rename 
  filter(time!="night") %>%
  mutate(lower_level = case_when(
    !is.na(item_spname) ~ "Species",
    item_cor==item_fam_cor ~ "Family", 
    item_cor==item_ord_cor ~ "Order",
    item_cor==item_class ~ "Class",
    item_cor==item_phylum ~ "Phylum")) #create a variable of the last levels

datatest_cor$lower_level
ord <- c("Species","Family","Order","Class","Phylum")
datatest_cor$lower_level <- factor(datatest_cor$lower_level, levels=ord, ordered=TRUE)

class(datatest_cor$lower_level)

min(datatest_cor$lower_level, na.rm=T)


datatest_cor$lower_level_sp <- sapply(datatest_cor$fish_sp, function(x){
  min(datatest_cor$lower_level)
})


class(datatest$item_cor)
class(datatest$item_fam_cor)
class(datatest$item_ord_cor)
class(datatest$item_class)
class(datatest$item_phylum)

datatest$item_cor <- as.character(datatest$item_cor)
datatest$item_fam_cor <- as.character(datatest$item_fam_cor)
datatest$item_ord_cor <- as.character(datatest$item_ord_cor)
datatest$item_class <- as.character(datatest$item_class)
datatest$item_phylum <- as.character(datatest$item_phylum)

datatest_cor <- datatest %>% 
  gather("item_kingdom", "item_phylum", "item_class", "item_ord_cor",
         "item_fam_cor", "item_gen_cor", "item_spname", key = "level", 
         value = "Tax_id")













table(data$item_phylum %in% data$item_cor)

table(data$item_class %in% data$item_cor)

table(data$item_ord_cor %in% data$item_cor)

table(data$item_fam_cor %in% data$item_cor)

table(data$item_gen_cor %in% data$item_cor)
