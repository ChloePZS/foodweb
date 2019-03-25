######################
#Adding New Caledonia#
######################


library(tidyverse)

  #1. Import last versions of the datasets
nca <- read.csv("data/data_clean_nc.csv") %>%
  select(-X)

nca <- nca %>% select(Family,item_raw,   item_family, item_ord,    
                     item_class,  item_phylum, item_kingdom,site_code,   class,       
                     fish_sp,     genus_cor,   sp_cor) %>%
  dplyr::rename(family = Family, item_fam = item_family)
nca <- unique(nca)

names(nca)
data_clean2 <- read.csv("data/data_clean.csv") %>%
  select(-X) %>%
  dplyr::rename(family = family_cor, item_ord = item_ord_cor, item_fam = item_fam_cor) %>%
  dplyr::select(fish_sp,      site_code,    class,        family,       genus_cor,    sp_cor,      
                item_kingdom, item_phylum,  item_class,   item_ord,     item_fam,     item_gen_cor, item_sp_cor,  item_spname,  item_cor,     item_raw)
names(data_clean2)

data_clean2 %>% filter(item_cor=="Inorganic") #OK

  #2. Adding new caledonia rows
dim(nca)
dim(data_clean2)
data_full <- plyr::rbind.fill(data_clean2,nca)
data_full <- unique(data_full)


data_full <- data_full %>% 
  mutate_if(is.factor, as.character) %>% glimpse()

  #3. Get groups
#There will be an issue with item_raw == "Palinuridae/Scyllaridae
data_full <- data_full %>% mutate(item_raw = replace(item_raw, item_fam == "Palinuridae", "Palinuridae"),
                                  item_raw = replace(item_raw, item_fam == "Scyllaridae", "Scyllaridae"))

data_full %>% filter(item_raw=="Palinuridae/Scyllaridae") %>% select(fish_sp) #Check if renamed properly

#There will be an issue with item_raw == "Solenigastrids/chitons"
data_full %>% filter(item_raw == "Solenogastrids")

data_full <- data_full %>% mutate(item_raw = replace(item_raw, item_cor =="Polyplacophora", "Chitons"),
                                  item_raw = replace(item_raw, item_cor =="Solenogastres", "Solenogastrids"))


grp_full <- data_full %>% select(site_code,item_raw, item_class, item_ord, grp1, grp2, grp3, grp4)

grp_full <- unique(grp_full)

write_csv2(grp_full, "grp_full.csv")
rm(grp_full)


  #3. Prepare item groups that would fit all 5 sites
grp_full2 <- read.csv2("grp_full.csv") #Possible to change the groups on the csv file and re upload
glimpse(grp_full2)
grp_full2<- grp_full2 %>% 
  mutate_if(is.factor, as.character) %>% glimpse()

grp_full2 <- unique(grp_full2)

grp_full2 <- grp_full2 %>% select(item_raw, site_code,grp1,grp3,grp4,grp5, grp6, grp7)

data_full2 <- left_join(data_full, grp_full2, by = c("site_code","item_raw"), all.x=TRUE)
data_full2 <- unique(data_full2)

write.csv(data_full2, "data/data_full.csv")


  #4. Check number of species and item tax per site
####Check the % items ID per site####
#haw
haw <- data_full2 %>% filter(site_code=="haw")

haw_tax <- haw %>% select(8:13) %>%
  summarise_all(funs(sum(!is.na(.))))

haw_per <- round(haw_tax*100/832, 2)

#mad
mad <- data_full2%>% filter(site_code=="mad")

mad_tax <- mad %>% select(8:13) %>%
  summarise_all(funs(sum(!is.na(.))))

mad_per <- round(mad_tax*100/1541, 2)

#vir
vir <- data_full2%>% filter(site_code=="vir")

vir_tax <- vir %>% select(8:13) %>%
  summarise_all(funs(sum(!is.na(.))))

vir_per <- round(vir_tax*100/2907, 2)


#mari
mari <- data_full2%>% filter(site_code=="mari")

mari_tax <- mari %>% select(8:13) %>%
  summarise_all(funs(sum(!is.na(.))))

mari_per <- round(mari_tax*100/1017, 2)

#nca
nca <- data_full2%>% filter(site_code=="nca")

nca_tax <- nca %>% select(8:13) %>%
  summarise_all(funs(sum(!is.na(.))))

nca_per <- round(nca_tax*100/1016, 2)

tax <- bind_rows(mad_per, haw_per, vir_per, mari_per, nca_per) %>%
  rownames_to_column(var="site_code") %>%
  mutate(site_code = recode(site_code, "1" = "mad" , "2" = "haw","3" = "vir" , "4"="mari", "5"="nca"))

write.table(tax, "tax.txt", sep="\t")

####Check the nb of predators species and families per site####

plyr::ddply(data_full2, ~site_code, summarize, 
            n_fish_sp = length(unique(fish_sp)),
            n_fish_fam = length(unique(family)))

#Less species as unrelevant items removed
plyr::ddply(new_data, ~site_code, summarize, 
            n_fish_sp = length(unique(fish_sp)),
            n_fish_fam = length(unique(family_cor)))

plyr::ddply(data, ~site_code, summarize, 
            n_fish_sp = length(unique(fish_sp)),
            n_fish_fam = length(unique(family_cor)))

plyr::ddply(data_raw, ~site_code, summarize, 
            n_fish_sp = length(unique(fish_sp)),
            n_fish_fam = length(unique(family_cor)))
