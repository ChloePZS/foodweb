######################
#Adding New Caledonia#
######################

  #1. Import last versions of the datasets
nca <- read.csv("data/data_clean_nc.csv") %>%
  select(-X)

nca <- nca %>% select(Family,item_raw,   item_family, item_ord,    
                     item_class,  item_phylum, item_kingdom,site_code,   class,       
                     fish_sp,     genus_cor,   sp_cor) %>%
  dplyr::rename(family = Family, item_fam = item_family)

names(nca)
data_clean2 <- read.csv2("data/data_clean2.csv") %>%
  select(-X) %>%
  dplyr::rename(family = family_cor, item_ord = item_ord_cor, item_fam = item_fam_cor)
names(data_clean2)

  #2. Adding new caledonia rows
dim(nca)
dim(data_clean2)
data_full <- rbind.fill(data_clean2,nca)
data_full <- unique(data_full)

data_full<- data_full %>% 
  mutate_if(is.factor, as.character) %>% glimpse()

  #3. Get groups
#There will be an issue with item_raw == "Palinuridae/Scyllaridae
data_full <- data_full %>% mutate(item_raw = replace(item_raw, item_fam == "Palinuridae", "Palinuridae"),
                     item_raw = replace(item_raw, item_fam == "Scyllaridae", "Scyllaridae"))

data_full %>% filter(item_raw=="Palinuridae/Scyllaridae") %>% select(fish_sp)

data_full <- data_full %>% filter(is.na(item_raw) | item_raw != "Palinuridae/Scyllaridae")

grp_full <- data_full %>% select(site_code,item_raw, item_class, item_ord, grp1, grp2, grp3, grp4)

grp_full <- unique(grp_full)

write_csv2(grp_full, "grp_full.csv")

  #3. Prepare item groups that would fit all 5 sites
grp_full2 <- read.csv2("grp_full.csv")
glimpse(grp_full2)
grp_full2<- grp_full2 %>% 
  mutate_if(is.factor, as.character) %>% glimpse()

data_full2 <- left_join(data_full, grp_full2, by = c("item_raw","site_code"))
data_full2 <- unique(data_full2)

a <- grp_full2 %>% dplyr::count(item_raw, grp3)
a[duplicated(a$item_raw),]

names(grp_full2)


