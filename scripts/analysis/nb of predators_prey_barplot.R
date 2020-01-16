##################################################
#Check nb of species per site for each prey group#
##################################################

#Data frame with nb of species predating on prey items per site
nb_sp_int <- data_ISfull %>% select(site_code, fish_sp, grp6) %>% unique(.) %>% group_by(site_code) %>% dplyr::count(grp6) %>%
  mutate(nb_sp = case_when(site_code == "haw" ~ 91,
                                       site_code == "mad" ~139,
                                       site_code == "mari" ~ 139,
                                       site_code == "nca" ~ 164,
                                       site_code == "vir" ~ 191),
         prop_sp = n/nb_sp)

data_ISfull %>% filter(site_code == "haw" & grp6 == "Copepoda") %>% dplyr::count(fish_sp)

#To get the nb of species per site
plyr::ddply(data_ISfull, ~site_code, summarize, 
                            n_fish_sp = length(unique(fish_sp)))

#Facet bar plot
ggplot(nb_sp_int, aes(x = site_code, y=factor(n), fill=site_code)) +
  geom_bar(stat = "identity") + 
  facet_wrap(.~grp6, scales = "free") +
  scale_fill_manual(name = "Regions", labels = c("Hawaï","Madagascar", "Marshall Islands", "New Caledonia", "Virgin Islands"), 
                    values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  labs(y = "Number of species", x ="") +
  theme(strip.text.x = element_text(size=8, face="bold"),
                strip.text.y = element_blank(),
                panel.background = element_rect(fill = 'white'),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line.x = element_line(colour = "black", 
                                           size=0.5, 
                                           lineend = "butt"),
                axis.line.y = element_line(colour = "black", 
                                           size=0.5))



#With Families
nb_fam_int <- data_ISfull %>% select(site_code, family_cor, grp6) %>% unique(.) %>% group_by(site_code) %>% dplyr::count(grp6) %>%
  mutate(nb_fam = case_when(site_code == "haw" ~ 28,
                           site_code == "mad" ~35,
                           site_code == "mari" ~ 36,
                           site_code == "nca" ~ 48,
                           site_code == "vir" ~ 50),
         prop_fam = n/nb_fam)

data_ISfull %>% filter(site_code == "haw" & grp6 == "Copepoda") %>% dplyr::count(family_cor)

#To get the nb of families per site
plyr::ddply(data_ISfull, ~site_code, summarize, 
            n_family_cor = length(unique(family_cor)))

#Facet bar plot
ggplot(nb_fam_int, aes(x = site_code, y=factor(n), fill=site_code)) +
  geom_bar(stat = "identity") + 
  facet_wrap(.~grp6, scales = "free") +
  scale_fill_manual(name = "Regions", labels = c("Hawaï","Madagascar", "Marshall Islands", "New Caledonia", "Virgin Islands"), 
                    values = c("darkorchid4","turquoise3","chartreuse4","gold","coral1")) +
  labs(y = "Number of families", x ="") +
  theme(strip.text.x = element_text(size=8, face="bold"),
        strip.text.y = element_blank(),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(colour = "black", 
                                   size=0.5, 
                                   lineend = "butt"),
        axis.line.y = element_line(colour = "black", 
                                   size=0.5))


names(data_ISfull)
