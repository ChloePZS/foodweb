#####################################
#Check fish species for team meeting#
#####################################

library(tidyverse)
library(igraph)

#Check families in common for all five sites : 90 families in total, only 17 in common in all three sites
intersect_all <- function(a,b,...){
  Reduce(intersect, list(a,b,...))
}

fam_com <- intersect_all(nca$family, haw$family, vir$family, mad$family, mari$family) #Only 17 families in common between the 5 locations

sp_com <- intersect_all(nca$fish_sp, haw$fish_sp, vir$fish_sp, mad$fish_sp, mari$fish_sp) #No fish species common to all five sites

sp_com1 <- intersect_all(nca$fish_sp, haw$fish_sp, mari$fish_sp) 

sp_site <- data_full2 %>% group_by(fish_sp, site_code) %>% select(fish_sp, site_code) %>%
  unique(.)

gen_site <- data_ISfull_grp_final2 %>% group_by(fish_sp, site_code) %>% select(fish_sp, site_code) %>%
  unique(.)

names(data_ISfull)


#Create a graph object
gen_site.graph <- graph.edgelist(as.matrix(gen_site))
gen_site.matrix <- get.adjacency(gen_site.graph, sparse=F)
dimnames(gen_site.matrix)

#g[!rownames(g) %in% remove, ]
remove <- c("vir","mari","mad","haw","nca","jap")
gen_site <- as.data.frame(gen_site.matrix[!rownames(gen_site.matrix) %in% remove,c("vir","mari","mad","haw","nca","jap")])

gen_site$sum <- rowSums(gen_site)

gen_site <- gen_site %>% rownames_to_column(., var="genus") %>%
  dplyr::left_join(data_ISfull[,c("family_cor", "genus")] , by="genus") %>%
  data.frame(.) %>%
  unique(.)

write.csv2(gen_site, "gen_site.csv")


names(gen_site)


               