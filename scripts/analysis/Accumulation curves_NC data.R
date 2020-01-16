##################################
#Accumulation curve New Caledonia#
##################################

library(tidyverse)
library(reshape2)
library(vegan)
library(readxl)

#1. Import dataset
data_nc <- read_excel("data/Data_NC.xlsx", sheet = "BIONC")

names(data_nc)[c(27:30)] <- c("diet1","diet2","diet3","diet4")


data_nc_sel <- data_nc %>% select(Family, nom, diet1, diet2, diet3, diet4) %>%
  group_by(nom) %>% mutate(ind = row_number())

#5. Clean the variables 
#Change Family name with lower cases and first letter as upper case
data_nc_sel$Family <- tolower(data_nc_sel$Family)
data_nc_sel$Family <-str_to_title(data_nc_sel$Family)


#Item
data_nc_sel2 <- gather(data_nc_sel, key = "nb_item", value = "item", diet1, diet2, diet3, diet4)#Collapse columne into column with single key values

data_nc_sel2 <- data_nc_sel2 %>% mutate(item_cor = case_when(item == "Poisson" ~ "Actinopterygii" ,
                                                     item == "vers" ~ "Worms",
                                                     item == "crabes" ~ "Crab",
                                                     item == "nudibranche" ~ "Nudibranchia", 
                                                     item =="bivalves" ~ "Bivalvia" , 
                                                     item == "crustacé" ~ "Crustacea" ,
                                                     item == "algues" ~ "Algae", 
                                                     item == "crevettes" ~ "Shrimp", 
                                                     item == "poulpe" ~ "Octopoda", 
                                                     item == "squilles" ~ "Stomatopoda",
                                                     item == "ophiure" ~ "Ophiuroidea",
                                                     item == "céphalopode" ~ "Cephalopoda", 
                                                     item == "bernard l'hermite" ~ "Paguroidea", 
                                                     item == "langouste/cigalles" ~ "Palinuridae/Scyllaridae",
                                                     item == "gastéropodes" ~ "Gastropoda", 
                                                     item == "mollusque" ~ "Mollusca", 
                                                     item == "alcyonaires (corail mou)" ~ "Alcyonacea", 
                                                     item == "seiche/calmar" ~ "Sepiida/Teuthida", 
                                                     item == "spirographe" ~ "Sabellidae", 
                                                     item == "oursin sable" ~ "Spatangoida",
                                                     item == "annélide" ~ "Annelida",
                                                     item == "amphipodes" ~ "Amphipoda",
                                                     item == "holothurie" ~ "Holothuroidea",
                                                     item == "méduse" ~ "Jellyfish",
                                                     item == "étoile de mer" ~ "Asteroidea",
                                                     item == "éponge" ~ "Porifera",
                                                     item == "strombes" ~ "Strombidae",
                                                     item == "oursin roche" ~ "Echinoidea",
                                                     item == "haliotis" ~ "Haliotidae",
                                                     item == "polychète" ~ "Polychaeta",
                                                     item == "cérithes" ~ "Cerithiidae",
                                                     item == "dollar sable" ~ "Clypeasteroida",
                                                     item == "foraminifère" ~ "Foraminifera",
                                                     item == "dentales" ~ "Dentaliidae",
                                                     item == "échinoderme" ~ "Echinodermata",
                                                     item == "coraux libres" ~ "Scleractinia", 
                                                     item == "halimeda" ~ "Halimeda",
                                                     item == "etoile de mer" ~ "Asteroidea",
                                                     item == "némerthe" ~ "Nemertea",
                                                     item == "débris" ~ "Debris", 
                                                     item == "siponculide"~ "Sipuncula",
                                                     item == "brachyopodes" ~ "Brachiopoda",
                                                     item == "copépodes" ~ "Copepoda",
                                                     item == "turritelles" ~ "Turritellidae",
                                                     item == "Algues" ~ "Algae",
                                                     item == "Plancton" ~ "Plankton", 
                                                     item == "plancton" ~ "Plankton",
                                                     item == "chiton" ~ "Polyplacophora",
                                                     item == "inconnu" ~ "Unknown",
                                                     item == "indéterminé" ~"Unknown",
                                                     item == "divers" ~ "Unknown"))

data_nc_sel2$item_raw <- data_nc_sel2$item_cor
data_nc_sel2$item_raw <- ifelse(is.na(data_nc_sel2$item_raw), data_nc_sel2$item, data_nc_sel2$item_cor)
data_nc_sel2 <- data_nc_sel2 %>% select(-item, -item_cor)
data_nc_sel2 <- data_nc_sel2 %>% filter(item_raw != "non examiné", 
                                        !is.na(item_raw),
                                        !is.na(nom))
data_nc_sel2 <- data_nc_sel2 %>% mutate(int = ifelse(item_raw =="Vide", 0, 1))

#Add prey groups
data_nc_sel2 <- left_join(data_nc_sel2 ,grp_dataISfull[,c(2,9)], by="item_raw") %>% unique(.) %>%
  mutate(grp6 = replace(grp6, item_raw == "Crustacea", "Crustacea"),
         grp6 = replace(grp6, item_raw == "Mollusca", "Mollusca"),
         grp6 = replace(grp6, item_raw == "Echinodermata", "Echinodermata")) #keep those groups instead of having NA's

#Get the matrice --> not right    
data_nc_sel2 <- data_nc_sel2 %>% mutate(pairwise_int = paste(nom,item_raw, sep="-"))

nc_mat <- reshape2::acast(data_nc_sel2, ind ~ pairwise_int, value.var = "int")

nc_mat2 <- nc_mat[, -grep("Vide", colnames(nc_mat))]

specaccum(t(nc_mat2),"random") %>% 
  plot(., ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="lightgrey", xlab= "Number of sampled individuals", ylab="Number of interactions",main="New Caledonia")



#Other trial dataframe
nc_df <- data_nc_sel2 %>% select(-Family, -nb_item) %>%
  unique(.) %>% #some items duplicated e.g. actinopterygii for diet 1 and diet 2, so unique() first then spread ! 
  spread(grp6, int) 

nc_df <- data_nc_sel2 %>% select(-Family, -nb_item) %>%
  unique(.) %>% #some items duplicated e.g. actinopterygii for diet 1 and diet 2, so unique() first then spread ! 
  spread(item_raw, int) %>%
  select(-Vide, -Unknown)

nc_df[is.na(nc_df)] <- 0


#Accumulation curve
nc_acc <-specaccum(nc_df[,-c(1:3)],"random")
plot(nc_acc, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="lightgrey", xlab= "Number of sampled individuals", ylab="Number of interactions",main="New Caledonia")





ggplot(data=nc_acc, aes(x=sites, y=nc_acc$richness)) +
geom_line(colour="blue") + 
  geom_ribbon(aes(ymin=sd, ymax=sd), alpha=0.2, fill = "blue") +
  ylab(label="Number of individuals sampled") + 
  xlab(label = "Number of interactions") + 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())

