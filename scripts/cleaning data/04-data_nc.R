############################
#Prepare New Caledonia data#
############################

library(readxl)
library(tidyr)
library(stringr)
library(taxize)
library(rfishbase)

  # 1. Import data and change variables names
data_nc <- read_excel("data/Data_NC.xlsx", sheet = "BIONC")

data_nc <- data_nc[,-c(23:26)]

names(data_nc)[c(18,23:26)] <- c("length","diet1","diet2","diet3","diet4")
names(data_nc)[27:30] <- c("diet1_per","diet2_per","diet3_per","diet4_per")


  #2. Create a data frame with one data for length metrics and samples per species
nc <- as.data.frame(tapply(data_nc$length, data_nc$nom, mean, na.rm=T))
names (nc)[1] <- "mean_length"

nc <- data.frame(nom = row.names(nc), nc)
nc$nom <- as.character(nc$nom)

nc$max_length <- tapply(data_nc$length, data_nc$nom, max, na.rm=T)
nc$min_length <- tapply(data_nc$length, data_nc$nom, min, na.rm=T)   

a <- as.data.frame(table(data_nc$nom))
nc$nb_sample <- a$Freq

b <- as.data.frame(table(data_nc$nom[!data_nc$diet1=="Vide"])) #nb of individuals with no empty stomach
class(b$Var1)
b$Var1 <- as.character(b$Var1)
names(b)[2] <- "nb_guts"
names(b)[1] <- "nom"

nc <- merge(nc , b, by="nom", all=T)

rm(a) ; rm(b)


  #3. Now let's do the items! Need to have one row per items and not per individual

#a %>% gather(key="item", v)

data %>% gather(key, value, ..., na.rm = FALSE, convert = FALSE)

data_nc2 <- gather(data_nc, key = "nb_item", value = "item", diet1, diet2, diet3, diet4)#Collapse columne into column with single key values

data_nc2.2 <- gather(data_nc,
                   key = "nb_item",
                   value = "item_per", 
                   diet1_per,diet2_per,diet3_per,diet4_per) 

#data_nc2 <- data_nc2 %>% left_join(data_nc2, data_nc3[,c("nom","%nb_item","item_per")], by="nom") #Erreur : impossible d'allouer un vecteur de taille 3.8 Go
library(data.table)
a <- melt(as.data.table(data_nc), measure.vars = patterns("diet", "_per")) #Well we don"t really care about the %

unique(data_nc2$item)


  #4. Remove unrelevant rows and merge data
#Check time of sample
max(data_nc2[,7])
min(data_nc2[,7])
max(data_nc2[,8]) #9999 ????
min(data_nc2[,8])

data_nc2 %>% filter(data_nc2[,8]=="9999")

#Remove useless items 
data_nc2 <- data_nc2 %>% filter(item!="Vide" , item!="non examiné" , item!="indéterminé" , item!="inconnu" , item!="divers") %>%
  filter(!is.na(item))

data_nc2.2 <- data_nc2.2 %>% filter(item_per!=0)

data_nc2.2 %>% filter(nom=="Lethrinus nebulosus") %>% select(item_per)

nb_item <- data_nc2 %>% group_by(nom) %>% count(item) %>% filter(!is.na(nom))

per_item <- data_nc2.2 %>% group_by(nb_item, nom) %>% count(item_per)

names(data_nc2)

#Merge date set
data_nc3 <- unique(data_nc2[c("Family","Genus","Species","nom","item")]) %>%
  dplyr::left_join(nc,by = "nom", all.x=T) %>% 
  filter(!is.na(Family)) %>%
  dplyr::left_join(nb_item, by = c("nom","item")) %>% 
  mutate(freq_item = n/nb_guts)



  #5. Clean the variables 
#Change Family name with lower cases and first letter as upper case
data_nc3$Family <- tolower(data_nc3$Family)
data_nc3$Family <-str_to_title(data_nc3$Family)

#Get rid of the "sp." and "spp."
unique(data_nc3$Species)
data_nc3$Species[data_nc3$Species == "sp."| data_nc3$Species == "spp."] <- NA

data_nc3$nom <- gsub("spp.", "sp",data_nc3$nom)
data_nc3$nom <- gsub("sp.", "sp",data_nc3$nom)
unique(data_nc3$nom)


#Renaming item names
unique(data_nc3$item)
data_nc3 <- data_nc3 %>% mutate(item_raw = case_when(item == "Poisson" ~ "Actinopterygii" ,
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
                                         item == "chiton" ~ "Polyplacophora"))


    #6. Get taxonomy of items
library(acs)
api.key.install("748f08277d8b379af2d4d27561b458c9c108", file = "key.rda")

Sys.setenv(ENTREZ_KEY = "748f08277d8b379af2d4d27561b458c9c108")

item_tax <- tax_name(data_nc3$item_raw, get=c("family", "order","class","phylum","kingdom"), db="ncbi")
item_tax <- item_tax[,-1]                                         

#Check tax names
unique(item_tax[c("query","family")])
item_tax <- item_tax %>% mutate(family = replace(family, query == "Palinuridae/Scyllaridae", "Palinuridae/Scyllaridae"))


unique(item_tax[c("query","order")])
item_tax <- item_tax %>% mutate(order = replace(order, query == "Shrimp", "Decapoda"),
                                order = replace(order, query == "Crab", "Decapoda"),
                                order = replace(order, query == "Sepiida/Teuthida", "Sepiida/Teuthida")) 

unique(item_tax[c("query","class")])
item_tax <- item_tax %>% mutate(class = replace(class, query == "Shrimp", "Malacostraca"),
                                class = replace(class, query == "Crab", "Malacostraca"),
                                class = replace(class, query == "Actinopterygii", "Actinopterygii"))

unique(item_tax[c("query","phylum")])
item_tax <- item_tax %>% mutate(phylum = replace(phylum, query == "Shrimp", "Arthropoda"),
                                phylum = replace(phylum, query == "Crab", "Arthropoda"),
                                phylum = replace(phylum, query == "Jellyfish", "Cnidaria"),
                                phylum = replace(phylum, query == "Foraminifera", "Foraminifera"))


unique(item_tax[c("query","kingdom")])
item_tax <- item_tax %>% mutate(kingdom = replace(kingdom, kingdom == "Metazoa", "Animalia"),
                                kingdom = replace(kingdom, query == c("Crab","Shrimp","Worms","Jellyfish"), "Animalia"),
                                kingdom = replace(kingdom, query == c("Algae","Halimeda"), "Plantae")
                                )
item_tax2 <- item_tax %>% filter((query != "Crab" | query != "Shrimp" | query != "Worms" | query != "Jellyfish" | query != "Algae") & !is.na(kingdom) & kingdom != "Viridiplantae")
unique(item_tax2[c("query","kingdom")])
names(item_tax2)[1] <- "item_raw"

#Merge data with item_tax
data_nc4 <- left_join(data_nc3, item_tax2, by = "item_raw", all.x=TRUE) %>%
  rename(item_family = family, item_ord = order, item_class = class, item_phylum = phylum, item_kingdom = kingdom)

data_nc4 <- unique(data_nc4)                    

data_nc4 %>% filter(item_raw == "Debris") #Check if Debris still here

  #6. Finishing to prepare data
data_nc4$site_code <- "nca"

#getting the class for predators species
class_sp <- tax_name(data_nc4$Family), get=c("class", db="ncbi"))

unique(class_sp[c("class","query")])

class_sp <- class_sp %>% mutate(class = replace(class, class =="Chondrichthyes", "Elasmobranchii"),
                            class = replace(class, class =="Teleostei", "Actinopterygii"),
                            class = replace(class, class =="Ciliatea", "Actinopterygii"),
                            class = replace(class, is.na(class), "Actinopterygii")) %>%
  select(Family, class)

data_nc5 <- left_join(data_nc4, class_sp, by="Family")
data_nc5 <- unique(data_nc5)      

#correcting family names
#Families names with class <NA>
Teraponidae
Ephippididae
Platacidae
Mugiloididae
Trichyuridae


data_nc5 <- data_nc5 %>% mutate(Family = replace(Family, Family == "Teraponidae", "Terapontidae"),
                                Family = replace(Family, Family == "Ephippididae", "Drepaneidae"),
                                Family = replace(Family, Family == "Mugiloididae", "Pinguipedidae"),
                                Family = replace(Family, Family == "Mugiloididae", "Pinguipedidae"),
                                Family = replace(Family, Family == "Trichyuridae", "Trichiuridae"))
                                
                                
data_nc5 %>% filter(Family == "Trichiuridae") %>% select(nom)


#Check sp name with Fishbase
class(data_nc5$nom)
fish_sp<-unique(data_nc5$nom)
sp_correct<-suppressWarnings(rfishbase::validate_names(fish_sp))
sp_errors<-fish_sp[!fish_sp %in% sp_correct]

length(unique(sp_correct)) #214
length(unique(sp_errors)) #25
length(unique(fish_sp)) #231 

print(sp_errors)

corrected <- c("Albula sp", "Hemitrygon bennettii","Gymnothorax chilospilus",         
"Synodus sp","Trachinocephalus myops","Tylosurus crocodilus",
"Sargocentron spiniferum","Platycephalus sp" ,"Epinephelus macrospilos",         
"Jaydia ellioti","Yarica hyalosoma","Pristicon trimaculatus",            
"Sillago sp","Carangidae sp" , "Carangoides sp",                 
"Caranx sp", "Gnathanodon speciosus","Megalaspis cordyla",              
"Gerres subfasciatus","Haemulidae sp", "Gymnocranius sp",                
"Lethrinus sp" ,"Scolopsis sp","Zebrasoma veliferum",            
"Arothron hispidus")

## Replace the errors
data_nc5$fish_sp <- data_nc5$nom
data_nc5$fish_sp[data_nc5$fish_sp%in%sp_errors] <- sapply(data_nc5$fish_sp[data_nc5$fish_sp%in%sp_errors],FUN = function(x){
  line <- which(sp_errors==x)
  y <- gsub(sp_errors[sp_errors==x],corrected[line],x = x)
  print(paste("correcting ",x))
  return(y)})

#check
data_nc5 %>% filter(fish_sp=="Epinephelus macrospilos")
data_nc5 %>% filter(nom=="Epinephelus macrospilos")

#Split fish_sp into genus and species
data_nc5$genus_cor<-data_nc5$fish_sp
data_nc5 <- data_nc5 %>% separate(genus_cor, c("genus_cor", "sp_cor"), sep=" ")

data_nc5 <- data_nc5 %>% select(-Genus,-Species)

#Duplicate for which two items but maybe couldn't do the difference... so let like that for now
palscy <- data_nc5 %>% filter(item_raw=="Palinuridae/Scyllaridae")
septeu <- data_nc5 %>% filter(item_raw=="Sepiida/Teuthida")

dup_palscy <- palscy %>% slice(rep(1:n(), each=2)) #duplicate each row)
dup_septeu <- septeu %>% slice(rep(1:n(), each=2))
                               
dup_palscy$item_family[c(2,4,6,8,10,12,14,16,18,20,22,24)] <- "Palinuridae" #Rename family accordingly
dup_palscy$item_family[c(1,3,5,7,9,11,13,15,17,19,21,23)] <- "Scyllaridae"

dup_septeu$item_ord[c(2,4,6,8,10,12,14,16,18,20)] <- "Sepiida" #Rename family accordingly
dup_septeu$item_ord[c(1,3,5,7,9,11,13,15,17,19)] <- "Teuthida"

data_nc6 <- rbind(data_nc5, dup_palscy, dup_septeu) #Add rows

#Remove rows with double family/order names
data_nc6 <- data_nc6 %>% filter(is.na(item_family) | item_family != "Palinuridae/Scyllaridae")
data_nc6 <- data_nc6 %>% filter(is.na(item_ord) | item_ord!="Sepiida/Teuthida")

data_nc6 %>% filter(item_ord=="Sepiida/Teuthida")

#Noticed Sipuncula as Annelida....

data_nc6 %>% filter(item_raw=="Sipuncula") %>% select(item_class, item_phylum)

data_nc6 <- data_nc6 %>% mutate(item_class = replace(item_class, item_raw=="Sipuncula", NA)) 

data_nc6 <-data_nc6 %>% mutate(item_phylum = replace(item_phylum, item_raw=="Sipuncula", "Sipuncula")) 


data_nc6 %>% filter(item_raw=="Stomatopoda") %>% select(item_ord, item_class, item_phylum) 

data_nc6 <-data_nc6 %>% mutate(item_ord = replace(item_ord, item_raw=="Stomatopoda", "Stomatopoda"))

 #7. Export clean data
write.csv(data_nc6, "data/data_clean_nc.csv")

                                  


