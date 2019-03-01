setwd("D:/Work, Courses/EPHE/Stage_Coral reef food web CRIOBE/Data/Check lists")
getwd()
dir()

library("xlsx")
check<-read.xlsx2("Checklist_Chloe.xlsx", sheetIndex=1, header=T)
head(check)

trait<-read.xlsx2("Traits_Gaspar.xlsx", sheetIndex=1, header=T)
head(trait)
trait<-trait[-1,]

?merge

#nc<-read.xlsx2("Data_NC.xlsx", header=T, sheetIndex=1) #take too much memory to open

check_data_full<-merge(check, trait, by="Gaspar_code", all.x=TRUE) #Check data
rm(check,trait)

length(intersect(check$Gaspar_code, trait$Gaspar_code)) #2669 sp in the check list with diet and sizeclass
length(unique(check$Gaspar_code)) #2669 sp in the check list
length(unique(trait$Gaspar_code)) #6316 sp in trait sp list

                          #Get check data ready#
library(tidyverse)
check_data1 <- check_data_full %>% select(Site_code,Family.x,Species.x,Diets, Size.Class)
check_data<-dplyr::filter(check_data1, Site_code %in% c("haw","mads","mari","vir")) #Check data with lignes corresponding to those site codes
levels(check_data$Site_code) 
unique(check_data$Site_code) 

rm(check_data_full) ; rm(check_data1)

summary(check_data1$Site_code)
unique(summary(check_data$Site_code)) #SOLVED with dplyr :: filter() - WHY diff nb of occurence per site after subsetting ??? Subset whas the pb 

check_data %>% 
  count(Site_code)

names(check_data)[2] <- "family"
names(check_data)[3] <- "spcheck"
levels(check_data$Site_code)[6] <-"mad" #mads as mad to match with the diet data

length(unique(check_data$spcheck)) #1959 species
length(unique(check_data$family)) #118 families
length(unique(check_data$Size.Class))
levels(check_data$Diets)
levels(check_data$Size.Class)

#diet_sc<-unique(check_data %>% select(spcheck,Diets, Size.Class)) #to have only one obs per species


                              #Get diet data ready !

data<-read.xlsx2("Dietdata.xlsx", sheetIndex = 3, header=T) #8739 in excel so more rows !!!

length(unique(data$spname)) # 615
length(unique(data$acc_name)) #597

#data$acc_name<-trimws(data$acc_name, which=c("both")) #to remove whitespace at the end
#data$acc_name<-gsub(" ", "_",data$acc_name, fixed=T) #as some species have alreay a space
#data$acc_name<-gsub("_ ", "_",data$acc_name, fixed=T)
#data$acc_name<-gsub(" _ ", "_",data$acc_name, fixed=T)
#data$acc_name<-gsub("__", "_",data$acc_name, fixed=T)

data$acc_name<-gsub("_", " ",data$acc_name, fixed=T) #replacing the _ by whitespace

#data$family<-trimws(data$family, which=c("right")) #doesn't work

library(stringr)
data$family<-as.character(data$family)

data$family<- str_trim(data$family, side="right")
 
print(unique(data$family))


#data$sci_name<-paste(data$genus, data$sp, sep="_") #reconstructing sp name variables
#data$spname<-paste(data$genus, data$sp, sep=" ")

#data$sci_name<-gsub(" ", "",data$sci_name, fixed=T)
#data$spname<-gsub("_", " ",data$sci_name, fixed=T)

#data[data$acc_name=="Stegastes fuscus",] #yep no space...SOLVED
#data[data$genus=="Zanclus",] #present
#data[data$acc_name=="Zanclus cornutus",]  #SOLVED - absent but is in the excel sheet ...PB is sometimes space after sp name!! !!

#data[data$acc_name=="Coris gaimard",]
#data[data$sp=="fulviflamma",]
#full_data[full_data$acc_name=="Lutjanus fulviflamma",] # SOLVED - PB two spaces

length(intersect(data$acc_name, check_data$spcheck)) #459 species matches
length(unique(data$acc_name)) #597
length(unique(check_data$spcheck))  #1959


#Loop if else to have site code according to each site name
summary(data$country)
summary(check_data$Site_code)

for( i in 1:length(data$country)){
  if(data$country[i] == "Hawai"){
    data$site_code[i] = "haw"
  } else if (data$country[i] == "Madagascar"){
    data$site_code[i] = "mad"
  } else if (data$country[i] == "Marsh"){
    data$site_code[i] = "mari"
  } else {
    data$site_code[i] = "vir"
  }
}

unique(data$country)
unique(data$site_code)

#Nb of species per site
aggregate(spname ~ site_code, data, function(x) length(unique(x)))
aggregate(acc_name ~ site_code, data, function(x) length(unique(x)))
aggregate(spcheck ~ Site_code, check_data, function(x) length(unique(x)))

?intersect

####Check sp name with the R Fishbase package####
library(rfishbase)

#Check for valid names
fish_val<-validate_names(data$spname) #get vectors of sp name recognized by FishBase
length(unique(fish_val)) #554 sp
length(unique(data$spname)) #615 single sp

fish_val_acc<-validate_names(data$acc_name)
length(unique(fish_val_acc)) #556 sp
length(unique(data$acc_name)) #597 sp 

fish_check<-validate_names(check_data$spcheck) 
length(unique(fish_check)) # 1939 sp so 20 species not in the fishbase
length(unique(check_data$spcheck))  #1959 sp

length(intersect(fish_val, fish_check)) #only 502 matches
length(intersect(fish_val_acc, fish_check)) #501 matches

#Synonyms tab
#Original sp name
fish_syn<-synonyms(species_list = data$spname, server=NULL) #check for alternative version of scientific names
#Chlo? accepted names
fish_syn_acc<-synonyms(species_list = data$acc_name, server=NULL)
length(unique(fish_syn_acc$synonym)) #596

fish_check_syn<-synonyms(species_list = check_data$spcheck, server=NULL)
fish_check_acc<-subset(fish_check_syn$Species, fish_check_syn$Status=="accepted name" | fish_check_syn$Status=="synonym") #accepted or synonym
length(unique(fish_check_acc)) #1939 as validate_names(fish_check)


#Select species for which status accepted or synonym
fish_acc<-subset(fish_syn$Species, fish_syn$Status=="accepted name" | fish_syn$Status=="synonym")
length(unique(fish_acc)) #554 sp which what get validate_names(fish)

fish_acc2<-subset(fish_syn_acc$Species, fish_syn$Status=="accepted name" | fish_syn$Status=="synonym")
length(unique(fish_acc2)) #657 sp which more than with validate_names more than 596 coz for some species both accepted and synonym names

#Check for nb of match between check list accepted names and mine
length(intersect(fish_acc,fish_acc2)) #519 matches
length(intersect(fish_check_acc,fish_acc)) #only 502 matches 615
length(intersect(fish_check_acc,fish_acc2)) #only 532 matches 596


                             ####Nina fishbase####
#Diet data
library(rfishbase)
class(fish_sp)
fish_sp<-unique(data$acc_name) 
sp_correct<-suppressWarnings(rfishbase::validate_names(fish_sp))
sp_errors<-fish_sp[!fish_sp %in% sp_correct]

length(unique(sp_correct)) #557
length(unique(sp_errors)) #83
length(unique(fish_sp)) #597

print(sp_errors)
print(sp_correct)

corrected<-c("Synodus synodus","Platybelone argalus","Tylosurus acus",
             "Heteropriacanthus cruentatus","Pempheris schomburgkii","Haemulon vittatum",              
             "Haemulon plumierii","Diplodus caudimacula","Pareques acuminatus",
             "Oligoplites saurus","Halichoeres maculipinna","Scarus iseri",            
             "Sparisoma viride","Gobiosoma sp","Gobioclinus guppyi",
             "Opistognathus aurifrons","Opistognathus macrognathus","Opistognathus maxillosus" ,   
             "Opistognathus whitehursti","Chaetodipterus faber","Aluterus schoepfii",
             "Lactophrys triqueter","Sphoeroides spengleri","Antennarius striatus","Gymnothorax eurostus",
             "Parupeneus forsskali","Parupeneus trifasciatus","Plectroglyphidodon imparipennis",       
             "Mobula alfredi","Brachysomophis crocodilinus","Gymnomuraena zebra",
             "Gymnothorax sp","Conger cinereus","Hyporhamphus dussumieri",        
             "Myripristis sp","Holocentrus sp","Neomyxus chaptalii",
             "Gymnosarda unicolor", "Trachinotus baillonii","Caranx ignobilis",             
             "Elagatis bipinnulata","Apogon novaeguineae","Apogon sp",
             "Cephalopholis urodeta","Plectropomus areolatus","Gerres argyreus",              
             "Siganus argenteus","Zebrasoma veliferum",          
"Scorpaenodes sp",               "Scorpaenopsis gibbosa",         
"Caracanthus unipinna",         "Thysanophrys sp",              
"Plectroglyphidodon leucozonus",          "Chrysiptera brownriggii",           
"Stethojulis sp",                "Macropharyngodon meleagris",    
"Thalassoma hardwicke",          "Calotomus spinidens",        
"Scarus sp",                     "Parapercis cephalopunctata",   
"Monacanthus sp" ,               "Arothron sp",                  
"Chrysiptera brownriggii",         "Sargocentron diadema",        
"Archamia fucata",               "Asterropteryx semipunctata",   
"Canthigaster margaritata",     "Chaetodon blackburnii",         
"Oxycheilinus bimaculatus",         "Chromis weberi",             
"Epinephelus longispinis",         "Sufflamen chrysopterum",        
"Myripristis murdjan",        "Novaculoides macrolepidotus",
"Cheilodipterus quinquelineatus", "Parupeneus barberinus",       
"Parupeneus macronemus",          "Pervagor melanocephalus",      
"Cociella crocodilus",            "Pomacentrus caeruleus",     
"Sebastapistes cyanostigma",       "Sebastapistes strongia",        
"Hippichthys cyanospilos") 


 ## Replace the errors
data$fish_sp <- data$acc_name
data$fish_sp[data$fish_sp%in%sp_errors] <- sapply(data$fish_sp[data$fish_sp%in%sp_errors],FUN = function(x){
line <- which(sp_errors==x)
y <- gsub(sp_errors[sp_errors==x],corrected[line],x = x)
print(paste("correcting ",x))
return(y)})

subset(data,is.na(data$fish_sp)) #no Na 
length(unique(data$fish_sp)) #583 accepted species name

aggregate(fish_sp ~ site_code, data, function(x) length(unique(x))) #after correction same nb of sp exepet for Mari where 3 species less
  
#Check data
check_data$spcheck<-as.character(check_data$spcheck)
check_sp<-unique(check_data$spcheck)
check_correct<-suppressWarnings(rfishbase::validate_names(check_sp))
check_errors<-check_sp[!check_sp %in% check_correct] 
check_errors<-as.character(check_errors)
class(check_sp)
class(check_errors)
length(unique(check_errors)) #121

print(check_errors)


check_corrected<-c("Alectis indica",                      
"Ostorhinchus apogonoides",                     
"Centropyge interrupta",                
"Lactophrys triqueter",                  
"Antennatus analis",                    
"Roa modesta",                          
"Hyporthodus mystacinus",                
"Tigrigobius multifasciatus",             
"Pomacentrus trichrourus",                
"Apogonichthyoides timorensis",                     
"Zebrasoma veliferum",                   
"Apogonichthyoides taeniatus",                      
"Pseudanthias ventralis",                
"Dunckerocampus baldwini",                 
"Sunagocia arenicola",                
"Sunagocia otaitensis",               
"Upeneus taeniopterus",                          
"Psilotris celsa",                      
"Elacatinus gemmatus",                   
"Elacatinus pallens",                    
"Elacatinus dilepis",                    
"Carapus boraborensis",                  
"Ostorhinchus flagelliferus",                  
"Amoya signata",                  
"Ariosoma fasciatum",               
"Limnichthys nitidus",                
"Atrosalarias fuscus",             
"Lutjanus russellii",                     
"Caranx ruber",                     
"Scorpaenodes evides",               
"Xyrichtys woodi",                       
"Iniistius melanopus",                   
"Archamia mozambiquensis",               
"Zalanthias kelloggi",                
"Kyphosus sectatrix",                     
"Opua atherinoides",                     
"Trachinocephalus myops",                
"Antennatus duescus",                   
"Mugilogobius parvus",                   
"Amphelikturus dendriticus",                
"Apogon affinis",                        
"Ulaema lefroyi",                  
"Haemulon vittatum",                       
"Sectator ocyurus",                      
"Ostorhinchus cyanosoma",                      
"Ostorhinchus holotaenia",                     
"Planiliza macrolepis",                       
"Emblemariopsis signifer",              
"Ostorhinchus nigrofasciatus",                 
"Ostorhinchus aureus",                         
"Ostorhinchus fleurieu",                       
"Pseudogramma astigma",                 
"Synchiropus corallinus",           
"Pseudanthias ventralishawaiiensis",     
"Gymnothorax rueppelliae",                
"Antennatus coccineus",                 
"Antennatus nummifer",                  
"Oxycheilinus celebicus",                 
"Novaculoides macrolepidotus",         
"Alionematichthys minyomma",               
"Ostorhinchus gularis",                        
"Jaydia hungi",                          
"Ellochelon vaigiensis",                       
"Osteomugil engeli",                      
"Moolgarda seheli",                      
"Centropyge fisheri",                 
"Paracentropyge multifasciata",              
"Verulux cypselurus",                   
"Pristicon trimaculatus",                   
"Zapogon evermanni",                      
"Pristiapogon taeniopterus",                   
"Pristiapogon exostigma",                      
"Pristiapogon fraenatus",                      
"Pristiapogon kallopterus",                    
"Nectamia fusca",                      
"Ostorhinchus angustatus",                     
"Ostorhinchus taeniophorus",                   
"Ostorhinchus novemfasciatus",                 
"Archamia fucata",                       
"Siphamia tubifer",                   
"Elacatinus saucrus",                    
"Gerres argyreus",                       
"Jaydia truncata",                      
"Scolopsis bimaculata",                 
"Archamia macroptera",                   
"Jaydia smithi",                         
"Scorpaenodes evides",                  
"Dunckerocampus dactyliophorus",           
"Apogonichthyoides pharaonis",                      
"Synodus dermatogenys",                    
"Synodus janus",                         
"Albula argentea",                       
"Archamia flavofasciata",                
"Hyporthodus quernus",                   
"Paragobiodon xanthosoma",              
"Koumansetta hectori",                   
"Antennatus rosaceus",                  
"Pseudogramma polyacantha",             
"Cheilodactylus vittatus",                   
"Ostorhinchus maculiferus",                    
"Macropharyngodon bipartitusbipartitus",
"Centropyge loricula",                   
"Neosynchiropus ocellatus",                 
"Apogon queketti",                       
"Iniistius pavo",                       
"Helcogramma capidata",                 
"Ostorhinchus cookii",                         
"Apogon semilineatus",                   
"Platybelone argalus",                   
"Tylosurus acus",                        
"Canthigaster axiologus",                  
"Monotaxis heterodon",                   
"Thamnaconus garretti",                  
"Kyphosus sandwicensis",                 
"Acanthurus nigroris",                     
"Eviota asymbasia",                      
"Eviota diyhritisma",                    
"Grammonus nagaredai",                    
"Glossogobius sandakanensis",            
"Xyrichtys halsteadi",                   
"Pseudanthias hawaiiensis")

## Replace the errors
check_data$check_sp<- check_data$spcheck
check_data$check_sp[check_data$check_sp %in% check_errors] <- sapply(check_data$check_sp[check_data$check_sp%in%check_errors],FUN = function(x){
  line <- which(check_errors==x)
  y <- gsub(check_errors[check_errors==x],check_corrected[line],x = x)
  print(paste("correcting ",x))
  return(y)})

subset(check_data,is.na(check_data$check_sp)) # no NA 

###so all species with accepted names yeah !!

length(unique(check_data$check_sp)) #1951 sp - 1959 after correction
length(unique(data$fish_sp)) # 583 sp 596 before correction so 13 species for which same names ?!

length(intersect(data$fish_sp, check_data$check_sp)) #511 sp in common 
length(intersect(data$genus_cor, check_data$genus_cor)) #236 genus
length(intersect(data$family, check_data$family)) #69 families in common

no_sp<-setdiff(data$fish_sp, check_data$check_sp) #gives a vector of species not shared
length(no_sp) #72 species no match

setdiff(data$family, check_data$family) #15 families not in check list

##Replace family errors
data$family_cor<-data$family
fam_errors<-setdiff(data$family, check_data$family)
print(fam_errors)
fam_cor<-c("Ginglymostomatidae", "Carcharhinidae"  ,   "Dasyatidae" ,       
           "Mylobatidae"    ,    "Clupeidae"  ,   "Haemulidae"   ,
           "Malacanthidae",   "Pomacentridae"   ,  "Clinidae"  ,        
           "Tetraodontidae" ,  "Carcharhinidae"      ,    "Mobulinae" ,        
           "Cirrhitidae"     ,    "Pinguipedidae"    ,   "Tripterygiidae")

print(fam_errors)
data$family_cor[data$family_cor %in% fam_errors] <- sapply(data$family_cor[data$family_cor%in%fam_errors],FUN = function(x){
  line <- which(fam_errors==x)
  y <- gsub(fam_errors[fam_errors==x],fam_cor[line],x = x)
  print(paste("correcting ",x))
  return(y)})

setdiff(data$family_cor, check_data$family)


####Sp, genus and family####

library(tidyr)

#Cr?er variable "Genus" et s?parer "genus et "sp"
check_data$Genus<- check_data$spcheck
check_data<- check_data %>% separate(Genus, c("genus", "sp"), sep=" ")

data$genus_cor<-data$fish_sp
data <- data %>% separate(genus_cor, c("genus_cor", "sp_cor"), sep=" ")

check_data$genus_cor<-check_data$check_sp
check_data <- check_data %>% separate(genus_cor, c("genus_cor", "sp_cor"), sep=" ")

#Export clean dataset
write.xlsx2(data, file="Dietdata_corrected.xlsx", sheetName = "Sheet1",
            col.names = TRUE, row.names = TRUE, append = FALSE)

write.xlsx2(check_data, file="Checklist_corrected.xlsx", sheetName = "Sheet1",
            col.names = TRUE, row.names = TRUE, append = FALSE)

#Data frame nb occurence
tot<-sapply(data[,c(4,38,5,39,8,9,37)], function(x) length(unique(x)))
mad<-sapply(data[,c(4,38,5,39,8,9,37)], function(x) length(unique(x[data$site_code=="mad"])))
haw<-sapply(data[,c(4,38,5,39,8,9,37)], function(x) length(unique(x[data$site_code=="haw"])))
mari<-sapply(data[,c(4,38,5,39,8,9,37)], function(x) length(unique(x[data$site_code=="mari"])))
vir<-sapply(data[,c(4,38,5,39,8,9,37)], function(x) length(unique(x[data$site_code=="vir"])))# nb occurence data sp, gen, fam

tot.check<-sapply(check_data[,c(2,7,9,3,6)], function(x) length(unique(x)))
mad.check<-sapply(check_data[,c(2,7,9,3,6)], function(x) length(unique(x[check_data$Site_code=="mad"])))
haw.check<-sapply(check_data[,c(2,7,9,3,6)], function(x) length(unique(x[check_data$Site_code=="haw"])))
mari.check<-sapply(check_data[,c(2,7,9,3,6)], function(x) length(unique(x[check_data$Site_code=="mari"])))
vir.check<-sapply(check_data[,c(2,6,9,3,8)], function(x) length(unique(x[check_data$Site_code=="vir"])))

data_sum<-data.frame(tot,mad, haw, mari, vir)  #spname and acc_name diff as some spname have the same acc_name
check_sum<-data.frame(tot.check,mad.check, haw.check, mari.check, vir.check)

a<-c(length(intersect(data$family_cor[data$site_code=="mad"], check_data$family[check_data$Site_code=="mad"])),
     length(intersect(data$genus_cor[data$site_code=="mad"], check_data$genus_cor[check_data$Site_code=="mad"])),
     length(intersect(data$fish_sp[data$site_code=="mad"], check_data$check_sp[check_data$Site_code=="mad"])))
b<-c(length(intersect(data$family_cor[data$site_code=="haw"], check_data$family[check_data$Site_code=="haw"])),
     length(intersect(data$genus_cor[data$site_code=="haw"], check_data$genus_cor[check_data$Site_code=="haw"])),
     length(intersect(data$fish_sp[data$site_code=="haw"], check_data$check_sp[check_data$Site_code=="haw"])))
c<-c(length(intersect(data$family_cor[data$site_code=="mari"], check_data$family[check_data$Site_code=="mari"])),
     length(intersect(data$genus_cor[data$site_code=="mari"], check_data$genus_cor[check_data$Site_code=="mari"])),
     length(intersect(data$fish_sp[data$site_code=="mari"], check_data$check_sp[check_data$Site_code=="mari"])))
d<-c(length(intersect(data$family_cor[data$site_code=="vir"], check_data$family[check_data$Site_code=="vir"])),
     length(intersect(data$genus_cor[data$site_code=="vir"], check_data$genus_cor[check_data$Site_code=="vir"])),
     length(intersect(data$fish_sp[data$site_code=="vir"], check_data$check_sp[check_data$Site_code=="vir"])))

e<-c(length(intersect(data$family_cor, check_data$family)),
     length(intersect(data$genus_cor, check_data$genus_cor)),
     length(intersect(data$fish_sp, check_data$check_sp)))

com<-cbind(e,a,b,c,d) #matrix nb of fam, genus and sp in common with check list
com<-as.matrix(com)
rownames(com)<-c("family_cor", "genus_cor", "species_cor")
colnames(com)<-c("tot","mad","haw","mari","vir")

com

com_per<-com*100/data_sum[c(2,4,7),]
com_per<-as.matrix(com_per)
class(com_per)

check_per<-com*100/check_sum[c(1,3,5),]
check_per<-as.matrix(check_per)

library("RColorBrewer")
par(mar=c(6.1, 5.1, 3.1, 8.1), xpd=TRUE)

#plot % in common with check data
barplot(check_per, col=brewer.pal(n = 3, name = "Greys"),
        cex.names=1.5, cex.axis=1.5, cex.lab=1.5, beside=TRUE, ylim=c(0,100),
        main="Percentage of the data in common with check data")

legend("topright", inset=c(-0.22,-0.05), legend=rownames(check_per), 
       col=brewer.pal(n = 3, name = "Greys"), pch=15, pt.cex=3, bty="n")

#plot % in common of the data
barplot(com_per, col=brewer.pal(n = 3, name = "Greys"),
        cex.names=1.5, cex.axis=1.5, cex.lab=1.5, beside=TRUE, ylim=c(0,100),
        main="Percentage of the data that is in common")

legend("topright", inset=c(-0.27,-0.1), legend=rownames(com_per), 
       col=brewer.pal(n = 3, name = "Greys"), pch=15, pt.cex=3, bty="n")



?barplot

#% ramen? sur 100% pour stalked barplot
per_sum<-data_sum/check_sum*100
per_sum<-as.matrix(per_sum)

class(per_sum)

per_fam<-per_sum[1,]
per_gen<-per_sum[2,]
per_sp<-per_sum[3,]

par(mfrow=c(1,1))
barplot(per_fam, ylab="Percentage of family", cex.names=2.5, cex.axis=2, cex.lab=2)
barplot(per_gen, ylab="Percentage of genus", ylim=c(0,60),cex.names=2.5, cex.axis=2, cex.lab=2)
barplot(per_sp, ylab="Percentage of species", ylim=c(0,60), cex.names=2.5, cex.axis=2, cex.lab=2)



                            ####Nb of species per family#####

#Madagascar
mad_fam<-aggregate(data$fish_sp[data$site_code=="mad"] ~ data$family[data$site_code=="mad"], data, function(x) length(unique(x)))
mad_fam_check<-aggregate(check_data$check_sp[check_data$Site_code=="mad"] ~ check_data$family[check_data$Site_code=="mad"], check_data, function(x) length(unique(x)))

mad_fam<-mad_fam[order(mad_fam[,2], decreasing=T),]
names(mad_fam)=c("Family", "Nb_sp")

par(mar=c(8,4.2,3,1))
barplot(mad_fam$Nb_sp, names=mad_fam$Family, las=2, ylab="Number of species", main="Madagascar", cex.axis=1, cex.lab=1)

mad_fam_check<-mad_fam_check[order(mad_fam_check[,2], decreasing=T),]
names(mad_fam_check)=c("Family", "Nb_sp_check")
barplot(mad_fam_check$Nb_sp, names=mad_fam_check$Family, las=2, ylab="Number of species", main="Madagascar check",cex.names=0.7)


mad_fam$rank<-rank(-mad_fam$Nb_sp, ties.method='min') #assign every element to the lowest rank (-x) so start by the lowest
mad_fam_check$rank_check<-rank(-mad_fam_check$Nb_sp_check, ties.method="min")

mad_rank<-merge(mad_fam , mad_fam_check , by='Family', all=T)

cor.test(mad_rank$rank , mad_rank$rank_check, method="spearman")

plot(mad_rank$rank ~ mad_rank$rank_check , xlab="Rank check", ylab="Rank",
     main="Madagascar Family", cex.lab=1.5, cex=2, cex.axis=1.5, pch=20)
abline(model)
model<-lm(rank~rank_check,mad_rank)

#Hawai
haw_fam<-aggregate(data$fish_sp[data$site_code=="haw"] ~ data$family[data$site_code=="haw"], data, function(x) length(unique(x)))
haw_fam_check<-aggregate(check_data$check_sp[check_data$Site_code=="haw"] ~ check_data$family[check_data$Site_code=="haw"], check_data, function(x) length(unique(x)))

haw_fam<-haw_fam[order(haw_fam[,2], decreasing=T),]
names(haw_fam)=c("Family", "Nb_sp")
barplot(haw_fam$Nb_sp, names=haw_fam$Family, las=2, ylab="Number of species", main="Hawa?", cex.axis=1, cex.lab=1)

haw_fam_check<-haw_fam_check[order(haw_fam_check[,2], decreasing=T),]
names(haw_fam_check)=c("Family", "Nb_sp_check")
barplot(haw_fam_check$Nb_sp_check, names=haw_fam_check$Family, las=2, ylab="Number of species", main="Hawa? check",cex.names=0.7)

haw_fam$rank<-rank(-haw_fam$Nb_sp, ties.method='min') #assign every element to the lowest rank (-x) so start by the lowest
haw_fam_check$rank_check<-rank(-haw_fam_check$Nb_sp_check, ties.method="min")

haw_rank<-merge(haw_fam , haw_fam_check , by='Family', all=T)

cor.test(haw_rank$rank , haw_rank$rank_check, method="spearman")

plot(haw_rank$rank ~ haw_rank$rank_check , xlab="Rank check", ylab="Rank",
     main="Hawai Family", cex.lab=1.5, cex=2, cex.axis=1.5, pch=20)
abline(model)
model<-lm(rank~rank_check,haw_rank)


#Marshall islands
mari_fam<-aggregate(data$fish_sp[data$site_code=="mari"] ~ data$family[data$site_code=="mari"], data, function(x) length(unique(x)))
mari_fam_check<-aggregate(check_data$check_sp[check_data$Site_code=="mari"] ~ check_data$family[check_data$Site_code=="mari"], check_data, function(x) length(unique(x)))

mari_fam<-mari_fam[order(mari_fam[,2], decreasing=T),]
names(mari_fam)=c("Family", "Nb_sp")
barplot(mari_fam$Nb_sp, names=mari_fam$Family, las=2, ylab="Number of species", main="Marshall Islands", cex.axis=1, cex.lab=1, ylim=c(0,25))

mari_fam_check<-mari_fam_check[order(mari_fam_check[,2], decreasing=T),]
names(mari_fam_check)=c("Family", "Nb_sp_check")
barplot(mari_fam_check$Nb_sp_check, names=mari_fam_check$Family, las=2, ylab="Number of species", main="Marshall Islands check",cex.names=0.7)

mari_fam$rank<-rank(-mari_fam$Nb_sp, ties.method='min') #assign every element to the lowest rank (-x) so start by the lowest
mari_fam_check$rank_check<-rank(-mari_fam_check$Nb_sp_check, ties.method="min")

mari_rank<-merge(mari_fam , mari_fam_check , by='Family', all=T)

cor.test(mari_rank$rank , mari_rank$rank_check, method="spearman")

plot(mari_rank$rank ~ mari_rank$rank_check , xlab="Rank check", ylab="Rank",
     main="Marshall Islands Family", cex.lab=1.5, cex=2, cex.axis=1.5, pch=20)



#Virgin islands
vir_fam<-aggregate(data$fish_sp[data$site_code=="vir"] ~ data$family[data$site_code=="vir"], data, function(x) length(unique(x)))
vir_fam_check<-aggregate(check_data$check_sp[check_data$Site_code=="vir"] ~ check_data$family[check_data$Site_code=="vir"], check_data, function(x) length(unique(x)))

vir_fam<-vir_fam[order(vir_fam[,2], decreasing=T),]
names(vir_fam)=c("Family", "Nb_sp")
barplot(vir_fam$Nb_sp, names=vir_fam$Family, las=2, ylab="Number of species", main="Virgin Islands", cex.axis=1, cex.lab=1)

vir_fam_check<-vir_fam_check[order(vir_fam_check[,2], decreasing=T),]
names(vir_fam_check)=c("Family", "Nb_sp_check")
barplot(vir_fam_check$Nb_sp_check, names=vir_fam_check$Family, las=2, ylab="Number of species", main="Virgin Islands check",cex.names=0.7)

vir_fam$rank<-rank(-vir_fam$Nb_sp, ties.method='min') #assign every element to the lowest rank (-x) so start by the lowest
vir_fam_check$rank_check<-rank(-vir_fam_check$Nb_sp_check, ties.method="min")

vir_rank<-merge(vir_fam , vir_fam_check , by='Family', all=T)

cor.test(vir_rank$rank , vir_rank$rank_check, method="spearman")

plot(vir_rank$rank ~ vir_rank$rank_check , xlab="Rank check", ylab="Rank",
     main="Virgin Islands Family", cex.lab=1.5, cex=2, cex.axis=1.5, pch=20)


#Merging data with data of size and diets
full_data <- merge(data, check_data, by.x="fish_sp", by.y="check_sp", all.x=T) #Merge datasetsfull_data<- data %>% left_join(select (check_data, "check_sp","Diets", "Size.Class"), by=c("check_sp"="fish_sp"))
names(full_data)[5]<-"family"
full_data<-full_data[,-c(38:40)]
length(unique(full_data$fish_sp)) #583

pb_sp<-subset(full_data$fish_sp, is.na(full_data$Diets))
length(unique(pb_sp)) #74 species with no diet data


                            #####Size Class with rank####
#Mada
mad_size<-aggregate(fish_sp[full_data$site_code=="mad"] ~ Size.Class[full_data$site_code=="mad"] + family[full_data$site_code=="mad"], full_data, function(x) length(unique(x)))
mad_size
names(mad_size)<-c("Size.Class","Family","Nb_sp")
mad_size$rank<-rank(-mad_size$Nb_sp, ties.method="min")
?rank

mad_size_check<-aggregate(cbind(check_sp[check_data$Site_code=="mad"]) ~ Size.Class[check_data$Site_code=="mad"] + family[check_data$Site_code=="mad"], check_data, function(x) length(unique(x)))
names(mad_size_check)<-c("Size.Class","Family","Nb_sp_check")
mad_size_check$rank_check<-rank(-mad_size_check$Nb_sp_check, ties.method="min")

size_rank<-merge(mad_size , mad_size_check, by=c("Family","Size.Class") , all=T)

plot(size_rank$rank_check , size_rank$rank, xlab="Rank check", ylab="Rank",
     main="Madagascar Family x SizeClass", cex.lab=1.5, cex=2, cex.axis=1.5, pch=20)

cor.test(size_rank$rank , size_rank$rank_check, method="spearman")

#Hawai
haw_size<-aggregate(fish_sp[full_data$site_code=="haw"] ~ Size.Class[full_data$site_code=="haw"] + family[full_data$site_code=="haw"], full_data, function(x) length(unique(x)))
haw_size
names(haw_size)<-c("Size.Class","Family","Nb_sp")
haw_size$rank<-rank(-haw_size$Nb_sp, ties.method="min")

haw_size_check<-aggregate(cbind(check_sp[check_data$Site_code=="haw"]) ~ Size.Class[check_data$Site_code=="haw"] + family[check_data$Site_code=="haw"], check_data, function(x) length(unique(x)))
names(haw_size_check)<-c("Size.Class","Family","Nb_sp_check")
haw_size_check$rank_check<-rank(-haw_size_check$Nb_sp_check, ties.method="min")

haw_size_rank<-merge(haw_size , haw_size_check, by=c("Family","Size.Class") , all=T)

plot(haw_size_rank$rank_check , haw_size_rank$rank, xlab="Rank check", ylab="Rank",
     main="Hawai Family x SizeClass", cex.lab=1.5, cex=2, cex.axis=1.5, pch=20)

cor.test(haw_size_rank$rank , haw_size_rank$rank_check, method="spearman")

#Marshall 
mari_size<-aggregate(fish_sp[full_data$site_code=="mari"] ~ Size.Class[full_data$site_code=="mari"] + family[full_data$site_code=="mari"], full_data, function(x) length(unique(x)))
mari_size
names(mari_size)<-c("Size.Class","Family","Nb_sp")
mari_size$rank<-rank(-mari_size$Nb_sp, ties.method="min")

mari_size_check<-aggregate(cbind(check_sp[check_data$Site_code=="mari"]) ~ Size.Class[check_data$Site_code=="mari"] + family[check_data$Site_code=="mari"], check_data, function(x) length(unique(x)))
names(mari_size_check)<-c("Size.Class","Family","Nb_sp_check")
mari_size_check$rank_check<-rank(-mari_size_check$Nb_sp_check, ties.method="min")

mari_size_rank<-merge(mari_size , mari_size_check, by=c("Family","Size.Class") , all=T)

plot(mari_size_rank$rank_check , mari_size_rank$rank, xlab="Rank check", ylab="Rank",
     main="Marshall Isl Family x SizeClass", cex.lab=1.5, cex=2, cex.axis=1.5, pch=20)

cor.test(mari_size_rank$rank , mari_size_rank$rank_check, method="spearman")


#Virgin islands
vir_size<-aggregate(fish_sp[full_data$site_code=="vir"] ~ Size.Class[full_data$site_code=="vir"] + family[full_data$site_code=="vir"], full_data, function(x) length(unique(x)))
vir_size
names(vir_size)<-c("Size.Class","Family","Nb_sp")
vir_size$rank<-rank(-vir_size$Nb_sp, ties.method="min")

vir_size_check<-aggregate(cbind(check_sp[check_data$Site_code=="vir"]) ~ Size.Class[check_data$Site_code=="vir"] + family[check_data$Site_code=="vir"], check_data, function(x) length(unique(x)))
names(vir_size_check)<-c("Size.Class","Family","Nb_sp_check")
vir_size_check$rank_check<-rank(-vir_size_check$Nb_sp_check, ties.method="min")

vir_size_rank<-merge(vir_size , vir_size_check, by=c("Family","Size.Class") , all=T)

plot(vir_size_rank$rank_check , vir_size_rank$rank, xlab="Rank check", ylab="Rank",
     main="Virgin Isl Family x SizeClass", cex.lab=1.5, cex=2, cex.axis=1.5, pch=20)

cor.test(vir_size_rank$rank , vir_size_rank$rank_check, method="spearman")



                            ####Diet with rank####

#Mada
mad_diet<-aggregate(fish_sp[full_data$site_code=="mad"] ~ Diets[full_data$site_code=="mad"] + family[full_data$site_code=="mad"], full_data, function(x) length(unique(x)))
mad_diet
names(mad_diet)<-c("Diets","Family","Nb_sp")
mad_diet$rank<-rank(-mad_diet$Nb_sp, ties.method="min")

mad_diet_check<-aggregate(cbind(check_sp[check_data$Site_code=="mad"]) ~ Diets[check_data$Site_code=="mad"] + family[check_data$Site_code=="mad"], check_data, function(x) length(unique(x)))
names(mad_diet_check)<-c("Diets","Family","Nb_sp_check")
mad_diet_check$rank_check<-rank(-mad_diet_check$Nb_sp_check, ties.method="min")

mad_diet_rank<-merge(mad_diet , mad_diet_check, by=c("Family","Diet") , all=T)

plot(mad_diet_rank$rank_check , mad_diet_rank$rank, xlab="Rank check", ylab="Rank",
     main="Madagascar Family x Diet", cex.lab=1.5, cex=2, cex.axis=1.5, pch=20)

cor.test(mad_diet_rank$rank , mad_diet_rank$rank_check, method="spearman")

#Hawai
haw_diet<-aggregate(fish_sp[full_data$site_code=="haw"] ~ Diets[full_data$site_code=="haw"] + family[full_data$site_code=="haw"], full_data, function(x) length(unique(x)))
haw_diet
names(haw_diet)<-c("Diets","Family","Nb_sp")
haw_diet$rank<-rank(-haw_diet$Nb_sp, ties.method="min")

haw_diet_check<-aggregate(cbind(check_sp[check_data$Site_code=="haw"]) ~ Diets[check_data$Site_code=="haw"] + family[check_data$Site_code=="haw"], check_data, function(x) length(unique(x)))
names(haw_diet_check)<-c("Diets","Family","Nb_sp_check")
haw_diet_check$rank_check<-rank(-haw_diet_check$Nb_sp_check, ties.method="min")

haw_diet_rank<-merge(haw_diet , haw_diet_check, by=c("Family","Diets") , all=T)

plot(haw_diet_rank$rank_check , haw_diet_rank$rank, xlab="Rank check", ylab="Rank",
     main="Hawai Family x Diet", cex.lab=1.5, cex=2, cex.axis=1.5, pch=20)

cor.test(haw_diet_rank$rank , haw_diet_rank$rank_check, method="spearman")

#Marshall 
mari_diet<-aggregate(fish_sp[full_data$site_code=="mari"] ~ Diets[full_data$site_code=="mari"] + family[full_data$site_code=="mari"], full_data, function(x) length(unique(x)))
mari_diet
names(mari_diet)<-c("Diets","Family","Nb_sp")
mari_diet$rank<-rank(-mari_diet$Nb_sp, ties.method="min")

mari_diet_check<-aggregate(cbind(check_sp[check_data$Site_code=="mari"]) ~ Diets[check_data$Site_code=="mari"] + family[check_data$Site_code=="mari"], check_data, function(x) length(unique(x)))
names(mari_diet_check)<-c("Diets","Family","Nb_sp_check")
mari_diet_check$rank_check<-rank(-mari_diet_check$Nb_sp_check, ties.method="min")

mari_diet_rank<-merge(mari_diet , mari_diet_check, by=c("Family","Diets") , all=T)

plot(mari_diet_rank$rank_check , mari_diet_rank$rank, xlab="Rank check", ylab="Rank",
     main="Marshall Isl Family x Diet", cex.lab=1.5, cex=2, cex.axis=1.5, pch=20)

cor.test(mari_diet_rank$rank , mari_diet_rank$rank_check, method="spearman")


#Virgin islands
vir_diet<-aggregate(fish_sp[full_data$site_code=="vir"] ~ Diets[full_data$site_code=="vir"] + family[full_data$site_code=="vir"], full_data, function(x) length(unique(x)))
vir_diet
names(vir_diet)<-c("Diets","Family","Nb_sp")
vir_diet$rank<-rank(-vir_diet$Nb_sp, ties.method="min")

vir_diet_check<-aggregate(cbind(check_sp[check_data$Site_code=="vir"]) ~ Diets[check_data$Site_code=="vir"] + family[check_data$Site_code=="vir"], check_data, function(x) length(unique(x)))
names(vir_diet_check)<-c("Diets","Family","Nb_sp_check")
vir_diet_check$rank_check<-rank(-vir_diet_check$Nb_sp_check, ties.method="min")

vir_diet_rank<-merge(vir_diet , vir_diet_check, by=c("Family","Diets") , all=T)

plot(vir_diet_rank$rank_check , vir_diet_rank$rank, xlab="Rank check", ylab="Rank",
     main="Virgin Isl Family x Diet", cex.lab=1.5, cex=2, cex.axis=1.5, pch=20)

cor.test(vir_diet_rank$rank , vir_diet_rank$rank_check, method="spearman")



####First trial####
#Matrix with % relative to 100%
size.per.sum<-tapply(size.merge$per, size.merge$site_code, sum, na.rm=T)

size.merge$per_100[size.merge$site_code=="haw"]<-size.merge$per[size.merge$site_code=="haw"]*100/size.per.sum[1]
size.merge$per_100[size.merge$site_code=="mad"]<-size.merge$per[size.merge$site_code=="mad"]*100/size.per.sum[2]
size.merge$per_100[size.merge$site_code=="mari"]<-size.merge$per[size.merge$site_code=="mari"]*100/size.per.sum[3]
size.merge$per_100[size.merge$site_code=="vir"]<-size.merge$per[size.merge$site_code=="vir"]*100/size.per.sum[4]

size.matrix.100<-matrix(size.merge$per_100,nrow=6, ncol=4)
colnames(size.matrix.100)<-c("haw", "mad", "mari", "vir")
rownames(size.matrix.100)<-c("S1","S2","S3", "S4", "S5", "S6")
size.matrix.100[1,4]<-0

library("RColorBrewer")
display.brewer.all()

par(mar=c(6.1, 5.1, 3.1, 8.1), xpd=TRUE)

barplot(size.matrix.100,
        ylab="% of species", 
        col=brewer.pal(n = 6, name = "Greys"),
        cex.names=1.5, cex.axis=1.5, cex.lab=1.5)

legend("topright", inset=c(-0.15,0), legend=rownames(size.matrix.100), 
       col=brewer.pal(n = 6, name = "Greys"), pch=15, pt.cex=2)

#Matrice nb d'esp/classe de taille
size.matrix.nb<-matrix(size.merge$acc_name, nrow=6, ncol=4)
colnames(size.matrix.nb)<-c("haw", "mad", "mari", "vir")
rownames(size.matrix.nb)<-c("S1","S2","S3", "S4", "S5", "S6")
size.matrix.nb[1,4]<-0

barplot(size.matrix.nb,
        ylab="Number of species", 
        col=brewer.pal(n = 6, name = "Greys"),
        cex.names=1.5, cex.axis=1.5, cex.lab=1.5)

legend("topright", inset=c(-0.15,0), legend=rownames(size.matrix.nb), 
       col=brewer.pal(n = 6, name = "Greys"), pch=15, pt.cex=2)


####Diets####
        
diet<-aggregate(acc_name ~ Diets + site_code, full_data, function(x) length(unique(x)))
diet
tapply(diet$acc_name, diet$site_code, sum)

diet_check_full<-aggregate(acc_name ~ Diets+Site_code, check_data, function(x) length(unique(x)))
diet_check<-subset(diet_check_full, Site_code == "haw" | Site_code == "mads" | Site_code == "mari" | Site_code == "vir",
                   select = c("Diets", "Site_code", "acc_name")) #Only for wanted sites
names(diet_check)[3]<-"spcheck"
levels(diet_check$Site_code)[6] <- "mad"

dim(size) ; dim(size_check)

#Merge diet dataframe and percentage
diet.merge<-merge(diet, diet_check, by.x=c("site_code","Diets"), by.y=c("Site_code","Diets"), all=T)
diet.merge$per<-diet.merge$acc_name*100/diet.merge$spcheck

#Matrix with % relative to 100%
diet.per.sum<-tapply(diet.merge$per, diet.merge$site_code, sum, na.rm=T)

diet.merge$per_100[diet.merge$site_code=="haw"]<-diet.merge$per[diet.merge$site_code=="haw"]*100/diet.per.sum[1]
diet.merge$per_100[diet.merge$site_code=="mad"]<-diet.merge$per[diet.merge$site_code=="mad"]*100/diet.per.sum[2]
diet.merge$per_100[diet.merge$site_code=="mari"]<-diet.merge$per[diet.merge$site_code=="mari"]*100/diet.per.sum[3]
diet.merge$per_100[diet.merge$site_code=="vir"]<-diet.merge$per[diet.merge$site_code=="vir"]*100/diet.per.sum[4]

diet.matrix<-matrix(diet.merge$per_100, ncol=4, nrow=7)
colnames(diet.matrix)<-c("haw", "mad", "mari", "vir")
rownames(diet.matrix)<-c("FC","HD","HM", "IM", "IS", "OM","PK")
diet.matrix[3,2]<-0

#Barplot
barplot(diet.matrix,
        ylab="% of species", 
        col=brewer.pal(n = 7, name = "Greys"),
        cex.names=1.5, cex.axis=1.5, cex.lab=1.5)

legend("topright", inset=c(-0.15,0), legend=rownames(diet.matrix), 
       col=brewer.pal(n = 7, name = "Greys"), pch=15, pt.cex=2)

#Merge nb species, matrix and barplot
diet.matrix.nb<-matrix(diet.merge$acc_name, ncol=4, nrow=7)
colnames(diet.matrix.nb)<-c("haw", "mad", "mari", "vir")
rownames(diet.matrix.nb)<-c("FC","HD","HM", "IM", "IS", "OM","PK")
diet.matrix.nb[3,2]<-0

barplot(diet.matrix.nb,
        ylab="Nb of species", 
        col=brewer.pal(n = 7, name = "Greys"),
        cex.names=1.5, cex.axis=1.5, cex.lab=1.5)

legend("topright", inset=c(-0.15,0), legend=rownames(diet.matrix.nb), 
       col=brewer.pal(n = 7, name = "Greys"), pch=15, pt.cex=2)


####Family x Size class x Diet####
agg<-aggregate(fish_sp ~ site_code + family + Size.Class + Diets, full_data, function(x) length(unique(x)))
agg
agg$site_code<-as.factor(agg$site_code)
levels(agg$site_code)
summary(agg)

agg_check_full<-aggregate(acc_name ~ Site_code + Family + Size.Class + Diets, check_data, function(x) length(unique(x)))
agg_check<-subset(agg_check_full, Site_code == "haw" | Site_code == "mads" | Site_code == "mari" | Site_code == "vir",
                   select = c("Diets", "Site_code", "acc_name", "Size.Class", "Family")) #Only for wanted sites
names(agg_check)[3]<-"spcheck"
levels(agg_check$Site_code)[6] <- "mad"
summary(agg_check)

agg.merge<-merge(agg, agg_check, by.x=c("site_code", "Size.Class", "Diets","family"), by.y=c("Site_code", "Size.Class", "Diets","Family"))
head(agg.merge)

agg.merge$per<-agg.merge$acc_name*100/agg.merge$spcheck

agg.haw<-subset(agg, site_code=="haw")
summary(agg.haw)
agg.mad<-subset(agg, site_code=="mad")
agg.mari<-subset(agg, site_code=="mari")
agg.vir<-subset(agg, site_code=="vir")

length(unique(agg.vir$family))

#graph
library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

#gg.haw <- mutate(group_by(agg.haw, Size.Class), per)
ggplot(agg.haw) +
  geom_col(aes(family, fish_sp, fill = Diets)) +
  facet_wrap(~Size.Class)+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

#gg.mad <- mutate(group_by(agg.mad, Size.Class), per)
ggplot(agg.mad) +
  geom_col(aes(family, fish_sp, fill = Diets)) +
  facet_wrap(~Size.Class)+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

#gg.mari <- mutate(group_by(agg.mari, Size.Class), per)
ggplot(agg.mari) +
  geom_col(aes(family, fish_sp, fill = Diets)) +
  facet_wrap(~Size.Class)+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5, size=7))

#gg.vir <- mutate(group_by(agg.vir, Size.Class), per)
ggplot(agg.vir) +
  geom_col(aes(family, fish_sp, fill = Diets)) +
  facet_wrap(~Size.Class)+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5, size=7.5))




#####Final nb sp dataframe####

names(data)
#Data frame nb occurence
tot<-sapply(data[,c("family_cor","genus_cor","fish_sp")], function(x) length(unique(x)))
mad<-sapply(data[,c("family_cor","genus_cor","fish_sp")], function(x) length(unique(x[data$site_code=="mad"])))
haw<-sapply(data[,c("family_cor","genus_cor","fish_sp")], function(x) length(unique(x[data$site_code=="haw"])))
mari<-sapply(data[,c("family_cor","genus_cor","fish_sp")], function(x) length(unique(x[data$site_code=="mari"])))
vir<-sapply(data[,c("family_cor","genus_cor","fish_sp")], function(x) length(unique(x[data$site_code=="vir"])))# nb occurence data sp, gen, fam

df<-data.frame(tot,mad, haw, mari, vir)
rownames(df) <-c ("family","genus","species")
