################
#Beta-Diversity#
################

library(betalink)
library(tidyverse)
library(reshape2)
library(igraph)
library(wesanderson)

w <- list(vir = vir_ISmatrix2, mari = mari_ISmatrix2, nca = nca_ISmatrix2, mad = mad_ISmatrix2, haw = haw_ISmatrix2)
networks <- betalink::prepare_networks(w, directed = FALSE) #transform into igraph object

betalink::betalink(networks$vir, networks$mad, bf=betalink::B01)
class(networks$vir)
betalink::betapart(networks$vir, networks$mad)

betalink::B01(betalink::betapart(networks$vir, networks$mad))
?betalink

length(haw_ISmatrix2[haw_ISmatrix2 != 0]) #nb of interactions (vertices)
E(networks$haw)
V(networks$haw)$name

get.edgelist(networks$haw)

betadiv <- betalink::network_betadiversity(networks, complete = TRUE, bf=betalink::B01) #B01 with Whittaker
#S = dissimilarity of species composition
#WN = network interactions dissimilarity = OS + ST
#OS = dissimilarity of interactions in co-occuring species 
#ST = dissimilaity of interactions due to species turnover

betadiv_mat <- reshape2::acast(betadiv, i ~ j, value.var = "WN")

betadiv_mat_sor <- betalink::network_betadiversity(networks, complete = TRUE, bf=betalink::B11) %>%
  reshape2::acast(., i ~ j, value.var = "WN") #Sorensen, higher more similar

#nca is the less dissimilar to all networks and it's the network that have the fewer interactions (edges)

plot(betadiv$S, betadiv$WN)
plot(betadiv$S, betadiv$OS) #S and OS not so correlated
plot(betadiv$S, betadiv$ST)
plot(betadiv$WN, betadiv$OS) #WN and OS not so correlated

betadiv <- betadiv %>% mutate(OS_per = (OS/WN) *100,
                              ST_per = (ST/WN)*100)

mean_se(betadiv$OS_per)

betalink::betapart(networks$vir, networks$mad)
betalink::B01(betalink::betapart(networks$vir, networks$mad))

kruskal.test(betadiv_long$value[betadiv_long$indice=="OS"],
             betadiv_long$value[betadiv_long$indice=="ST"]) #Only ten values though ... so Sign :  betadiv driven by interactions turnover rather than species turnover ? 

?betapart

betadiv_long <- gather(betadiv, key = "indice", value="value", -OS_per, -ST_per, -i, -j) %>%
  mutate(sites = paste(i, j, sep="_")) %>%
  filter(sites %in% c("nca_mari","nca_haw","nca_vir","nca_mad","mari_haw","mari_vir","mari_mad",
                      "haw_vir","haw_mad","vir_mad")) %>%
  mutate(region = dplyr::case_when(sites == "vir_mad" ~ "OA_OI",
                                   sites == "haw_mad" ~ "OP_OI",
                                   sites == "haw_vir" ~ "OP_OA",
                                   sites == "mari_mad" ~"OP_OI",
                                   sites == "mari_vir" ~ "OP_OA",
                                   sites == "mari_haw" ~ "OP",
                                   sites == "nca_mad" ~ "OP_OI",
                                   sites == "nca_vir" ~ "OP_OA",
                                   sites == "nca_haw" ~ "OP", 
                                   sites == "nca_mari" ~ "OP"))
#Barplot
ggplot(data=betadiv_long, aes(x = sites, y=value, fill=indice)) +
  geom_bar(data = subset(betadiv_long, indice %in% c("ST","OS")), stat="identity") +
  geom_text(data = subset(betadiv_long, indice %in% "WN"), aes(x = sites, y=value, label= round(value,2)), vjust=-0.5, size=3) +
  facet_grid(.~region, scales="free") +
  labs(title="Network dissimilarity", 
       x="Sites", y = "Indice value") + 
  scale_fill_brewer(palette="Paired") +
  theme(strip.text.x = element_text(size=8, face="bold"),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(colour = "black", 
                                   size=0.5, 
                                   lineend = "butt"),
        axis.line.y = element_line(colour = "black", 
                                   size=0.5)) +
  guides(color=FALSE)


ggplot(data=betadiv_long, aes(x = indice, y=value, fill=indice)) +
  #geom_boxplot(data = subset(betadiv_long, indice %in% c("ST","OS"))) +
  #geom_violin(data = subset(betadiv_long, indice %in% c("ST","OS"))) + #show data distribution
  stat_summary(data = subset(betadiv_long, indice %in% c("ST","OS")), fun.y = mean, geom = "point", shape = 19, size = 2, color = c("#0099CC","#0033CC")) +
  stat_summary(data = subset(betadiv_long, indice %in% c("ST","OS")), fun.data = mean_ICt, geom="errorbar", color = c("#0099CC","#0033CC"), width=0.5) +
  labs(title="Network dissimilarity", 
       x="Beta-diversity", y = "Indice value") + 
  #scale_fill_brewer(palette="Paired") +
  #scale_fill_manual(values=wes_palette(n=3, name="Darjeeling2")) +
  theme(strip.text.x = element_text(size=8, face="bold"),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(colour = "black", 
                                   size=0.5, 
                                   lineend = "butt"),
        axis.line.y = element_line(colour = "black", 
                                   size=0.5)) +
  guides(color=FALSE)


hist(betadiv_long$value[betadiv_long$indice=="OS"])

mean_ICt <- function(x){
  r <- c(mean(x), mean(x)-qt(0.975, df=length(x)-1)*sd(x)/sqrt(length(x)), mean(x)+qt(0.975, df=length(x)-1)*sd(x)/sqrt(length(x)))
  names(r) <- c("y","ymin","ymax")
  r
}
  





#Try get a distance matrix from geographic points
df.sites <- data.frame(name = c("haw", "mad", "mari", "nca", "vir"), #to have same order as network dissimilarity
                      lon  = c(-157.975203, 43.66970, 171.55399, 166.44900, -64.794499),
                       lat  = c(21.2160437, -23.3542, 7.06691,  -22.2741, 18.3308)) %>%
  column_to_rownames(., var="name") %>%
  as.matrix(.)


#Create distance matrix
library(geosphere)

mat.sites <- distm(df.sites, fun=distHaversine)/1000  #1000 to have values in km
  rownames(mat.sites) <- c("haw", "mad", "mari", "nca", "vir")
  colnames(mat.sites) <- c("haw", "mad", "mari", "nca", "vir")

#Try matrix correlation test
#Is the network dissimilarity is link to geographic distance ? 
#Is network dissimilarity increase with increasing geopgraphic distance ?  
library(ecodist)
mantel(as.dist(betadiv_mat) ~ as.dist(mat.sites), mrank =TRUE) #Whittaker dissimilarity
dist(mat.sites)
dist(betadiv_mat) #Euclidian distance
as.dist() #to keep the distance matrice as given and transform into a dist object
?dist

#With Sorensen
mantel(as.dist(betadiv_mat_sor) ~ as.dist(mat.sites), mrank =TRUE) #Sorensen dissimilarity
#Euclidian dist are the same depending on index used (makes sense...), so conclusions of the test can only be is dissimilarity/similarity link to geographic distances
#When keeping observed dissimilarity/distance matrices, results test change (make sense...)
#Buuuut mantel test seems finally innapropriated and very small matrices ...

#Metaweb stuff
fullnet <- betalink::metaweb(networks) #create a metaweb from all networks
net_pacifique <- betalink::metaweb(list(networks$mari, networks$nca, networks$haw))
N=list(net_pacifique, networks$mad, networks$vir)
betalink::beta_os_prime(N, bf=betalink::B01) #returns beta 0S so dissimilarity from interactions turnover

?metaweb


####Check with null model picante::frequency (rand)####
wrand <- list(rand_vir = rand_vir, rand_mari= rand_mari, rand_nca= rand_nca, rand_mad=rand_mad, rand_haw=rand_haw) #10 randomized matrices for each region

networks_rand <- sapply(wrand, betalink::prepare_networks, directed = FALSE)
class(networks_rand[1,])

#To get indices values for each set of random matrices
betalink::network_betadiversity(networks_rand[1,], complete = TRUE, bf=betalink::B01)
betadiv_rand <- apply(networks_rand, 1, function(x) betalink::network_betadiversity(x, complete = TRUE, bf=betalink::B01))

betadiv_rand$network_1$WN

WN_rand <- unlist(sapply(betadiv_rand, function(x) x$WN)) %>% 
  data.frame(.) %>% 
  gather(., key="net_rand", value="WN")

ST_rand <- unlist(sapply(betadiv_rand, function(x) x$ST)) %>% 
  data.frame(.) %>% 
  gather(., key="net_rand", value="ST")
  
OS_rand <- unlist(sapply(betadiv_rand, function(x) x$OS)) %>% 
  data.frame(.) %>% 
  gather(., key="net_rand", value="OS")

S_rand <- unlist(sapply(betadiv_rand, function(x) x$S)) %>% 
  data.frame(.) %>% 
  gather(., key="net_rand", value="S")

i_rand <- unlist(sapply(betadiv_rand, function(x) x$i)) %>% 
  data.frame(.) %>% 
  gather(., key="net_rand", value="i_rand")

j_rand <- unlist(sapply(betadiv_rand, function(x) x$j)) %>% 
  data.frame(.) %>% 
  gather(., key="net_rand", value="j_rand")


beta_rand <- cbind(WN_rand, OS_rand[,2], ST_rand[,2], S_rand[,2], i_rand[,2], j_rand[,2])
names(beta_rand)[3:7] <- c("OS_rand","ST_rand","S_rand","i_rand","j_rand")


####Check for most connected nodes ####
nb_int_vir <- sort(lengths(as_adj_list(networks$vir)), decreasing=TRUE) %>% data.frame(.) #Haemulidae and Decapoda
nb_int_mari <- sort(lengths(as_adj_list(networks$mari)), decreasing=TRUE) %>% data.frame(.) #Actinopterygii + Decapoda and Labridae + Pomacentridae
nb_int_mad <- sort(lengths(as_adj_list(networks$mad)), decreasing=TRUE) %>% data.frame(.) #Decapoda + Monacanthidae
nb_int_nca <- sort(lengths(as_adj_list(networks$nca)), decreasing=TRUE) %>% data.frame(.) #Actinopterygii + Decapoda and Lethrinidae
nb_int_haw <- sort(lengths(as_adj_list(networks$haw)), decreasing=TRUE) %>% data.frame(.) #Decapoda + Pomacentridae


# Vertices in the two networks
v1 <- igraph::V(networks$vir)$name
v2 <- igraph::V(networks$mad)$name
vs <- v1[v1 %in% v2] # Shared vertices


sn1 <- igraph::induced.subgraph(networks$vir, which(igraph::V(networks$vir)$name %in% vs))
sn2 <- igraph::induced.subgraph(networks$mad, which(igraph::V(networks$mad)$name %in% vs))
            