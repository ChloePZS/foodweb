##################
#Tripartite plots#
##################

library(tidyverse)
library(bipartite)

#Getting one dataset per site
haw <- data_full2 %>% filter(site_code=="haw")
vir <- data_full2 %>% filter(site_code=="vir")
mari <- data_full2 %>% filter(site_code=="mari")
mad <- data_full2 %>% filter(site_code=="mad")
nca <- data_full2 %>% filter(site_code=="nca")

#Check fish_sp as prey items --> Only vir, mari and haw for which fish items ID to the family levels

vir %>% filter(!is.na(item_fam), item_class=="Actinopterygii") %>% n_distinct()
mari %>% filter(!is.na(item_fam), item_class=="Actinopterygii") %>% n_distinct()
mad %>% filter(!is.na(item_fam), item_class=="Actinopterygii") %>% n_distinct()
haw %>% filter(!is.na(item_fam), item_class=="Actinopterygii") %>% n_distinct()
nca %>% filter(!is.na(item_fam), item_class=="Actinopterygii") %>% n_distinct()

#Creating list of all fish families and fish prey items
fish.vir <- data_clean2 %>% filter(!is.na(item_fam), item_class=="Actinopterygii", site_code=="vir")

fish.vir<- fish.vir %>% 
  mutate_if(is.factor, as.character) %>% glimpse()

n <- unique(fish.vir$item_fam)
n <- n[order(n)] #47 fish prey
m <- unique(fish.vir$family) #predators 28 families
m<- m[order(m)] #alphabetic order 

#Fish pred/prey matrix for Virgin Islands
x <- with(fish.vir, table(fish.vir$item_fam, fish.vir$family))
fish.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
fish.matrix[i1] <- x[i1]

dim(fish.matrix)

fish.matrix.std <- apply(fish.matrix, 2,function(x) x/sum(x))
fish.matrix.std[is.na(fish.matrix.std)] <- 0
rownames(fish.matrix.std)

#Fish pred/other preys matrix
intersect(vir$family, fish.vir$item_fam)
fish.vir2 <- vir %>% filter(vir$family %in% fish.vir$item_fam)

n <- unique(fish.vir2$grp3) #prey
n <- n[order(n)]
m <- unique(fish.vir2$family) #pred same as the preys from the fish matrix
m <- m[order(m)] #alphabetic order  

#Virgin Islands
x <- with(fish.vir2, table(fish.vir2$grp3, fish.vir2$family))
vir.matrix <- matrix(0, ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
vir.matrix[i1] <- x[i1]

dim(vir.matrix)

vir.matrix.std <- apply(vir.matrix, 2,function(x) x/sum(x))

vir.matrix.std[is.na(vir.matrix.std)] <- 0 #replace the NA values by 0

rownames(vir.matrix.std) #look for item as NA

vir.matrix.std <- vir.matrix.std[-37,] #removing items as NA

dim(fish.matrix.std) #47 prey and 28 predators
dim(vir.matrix.std) #36 preys and 40 predators

remove <- setdiff(rownames(fish.matrix.std), colnames(vir.matrix.std)) #remove families not as predtors to have exact same sets of nodes between the two plots

fish.matrix.std <- fish.matrix.std[!rownames(fish.matrix.std) %in% remove,]

plotweb(fish.matrix.std, text.rot = 90, empty = FALSE)
plotweb(vir.matrix.std, text.rot = 90, empty = FALSE)

plotweb2(fish.matrix.std, vir.matrix.std, empty = FALSE, empty2 = FALSE) #still doesnt work...

#same with no standardised
dim(fish.matrix) #47 prey and 28 predators
dim(vir.matrix) 

remove <- setdiff(rownames(fish.matrix), colnames(vir.matrix)) #remove families not as predtors to have exact same sets of nodes between the two plots

fish.matrix<- fish.matrix[!rownames(fish.matrix) %in% remove,]

plotweb(fish.matrix, text.rot = 90, empty = FALSE)
plotweb(vir.matrix, text.rot = 90, empty = FALSE)

plotweb2(fish.matrix, vir.matrix, empty = FALSE, empty2 = FALSE)

intersect(rownames(fish.matrix), colnames(fish.matrix))

rname <- rownames(fish.matrix) 
rname <- paste0("2", rname)


#Let's try by having different names between rows and columns from fishmatrix
fish.matrix2 <- fish.matrix
rownames(fish.matrix2) <- paste0("2", rownames(fish.matrix2))

vir.matrix2 <- vir.matrix
colnames(vir.matrix2) <- paste0("2", colnames(vir.matrix2))

plotweb2(fish.matrix2, vir.matrix2, empty = FALSE, empty2 = FALSE) #still doesn't work

   ####Try with Igraph : ket's try with vir.matrix.std for a start####
library(igraph)

#Preparing a node and link objects
pred <- as.data.frame(colnames(vir.matrix.std)) 
colnames(pred) <- "id"
pred$grp <- "pred"

prey <- as.data.frame(rownames(vir.matrix.std)) 
colnames(prey) <- "id"
prey$grp <- "prey"

vir.nodes <- rbind(pred, prey)

vir.link <- vir.matrix.std


#Transform network into graph
net <- graph_from_incidence_matrix(vir.link, directed = T)
net[]
table(V(net)$type)
V(net)


  ##Other try from dataframe
head(vir)
#First the links, based for now just number of interactions
fish.vir <- vir %>% dplyr::select(family, item_fam) %>% dplyr::filter(vir$item_class == "Actinopterygii") %>%
  count() %>%
  dplyr::rename(item = item_fam)

grp3.vir <- vir %>% dplyr::select(family, grp3) %>% dplyr::filter(!is.na(grp3)) %>%
  count() %>%
  dplyr::rename (item = grp3)

vir.link2 <- rbind(fish.vir, grp3.vir) %>%
  dplyr::rename(from = family, to = item) %>%
  filter(!is.na(to), !is.na(from))

#Second, the nodes files
pred1 <- as.data.frame(unique(fish.vir$family))
names(pred1)[1] <- "id"
pred1$type <- "pred1"

unique(grp3.vir$family)

pred2 <- as.data.frame(unique(grp3.vir$family))
names(pred2)[1] <- "id"
pred2$type <- "pred2"

prey <- as.data.frame(unique(grp3.vir$item)) 
names(prey)[1] <- "id"
prey$type <- 'prey'

vir.fam <- as.data.frame(unique(vir$family)) 
  names(vir.fam)[1] <- "id"

grp3 <- as.data.frame(unique(vir$grp3))
names(grp3)[1] <- "id"

vir.nodes3 <- rbind(vir.fam, grp3) %>%
  filter(!is.na(id))


vir.nodes2 <- rbind(pred1, pred2, prey)
vir.nodes2 %>% filter (id == "Serranidae")


vir.net <- graph_from_data_frame(d=vir.link2, vertices=vir.nodes3, directed=T) 
vir.net

#Lets try plot just fish family/grp3 item
vir.fam <- as.data.frame(unique(vir$family)) 
names(vir.fam)[1] <- "id"

grp3 <- as.data.frame(unique(vir$grp3))
names(grp3)[1] <- "id"

vir.nodes3 <- rbind(vir.fam, grp3) %>%
  filter(!is.na(id))

grp3.vir <- grp3.vir %>% dplyr::rename(weight=freq, from = family, to =item)

vir.net <- graph_from_data_frame(d=grp3.vir, vertices=vir.nodes3, directed=T) 
vir.net

vir.net <- simplify(vir.net, remove.multiple = F, remove.loops = T) 

plot(vir.net,layout=layout.circle, vertex.size=6, arrow.mode = 0)

vir.net.bp <- bipartite.projection(net)

plot(vir.net.bp$proj2, vertex.label.color="black", vertex.label.dist=1, vertex.size=7)


#Tripartite : Worked by not nice ! https://rpubs.com/barryrowlingson/tripartite
vir.link3 <- vir.link2 %>% select(-freq)
vir.link3 %>% filter(to == "Syngnathidae")

g = graph_from_data_frame(vir.link3, directed=TRUE)
g
#Assign a level 
layer = length(V(g)$name)
layer[V(g)$name[1:28]] <- 1
layer[V(g)$name[29:63]] <- 2
layer[V(g)$name[64:99]] <- 3
layer <- layer[-1]

layer


layout1 = layout_with_sugiyama(g, layers=layer)

?layout_with_sugiyama

g <- simplify(g, remove.multiple = F, remove.loops = T)

plot(g,
     layout=cbind(layer,layout1$layout[,1]),
     vertex.size=c(10,5,0)[layer],
     vertex.label.dist=c(-6,0,1)[layer],
     vertex.label.degree=0,
     edge.arrow.size = 0.4)

table(layout1$layout[,2])

?rescale


