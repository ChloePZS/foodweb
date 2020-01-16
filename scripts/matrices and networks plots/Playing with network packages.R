##########################################
#Playing with some network/foodweb package#
##########################################

library(igraph)
library(NetIndices)

#Getting one dataset per site
haw <- data_full2 %>% filter(site_code=="haw")
vir <- data_full2 %>% filter(site_code=="vir")
mari <- data_full2 %>% filter(site_code=="mari")
mad <- data_full2 %>% filter(site_code=="mad")
nca <- data_full2 %>% filter(site_code=="nca")

#Create a graph object
vir.graph <- graph.edgelist(as.matrix(vir.link3))
vir.adjmatrix <- get.adjacency(vir.graph, sparse=F)
?get.adjacency

# Get the basic network indices from the matrices with GenInd()
ind.vir<-GenInd(vir.adjmatrix)

# Now to plot these two webs to get a feel for what we are dealing with
par(mar=c(.1,.1,.1,.1))
plot.igraph(vir.graph,vertex.label=NA,vertex.size=3,edge.arrow.size=.25,layout=layout.circle)

troph.vir<-TrophInd(vir.adjmatrix, Dead = as.character(vir.nodes3$id[57:92]))
troph.vir



# First we need to create a two-column matrix identifying the x and y values for each node.
layout.matrix.1<-matrix(
  nrow=length(V(vir.graph)),  # Rows equal to the number of vertices
  ncol=2
)
layout.matrix.1[,1]<-runif(length(V(vir.graph))) # randomly assign along x-axis
layout.matrix.1[,2]<-troph.vir$TL

plot.igraph(vir.graph,
            vertex.label.cex=1,
            vertex.size=3,
            edge.arrow.size=.10,
            layout=layout.matrix.1)

#Degree distribution
deg.vir<-degree(vir.graph)
dd.vir<-degree.distribution(vir.graph,mode="all",cumulative=T)
plot(dd.vir)


#Community detection
wtc.vir<-walktrap.community(vir.graph)

cw.vir <- cluster_walktrap(vir.graph)


plot.igraph(vir.graph,
            vertex.label.cex=.35,
            vertex.size=3,
            edge.arrow.size=.25,
            layout=layout.matrix.1,
            mark.groups=cw.vir$membership,
            mark.col="green")


####Cheddar package####
library(cheddar)
PlotWebByLevel(TL86)
head(TL86$trophic.links)

TL86.res <- TL86$trophic.links[1]
TL86.con <- TL86$trophic.links[2]
intersect(TL86.res[,1],TL86.con[,1]) #some as both consumers and resources

#Lets try to create the community object for Virgin Islands : need trhee files

properties <- data.frame("title" = "Virgin Islands food web by Randall", "habitat" = "coral reef")
write.table(properties, "vir community/properties.csv", row.names = FALSE, col.names = c("title", "habitat"))
nodes <- vir.nodes3
names(nodes)[1] <- "node"
write.table(nodes, "vir community/nodes.csv", row.names = FALSE, col.names = c("node"))
trophic.links <- vir.link3
names(trophic.links)[1] <- "consumer"
names(trophic.links)[2] <- "resource"
trophic.links <- trophic.links[,c(2,1)]
write.table(trophic.links, "vir community/trophic.links.csv", row.names = FALSE, col.names = c("resource","consumer"))

properties <- as.list(properties)
err_sp <- data.frame("node" = c("Engraulidae","Labrisomidae","Chaenopsidae","Syngnathidae","Pomacanthidae","Scissurellidae","Exocoetidae"))
nodes <- rbind(nodes, err_sp)

vir.com <- Community(nodes = nodes, properties = properties, trophic.links = trophic.links)
PlotWebByLevel(vir.com, show.level.labels = TRUE)
 pm <- PredationMatrix(vir.com)

 
#Check node properties
connectivity <- NPS(vir.com, c(Basal='IsBasalNode',Isolated='IsIsolatedNode',Intermediate='IsIntermediateNode',TopLevel='IsTopLevelNode'))
BasalNodes(vir.com)

#isolated No resources or consumers, other than possibly itself
#basal No resources and one or more consumers
#top-level One or more resources and no consumers, other than possibly itself
#intermediate Nodes not fitting any of the above categories


tc <- TrophicChains(vir.com)
dim(tc)  #longest chain has a length of 10


tl <- TrophicLevels(vir.com)
#Would be possible to do a weighted web with item frequency or diet fraction, and then obtain interaction strength with function InteractionStrength()


#Order Community
PlotPredationMatrix(vir.com)
new.order <- order(PreyAveragedTrophicLevel(vir.com))
vir.increasing.TL <- OrderCommunity(vir.com, new.order=new.order,title='Increasing TL')
PlotPredationMatrix(vir.increasing.TL)

#Possible to remove nodes
to.remove <- c("Anthozoa")
vir.com.r <- RemoveNodes(vir.com, to.remove) 
NumberOfNodes(vir.com.r)
PlotWebByLevel(vir.com.r, show.level.labels = TRUE)

#Degree distribution
PlotDegreeDistribution(vir.com)

#Trophic similarity
TrophicSimilarity(vir.com)
TrophicSpecies(vir.com)


####Foodweb package####
library(foodweb)
analyse.single(filename = "pm.csv")
write.csv(pm, "pm.csv")
pmcsv <- read.csv("pm.csv")
rm(pmcsv) ; rm(foodweb)

foodweb <- foodweb[-1,-1]
plotweb(foodweb)


####econullntr####

library(econullnetr)

Silene
Broadstone <- Broadstone
Broadstone.nodes
Broadstone.prey

colSums(Broadstone[,2:20])
