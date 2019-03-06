###############################
# Trying the bipartite network#
###############################
library(bipartite)
library(fossil)
library(tidyverse)

#Create data frame per site
haw <- data_replace %>% filter(site_code=="haw")
vir <- data_replace %>% filter(site_code=="vir")
mari <- data_replace %>% filter(site_code=="mari")
mad <- data_replace %>% filter(site_code=="mad")

haw <- as.data.frame(haw)
vir <- as.data.frame(vir)
mari <- as.data.frame(mari)
mad <- as.data.frame(mad)


  ###Simple occurence matrix for each site at grp, phylum levels

#Phylum
str(haw)
haw.phy <- create.matrix(haw, tax.name = "item_phylum" , locality = "family_cor") #create a simple occurence matrix
plotweb(haw.phy, text.rot = 90, arrow="down", labsize = 1.3, ybig = 1.2, low.y = 0.65, high.y=1.6, method="normal")

mad.phy <- create.matrix(mad, tax.name = "item_phylum" , locality = "family_cor") #create a simple occurence matrix
plotweb(mad.phy, text.rot = 90, arrow="down", labsize = 1.3, ybig = 1.2, low.y = 0.65, high.y=1.6, method="normal")

vir.phy <- create.matrix(vir, tax.name = "item_phylum" , locality = "family_cor") #create a simple occurence matrix
plotweb(vir.phy, text.rot = 90, arrow="down", labsize = 1.3, ybig = 1.2, low.y = 0.65, high.y=1.6, method="normal")

mari.phy <- create.matrix(mari, tax.name = "item_phylum" , locality = "family_cor") #create a simple occurence matrix
plotweb(mari.phy, text.rot = 90, arrow="down", labsize = 1.3, ybig = 1.2, low.y = 0.65, high.y=1.6, method="normal")



#Class
haw.cla <- create.matrix(haw, tax.name = "item_class" , locality = "family_cor") #create a simple occurence matrix
plotweb(haw.cla, text.rot = 90, arrow="down", labsize = 1.3, ybig = 1.2, low.y = 0.65, high.y=1.6,method="normal")

mad.cla <- create.matrix(mad, tax.name = "item_class" , locality = "family_cor") #create a simple occurence matrix
plotweb(mad.cla, text.rot = 90, arrow="down", labsize = 1.3, ybig = 1.2, low.y = 0.65, high.y=1.6, method="normal")

visweb(haw.cla, type="diagonal", square="compartment", text="none", 
       frame=TRUE, labsize = 2)

visweb(mad.cla, type="diagonal", square="compartment", text="none", 
       frame=TRUE, labsize = 2)

vir.cla <- create.matrix(vir, tax.name = "item_class" , locality = "family_cor") #create a simple occurence matrix
plotweb(vir.cla, text.rot = 90, arrow="down", labsize = 1.3, ybig = 1.2, low.y = 0.65, high.y=1.6, method="normal")

mari.cla <- create.matrix(mari, tax.name = "item_class" , locality = "family_cor") #create a simple occurence matrix
plotweb(mari.cla, text.rot = 90, arrow="down", labsize = 1.3, ybig = 1.2, low.y = 0.65, high.y=1.6, method="normal")

visweb(vir.cla, type="diagonal", square="compartment", text="none", 
       frame=TRUE, labsize = 2)

visweb(mari.cla, type="diagonal", square="compartment", text="none", 
       frame=TRUE, labsize = 2)

#Broad groups

haw.grp <- create.matrix(haw, tax.name = "grp" , locality = "family_cor") #create a simple occurence matrix
plotweb(haw.grp, text.rot = 90,arrow="down", labsize = 1.3, ybig = 1.2, low.y = 0.65, high.y=1.6, method="normal")

mad.grp <- create.matrix(mad, tax.name = "grp" , locality = "family_cor") #create a simple occurence matrix
plotweb(mad.grp, text.rot = 90, arrow="down", labsize = 1.3, ybig = 1.2, low.y = 0.65, high.y=1.6, method="normal")

vir.grp <- create.matrix(vir, tax.name = "grp" , locality = "family_cor") #create a simple occurence matrix
plotweb(vir.grp, text.rot = 90, arrow="down", labsize = 1.3, ybig = 1.2, low.y = 0.65, high.y=1.6, method="normal")

mari.grp <- create.matrix(mari, tax.name = "grp" , locality = "family_cor") #create a simple occurence matrix
plotweb(mari.grp, text.rot = 90, arrow="down", labsize = 1.3, ybig = 1.2, low.y = 0.65, high.y=1.6, method="normal")

  ####Now with nb of trophic interaction####

####Let's try on Virgin Islands####

#First need to build an interaction matrix on fish families and phylum 
str(vir)
n <- unique(vir$item_phylum) #18 phylum + 1 NA
m <- unique(vir$family_cor) #213 fish species, 56 families

?with

x <- with(vir, table(vir$item_phylum,vir$family_cor))

y <- t(x) # returns the transpose of x
?matrix
vir.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))

i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))

vir.matrix[i1] <- x[i1]

?expand.grid()



  ##Let's do the plot
#With default arguments
plotweb(matrix,
        method = "cca", empty = TRUE, labsize = 1, ybig = 1,  y.width.low = 0.1, 
        y.width.high = 0.1, low.spacing = NULL, high.spacing = NULL,
        arrow="no",  col.interaction="grey80", col.high = "grey10", 
        col.low="grey10",  bor.col.interaction = FALSE, bor.col.high="black", 
        bor.col.low="black", high.lablength = NULL, low.lablength = NULL,
        sequence=NULL, low.abun=NULL, low.abun.col="green", 
        bor.low.abun.col ="black", high.abun=NULL, high.abun.col="red", 
        bor.high.abun.col="black", text.rot=90, text.high.col="black", 
        text.low.col="black", adj.high=NULL, adj.low=NULL, plot.axes = FALSE,
        low.y=0.5, high.y=1.5, add=FALSE, y.lim=NULL, x.lim=NULL, low.plot=TRUE, 
        high.plot=TRUE, high.xoff = 0, low.xoff = 0, high.lab.dis = NULL, 
        low.lab.dis = NULL, abuns.type="additional")

plotweb(sortweb(vir.matrix, sort.order="dec"), text.rot = 90, method="normal", arrow="down") #to sort out the matrix

#Get interactions and box colors

phylum.all <- as.data.frame(vir[c(3,9)])
phylum.unique <- unique(phylum.all)
colnames(phylum.unique) <- c("Taxon", "Phyla")
phylum <- as.data.frame(unique(phylum.unique$Phyla))
phyla <- as.data.frame(phylum[-7,])
colnames(phyla) <- "Phyla"
phyla2 <- as.data.frame(phyla[order(phyla$Phyla),])
colnames(phyla2) <- "Phyla"

colfunc4 <- colorRampPalette(c("mediumpurple4","mediumblue","skyblue","lightseagreen","gold","coral1","gray85","black"))
colfunc4(18) #get 18 colors betweens those given into colorRampPalette
plot(rep(1,18),col=(colfunc4(18)),pch=19,cex=2) #plot to see the colours

phyla2$colors <- colfunc4(18)

colorvector <- as.data.frame(merge(phyla2, phylum.unique, by = "Phyla"))
names(colorvector)[2] <- "colors"
box.colors <- as.character(phyla2$colors)
colorvec1 <- as.character(colorvector$colors)
head(colorvec1)
colfunc4


colorvector %>% group_by(Phyla) %>% count()

#with Jordan's fun arguments
plotweb(matrix,method="normal", empty = TRUE, arrow="down",text.rot=90,
        col.low = box.colors, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.2, ybig = 1.2, low.spacing = NULL) #to sort out the matrix
#can't get the proper colours for each interactions  : color of interaction according to box colour !!


  ####Marshall Islands####

#First need to build an interaction matrix on fish families and phylum 
str(mari)
n <- unique(mari$item_phylum) #17 phylum + 1 NA
m <- unique(mari$family_cor) #56 families

x <- with(mari, table(mari$item_phylum,mari$family_cor))

mari.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))

i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))

mari.matrix[i1] <- x[i1]


#Prepare data frame for color function
phylum.all <- as.data.frame(mari[c(3,9)])
phylum.unique <- unique(phylum.all)
colnames(phylum.unique) <- c("Taxon", "Phyla")
phylum <- as.data.frame(unique(phylum.unique$Phyla))
phyla <- as.data.frame(phylum[-1,])
colnames(phyla) <- "Phyla"
phyla2 <- as.data.frame(phyla[order(phyla$Phyla),])
colnames(phyla2) <- "Phyla"

colfunc4 <- colorRampPalette(c("mediumpurple4","mediumblue","skyblue","lightseagreen","gold","coral1","gray85","black"))
colfunc4(16) #get 16 colors betweens those given into colorRampPalette
plot(rep(1,16),col=(colfunc4(16)),pch=19,cex=2) #plot to see the colours

phyla2$colors <- colfunc4(16)

colorvector <- as.data.frame(merge(phyla2, phylum.unique, by = "Phyla"))
names(colorvector)[2] <- "colors"
box.colors <- as.character(phyla2$colors)
colorvec1 <- as.character(colorvector$colors)
head(colorvec1)
colfunc4

#with Jordan's fun arguments
plotweb(matrix,method="normal", empty = TRUE, arrow="down",text.rot=90,
        col.low = box.colors, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.2, ybig = 1.2, low.spacing = NULL) #to sort out the matrix


  ####LEt's try on Mada fish families x grp
str(mad)
mad$grp <- as.character(mad$grp)
n <- unique(mad$grp) #49/72
m <- unique(mad$family_cor) #36 families

x <- with(mad, table(mad$grp, mad$family_cor))
matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))

i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))

matrix[i1] <- x[i1]


#Plot
plotweb(sortweb(matrix, sort.order = "dec"),method="normal", empty = TRUE, arrow="no",text.rot=90,
        bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.2, ybig = 1.2, low.spacing = NULL) #to sort out the matrix

####LEt's try on Mada fish families x grp
str(mad)
mad$grp <- as.character(mad$grp)
n <- unique(mad$grp) #49/72
m <- unique(mad$family_cor) #36 families

x <- with(mad, table(mad$grp, mad$family_cor))
mad.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))

i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))

mad.matrix[i1] <- x[i1]


#Plot
plotweb(sortweb(mad.matrix, sort.order = "dec"),method="normal", empty = TRUE, arrow="no",text.rot=90,
        bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.2, ybig = 1.2, low.spacing = NULL) #to sort out the matrix


####LEt's try on Marshall Islands fish families x grp
str(mari)
mari$grp <- as.character(mari$grp)
n <- unique(mari$grp) #49/72
m <- unique(mari$family_cor) #36 families

x <- with(mari, table(mari$grp, mari$family_cor))
mari.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))

i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))

mari.matrix[i1] <- x[i1]


#Plot
plotweb(sortweb(mari.matrix, sort.order = "dec"),method="normal", empty = TRUE, arrow="no",text.rot=90,
        bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.2, ybig = 1.2, low.spacing = NULL) #to sort out the matrix




#####Template####

data(Safariland)
plotweb(Safariland)

# shorter labels
plotweb(Safariland, high.lablength=3, low.lablength=0, arrow="down")

# centered triangles for displaying interacions
plotweb(Safariland, text.rot=90, arrow="down.center", col.interaction="wheat2",
        y.lim=c(-1,2.5))

#orginal sequence, up arrows and different box width
plotweb(Safariland, method="normal", arrow="up", y.width.low=0.3, low.lablength=4)

# interactions as lines
plotweb(Safariland, arrow="both", y.width.low=0.05, text.rot=90, col.high="blue", 
        col.low="green")

# add an abundance vector for lower trophic species 
low.abun = round(runif(dim(Safariland)[1],1,40)) #create
names(low.abun) <- rownames(Safariland)
plotweb(Safariland, text.rot=90, low.abun=low.abun, col.interaction="purple", 
        y.width.low=0.05, y.width.high=0.05)

plotweb(Safariland, text.rot=90, low.abun=low.abun, col.interaction ="red", 
        bor.col.interaction="red", arrow="down")

# now vectors for all colours can be given, to mark certain species or 
# interactions. Colour vectors are recycled if not of appropriate length
plotweb(Safariland,col.high=c("orange","green"))

plotweb(Safariland,col.low=c("orange","green"),col.high=c("white","grey","purple"),
        text.high.col=c("blue","red"), col.interaction=c("red",rep("green",26),rep("brown",242)),
        bor.col.interaction=c(rep("green",26),rep("brown",242)),method="normal", 
        text.rot=90, low.lablength=10, high.lablength=5)


#example one (tritrophic)
plotweb(Safariland,y.width.low=0.1, y.width.high=0.05,method="normal", 
        y.lim=c(0,3), arrow="up", adj.high=c(0.5,1.5), col.high="orange",
        high.lablength=3,high.lab.dis=0)

plotweb(t(Safariland), y.width.low=0.05, y.width.high=0.1, method="normal",
        add=TRUE,low.y=1.5,high.y=2.5, col.low="green", text.low.col="red", 
        low.lab.dis=0, arrow="down", adj.low=c(0.5,1.1),low.plot=FALSE)

#example two (4 trophic with abundance)
low.abun = round(runif(dim(Safariland)[1],1,40)) #create
names(low.abun) <- rownames(Safariland)
plotweb(Safariland, text.rot=90, high.abun=low.abun, col.interaction="purple", 
        y.lim=c(0,4.5), high.lablength=0, arrow="up", method="normal", 
        y.width.high=0.05)

plotweb(t(Safariland), y.width.low=0.05, y.width.high=0.1, method="normal", 
        add=TRUE, low.y=1.7,high.y=2.7, col.low="green", text.low.col="black", 
        low.lab.dis=0, arrow="down", adj.low=c(0.5,1.1), low.lablength=4, 
        high.lablength=0)

plotweb(Safariland,y.width.low=0.05, y.width.high=0.1, method="normal", 
        add=TRUE, low.y=2.95, high.y=3.95, col.low="green", text.low.col="black", 
        low.lab.dis=0, arrow="down", adj.low=c(0.5,1.1), low.lablength=4)

# now some examples with the abuns.type-option:
plotweb(Safariland, abuns.type='independent',arrow="down.center")
plotweb(Safariland, abuns.type='additional',arrow="down.center")


visweb(Safariland)
visweb(Safariland, type="diagonal", square="compartment", text="none", 
       frame=TRUE)
visweb(Safariland, type="nested", text="compartment")
visweb(Safariland, circles=TRUE,  boxes=FALSE,  labsize=1, circle.max=3, 
       text="no")
visweb(Safariland,square="b",box.col="green",box.border="red")

#define your colors here,length has to be the numbers of different entries
cols <-0:(length(table(Safariland))-1) 
visweb(Safariland, square="defined", def.col=cols) 
