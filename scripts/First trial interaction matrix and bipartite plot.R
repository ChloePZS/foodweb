###############################
# Trying the bipartite network#
###############################
library(bipartite)
library(fossil)
library(tidyverse)

#Create data frame per site
haw <- data2 %>% filter(site_code=="haw")
vir <- data2 %>% filter(site_code=="vir")
mari <- data2 %>% filter(site_code=="mari")
mad <- data2 %>% filter(site_code=="mad")

haw <- as.data.frame(haw)
vir <- as.data.frame(vir)
mari <- as.data.frame(mari)
mad <- as.data.frame(mad)

          ####Simple occurence matrix for each site at grp, phylum levels####

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
    ####Fish families x Phylum####
  #Virgin Islands
#First need to build an interaction matrix on fish families and phylum 
str(vir)
n <- unique(vir$item_phylum) #18 phylum + 1 NA
m <- unique(vir$family_cor) #213 fish species, 56 families

x <- with(vir, table(vir$item_phylum,vir$family_cor))
vir.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
vir.matrix[i1] <- x[i1]

#Standardize nb of links by dividing by the sum of the cols(here fish families)
dim(vir.matrix)
#vir.matrix.std <- vir.matrix/colSums(vir.matrix)

vir.matrix.std <- apply(vir.matrix, 2,function(x) x/sum(x)) #divie each cell by the sum of its column 2 means by column

#Let's do the plot
#plotweb(sortweb(vir.matrix, sort.order="dec"), text.rot = 90, method="normal", arrow="down") #to sort out the matrix
#Get interactions and box colors
phylum.all <- as.data.frame(vir[c(3,9)]) #select fish fam and item phylum col
phylum.unique <- unique(phylum.all) #get unique rows
colnames(phylum.unique) <- c("Taxon", "Phyla")
phylum <- as.data.frame(unique(phylum.unique$Phyla))
phyla <- as.data.frame(phylum[-5,]) #remove NA phylum
colnames(phyla) <- "Phyla"
phyla2 <- as.data.frame(phyla[order(phyla$Phyla),]) #Alphabetic order
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

#with Jordan's FUN arguments
plotweb(vir.matrix.std,method="normal", empty = TRUE, arrow="down.center",text.rot=90,
        col.low = box.colors, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.20, ybig = 1.1) #to sort out the matrix
#can't get the proper colours for each interactions  : color of interaction according to box colour !!

  #Marshall Islands#
#First need to build an interaction matrix on fish families and phylum 
str(mari)
n <- unique(mari$item_phylum) 
m <- unique(mari$family_cor) 

x <- with(mari, table(mari$item_phylum,mari$family_cor))
mari.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
mari.matrix[i1] <- x[i1]

mari.matrix.std <- apply(mari.matrix, 2,function(x) x/sum(x))

#Prepare data frame for color function PHYLUM
phylum.all <- as.data.frame(mari[c(3,9)])
phylum.unique <- unique(phylum.all)
colnames(phylum.unique) <- c("Taxon", "Phyla")
phylum <- as.data.frame(unique(phylum.unique$Phyla))
phyla <- as.data.frame(phylum[-2,])
colnames(phyla) <- "Phyla"
phyla2 <- as.data.frame(phyla[order(phyla$Phyla),])
colnames(phyla2) <- "Phyla"

colfunc4 <- colorRampPalette(c("mediumpurple4","mediumblue","skyblue","lightseagreen","gold","coral1","gray85","black"))
colfunc4(14) 
plot(rep(1,14),col=(colfunc4(14)),pch=19,cex=2) #plot to see the colours

phyla2$colors <- colfunc4(14)

colorvector <- as.data.frame(merge(phyla2, phylum.unique, by = "Phyla"))
names(colorvector)[2] <- "colors"
box.colors <- as.character(phyla2$colors)
colorvec1 <- as.character(colorvector$colors)
head(colorvec1)
colfunc4

#with Jordan's fun arguments
plotweb(mari.matrix.std,method="normal", empty = TRUE, arrow="down",text.rot=90,
        col.low = box.colors, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.2, ybig = 1.2, low.spacing = NULL) #to sort out the matrix

  #Hawai#
#First need to build an interaction matrix on fish families and phylum 
str(haw)
n <- unique(haw$item_phylum) 
m <- unique(haw$family_cor) 

x <- with(haw, table(haw$item_phylum,haw$family_cor))
haw.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
haw.matrix[i1] <- x[i1]

haw.matrix.std <- apply(haw.matrix, 2,function(x) x/sum(x))

#Prepare data frame for color function PHYLUM
phylum.all <- as.data.frame(haw[c(3,9)])
phylum.unique <- unique(phylum.all)
colnames(phylum.unique) <- c("Taxon", "Phyla")
phylum <- as.data.frame(unique(phylum.unique$Phyla))
phyla <- as.data.frame(phylum[-2,])
colnames(phyla) <- "Phyla"
phyla2 <- as.data.frame(phyla[order(phyla$Phyla),])
colnames(phyla2) <- "Phyla"

colfunc4 <- colorRampPalette(c("mediumpurple4","mediumblue","skyblue","lightseagreen","gold","coral1","gray85","black"))
colfunc4(13) 
plot(rep(1,13),col=(colfunc4(13)),pch=19,cex=2) #plot to see the colours

phyla2$colors <- colfunc4(13)

colorvector <- as.data.frame(merge(phyla2, phylum.unique, by = "Phyla"))
names(colorvector)[2] <- "colors"
box.colors <- as.character(phyla2$colors)
colorvec1 <- as.character(colorvector$colors)
head(colorvec1)
colfunc4

#with Jordan's fun arguments
plotweb(haw.matrix.std,method="normal", empty = TRUE, arrow="down",text.rot=90,
        col.low = box.colors, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.2, ybig = 1.2, low.spacing = NULL) #to sort out the matrix

#Madagascar#
#First need to build an interaction matrix on fish families and phylum 
str(mad)
n <- unique(mad$item_phylum) 
m <- unique(mad$family_cor) 

x <- with(mad, table(mad$item_phylum,mad$family_cor))
mad.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
mad.matrix[i1] <- x[i1]

mad.matrix.std <- apply(mad.matrix, 2,function(x) x/sum(x))

#Prepare data frame for color function PHYLUM
phylum.all <- as.data.frame(mad[c(3,9)])
phylum.unique <- unique(phylum.all)
colnames(phylum.unique) <- c("Taxon", "Phyla")
phylum <- as.data.frame(unique(phylum.unique$Phyla))
phyla <- as.data.frame(phylum[-3,])
colnames(phyla) <- "Phyla"
phyla2 <- as.data.frame(phyla[order(phyla$Phyla),])
colnames(phyla2) <- "Phyla"

colfunc4 <- colorRampPalette(c("mediumpurple4","mediumblue","skyblue","lightseagreen","gold","coral1","gray85","black"))
colfunc4(11) 
plot(rep(1,11),col=(colfunc4(11)),pch=19,cex=2) #plot to see the colours

phyla2$colors <- colfunc4(11)

colorvector <- as.data.frame(merge(phyla2, phylum.unique, by = "Phyla"))
names(colorvector)[2] <- "colors"
box.colors <- as.character(phyla2$colors)
colorvec1 <- as.character(colorvector$colors)
head(colorvec1)
colfunc4

#with Jordan's fun arguments
plotweb(mad.matrix.std,method="normal", empty = TRUE, arrow="down",text.rot=90,
        col.low = box.colors, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.2, ybig = 1.2, low.spacing = NULL) #to sort out the matrix


    ####Fish families x Class ####
#Virgin Islands
#First need to build an interaction matrix on fish families and phylum 
str(vir)
n <- unique(vir$item_class) 
m <- unique(vir$family_cor) 

x <- with(vir, table(vir$item_class,vir$family_cor))
vir.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
vir.matrix[i1] <- x[i1]

#Standardize nb of links by dividing by the sum of the cols(here fish families)
dim(vir.matrix)
#vir.matrix.std <- vir.matrix/colSums(vir.matrix)

vir.matrix.std <- apply(vir.matrix, 2,function(x) x/sum(x)) #divie each cell by the sum of its column 2 means by column

#Let's do the plot
#plotweb(sortweb(vir.matrix, sort.order="dec"), text.rot = 90, method="normal", arrow="down") #to sort out the matrix
#Get interactions and box colors
class.all <- as.data.frame(vir[c(3,10)]) #select fish fam and item class col
class.unique <- unique(class.all) #get unique rows
colnames(class.unique) <- c("Taxon", "Class")
class <- as.data.frame(unique(class.unique$Class))
class <- as.data.frame(class[-5,]) #remove NA phylum
colnames(class) <- "Class"
class2 <- as.data.frame(class[order(class$Class),]) #Alphabetic order
colnames(class2) <- "Class"

colfunc4 <- colorRampPalette(c("mediumpurple4","mediumblue","skyblue","lightseagreen","gold","coral1","gray85","black"))
colfunc4(34) #get 16 colors betweens those given into colorRampPalette
plot(rep(1,34),col=(colfunc4(34)),pch=19,cex=2) #plot to see the colours

class2$colors <- colfunc4(34)

colorvector <- as.data.frame(merge(class2, class.unique, by = "Class"))
names(colorvector)[2] <- "colors"
box.colors <- as.character(class2$colors)
colorvec1 <- as.character(colorvector$colors)
head(colorvec1)
colfunc4

#with Jordan's FUN arguments
plotweb(vir.matrix.std,method="normal", empty = TRUE, arrow="no",text.rot=90,
        col.low = box.colors, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.20, ybig = 1.1) #to sort out the matrix
#can't get the proper colours for each interactions  : color of interaction according to box colour !!

  #Marshall Islands#
#First need to build an interaction matrix on fish families and phylum 
str(mari)
n <- unique(mari$item_class) 
m <- unique(mari$family_cor) 

x <- with(mari, table(mari$item_class,mari$family_cor))
mari.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
mari.matrix[i1] <- x[i1]

#Standardize nb of links by dividing by the sum of the cols(here fish families)
dim(mari.matrix)
#mari.matrix.std <- mari.matrix/colSums(mari.matrix)

mari.matrix.std <- apply(mari.matrix, 2,function(x) x/sum(x)) #divie each cell by the sum of its column 2 means by column

#Let's do the plot
#plotweb(sortweb(mari.matrix, sort.order="dec"), text.rot = 90, method="normal", arrow="down") #to sort out the matrix
#Get interactions and box colors
class.all <- as.data.frame(mari[c(3,10)]) #select fish fam and item class col
class.unique <- unique(class.all) #get unique rows
colnames(class.unique) <- c("Taxon", "Class")
class <- as.data.frame(unique(class.unique$Class))
class <- as.data.frame(class[-2,]) #remove NA phylum
colnames(class) <- "Class"
class2 <- as.data.frame(class[order(class$Class),]) #Alphabetic order
colnames(class2) <- "Class"

colfunc4 <- colorRampPalette(c("mediumpurple4","mediumblue","skyblue","lightseagreen","gold","coral1","gray85","black"))
colfunc4(23) #get 16 colors betweens those given into colorRampPalette
plot(rep(1,23),col=(colfunc4(23)),pch=19,cex=2) #plot to see the colours

class2$colors <- colfunc4(23)

colorvector <- as.data.frame(merge(class2, class.unique, by = "Class"))
names(colorvector)[2] <- "colors"
box.colors <- as.character(class2$colors)
colorvec1 <- as.character(colorvector$colors)
head(colorvec1)
colfunc4

#with Jordan's FUN arguments
plotweb(mari.matrix.std,method="normal", empty = TRUE, arrow="no",text.rot=90,
        col.low = box.colors, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.20, ybig = 1.1, bor.col.low = box.colors) #to sort out the matrix
#can't get the proper colours for each interactions  : color of interaction according to box colour !!

  #Hawai#
#First need to build an interaction matrix on fish families and phylum 
str(haw)
n <- unique(haw$item_class) 
m <- unique(haw$family_cor) 

x <- with(haw, table(haw$item_class,haw$family_cor))
haw.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
haw.matrix[i1] <- x[i1]

#Standardize nb of links by dividing by the sum of the cols(here fish families)
dim(haw.matrix)
#haw.matrix.std <- haw.matrix/colSums(haw.matrix)

haw.matrix.std <- apply(haw.matrix, 2,function(x) x/sum(x)) #divie each cell by the sum of its column 2 means by column

#Let's do the plot
#plotweb(sortweb(haw.matrix, sort.order="dec"), text.rot = 90, method="normal", arrow="down") #to sort out the matrix
#Get interactions and box colors
class.all <- as.data.frame(haw[c(3,10)]) #select fish fam and item class col
class.unique <- unique(class.all) #get unique rows
colnames(class.unique) <- c("Taxon", "Class")
class <- as.data.frame(unique(class.unique$Class))
class <- as.data.frame(class[-2,]) #remove NA phylum
colnames(class) <- "Class"
class2 <- as.data.frame(class[order(class$Class),]) #Alphabetic order
colnames(class2) <- "Class"

colfunc4 <- colorRampPalette(c("mediumpurple4","mediumblue","skyblue","lightseagreen","gold","coral1","gray85","black"))
colfunc4(23) #get 16 colors betweens those given into colorRampPalette
plot(rep(1,23),col=(colfunc4(23)),pch=19,cex=2) #plot to see the colours

class2$colors <- colfunc4(23)

colorvector <- as.data.frame(merge(class2, class.unique, by = "Class"))
names(colorvector)[2] <- "colors"
box.colors <- as.character(class2$colors)
colorvec1 <- as.character(colorvector$colors)
head(colorvec1)
colfunc4

#with Jordan's FUN arguments
plotweb(haw.matrix.std,method="normal", empty = TRUE, arrow="no",text.rot=90,
        col.low = box.colors, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.20, ybig = 1.1, bor.col.low = box.colors) #to sort out the matrix

  #Madagascar#
#First need to build an interaction matrix on fish families and phylum 
str(mad)
n <- unique(mad$item_class) 
m <- unique(mad$family_cor) 

x <- with(mad, table(mad$item_class,mad$family_cor))
mad.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
mad.matrix[i1] <- x[i1]

#Standardize nb of links by dividing by the sum of the cols(here fish families)
dim(mad.matrix)
#mad.matrix.std <- mad.matrix/colSums(mad.matrix)

mad.matrix.std <- apply(mad.matrix, 2,function(x) x/sum(x)) #divie each cell by the sum of its column 2 means by column

#Let's do the plot
#plotweb(sortweb(mad.matrix, sort.order="dec"), text.rot = 90, method="normal", arrow="down") #to sort out the matrix
#Get interactions and box colors
class.all <- as.data.frame(mad[c(3,10)]) #select fish fam and item class col
class.unique <- unique(class.all) #get unique rows
colnames(class.unique) <- c("Taxon", "Class")
class <- as.data.frame(unique(class.unique$Class))
class <- as.data.frame(class[-3,]) #remove NA phylum
colnames(class) <- "Class"
class2 <- as.data.frame(class[order(class$Class),]) #Alphabetic order
colnames(class2) <- "Class"

colfunc4 <- colorRampPalette(c("mediumpurple4","mediumblue","skyblue","lightseagreen","gold","coral1","gray85","black"))
colfunc4(21) 
plot(rep(1,21),col=(colfunc4(21)),pch=19,cex=2) #plot to see the colours

class2$colors <- colfunc4(21)

colorvector <- as.data.frame(merge(class2, class.unique, by = "Class"))
names(colorvector)[2] <- "colors"
box.colors <- as.character(class2$colors)
colorvec1 <- as.character(colorvector$colors)
head(colorvec1)
colfunc4

#with Jordan's FUN arguments
plotweb(mad.matrix.std,method="normal", empty = TRUE, arrow="no",text.rot=90,
        col.low = box.colors, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.20, ybig = 1.1, bor.col.low = box.colors) #to sort out the matrix

  

    ####Fish families x Group 1####

  #Virgin Islands
#First need to build an interaction matrix on fish families and phylum 
str(vir)
n <- unique(vir$grp1) 
m <- unique(vir$family_cor) 

c <- count(vir, grp1)

x <- with(vir, table(vir$grp1,vir$family_cor))
vir.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
vir.matrix[i1] <- x[i1]

#Standardize nb of links by dividing by the sum of the cols(here fish families)
dim(vir.matrix)
#vir.matrix.std <- vir.matrix/colSums(vir.matrix)

vir.matrix.std <- apply(vir.matrix, 2,function(x) x/sum(x)) #divie each cell by the sum of its column 2 means by column

#Let's do the plot
#plotweb(sortweb(vir.matrix, sort.order="dec"), text.rot = 90, method="normal", arrow="down") #to sort out the matrix
#Get interactions and box colors
grp1.all <- as.data.frame(vir[,c(3,33)]) #select fish fam and item grp1 col
grp1.unique <- unique(grp1.all) #get unique rows
colnames(grp1.unique) <- c("Taxon", "grp")
grp1 <- as.data.frame(unique(grp1.unique$grp))
colnames(grp1) <- "grp"
grp1.ord <- as.data.frame(grp1[order(grp1$grp),]) #Alphabetic order
colnames(grp1.ord) <- "grp"

colfunc4 <- colorRampPalette(c("mediumpurple4","mediumblue","skyblue","lightseagreen","gold","coral1","gray85","black"))
colfunc4(48) #get 16 colors betweens those given into colorRampPalette
plot(rep(1,48),col=(colfunc4(48)),pch=19,cex=2) #plot to see the colours

grp1.ord$colors <- colfunc4(48)

colorvector <- as.data.frame(merge(grp1.ord, grp1.unique, by = "grp"))
names(colorvector)[2] <- "colors"
box.colors <- as.character(grp1.ord$colors)
colorvec1 <- as.character(colorvector$colors)
head(colorvec1)
colfunc4

#with Jordan's FUN arguments
plotweb(vir.matrix.std,method="normal", empty = TRUE, arrow="no",text.rot=90,
        col.low = box.colors, bor.col.interaction = FALSE, y.width.low=0.05, y.width.high=0.03,
        labsize = 1.20, ybig = 1.1, bor.col.low = box.colors) 

plotweb(sortweb(vir.matrix.std,sort.ord="dec"), method="normal", empty = TRUE, arrow="no",text.rot=90,
        col.low = box.colors, bor.col.interaction = FALSE, y.width.low=0.05, y.width.high=0.03,
        labsize = 1.20, ybig = 1.1, bor.col.low = box.colors) #to sort out by rows and cols total

  #Marshall Islands
#First need to build an interaction matrix on fish families and phylum 
str(mari)
n <- unique(mari$grp1) 
m <- unique(mari$family_cor) 

c <- count(mari, grp1)

x <- with(mari, table(mari$grp1,mari$family_cor))
mari.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
mari.matrix[i1] <- x[i1]

#Standardize nb of links by dividing by the sum of the cols(here fish families)
dim(mari.matrix)
#mari.matrix.std <- mari.matrix/colSums(mari.matrix)

mari.matrix.std <- apply(mari.matrix, 2,function(x) x/sum(x)) #divie each cell by the sum of its column 2 means by column

#Let's do the plot
#plotweb(sortweb(mari.matrix, sort.order="dec"), text.rot = 90, method="normal", arrow="down") #to sort out the matrix
#Get interactions and box colors
grp1.all <- as.data.frame(mari[,c(3,33)]) #select fish fam and item grp1 col
grp1.unique <- unique(grp1.all) #get unique rows
colnames(grp1.unique) <- c("Taxon", "grp")
grp1 <- as.data.frame(unique(grp1.unique$grp))
colnames(grp1) <- "grp"
grp1.ord <- as.data.frame(grp1[order(grp1$grp),]) #Alphabetic order
colnames(grp1.ord) <- "grp"

colfunc4 <- colorRampPalette(c("mediumpurple4","mediumblue","skyblue","lightseagreen","gold","coral1","gray85","black"))
colfunc4(38) #get 16 colors betweens those given into colorRampPalette
plot(rep(1,38),col=(colfunc4(38)),pch=19,cex=2) #plot to see the colours

grp1.ord$colors <- colfunc4(38)

colorvector <- as.data.frame(merge(grp1.ord, grp1.unique, by = "grp"))
names(colorvector)[2] <- "colors"
box.colors <- as.character(grp1.ord$colors)
colorvec1 <- as.character(colorvector$colors)
head(colorvec1)
colfunc4

#with Jordan's FUN arguments
plotweb(mari.matrix.std,method="normal", empty = TRUE, arrow="no",text.rot=90,
        col.low = box.colors, bor.col.interaction = FALSE, y.width.low=0.05, y.width.high=0.03,
        labsize = 1.20, ybig = 1.1, bor.col.low = box.colors) 

plotweb(sortweb(mari.matrix.std, sort.ord="dec"), method="normal", empty = TRUE, arrow="no",text.rot=90,
        col.low = box.colors, bor.col.interaction = FALSE, y.width.low=0.05, y.width.high=0.03,
        labsize = 1.20, ybig = 1.1, bor.col.low = box.colors) #to sort out the matrix

  #Hawai
#First need to build an interaction matrix on fish families and phylum 
str(haw)
n <- unique(haw$grp1) 
m <- unique(haw$family_cor) 

c <- count(haw, grp1)

x <- with(haw, table(haw$grp1,haw$family_cor))
haw.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
haw.matrix[i1] <- x[i1]

#Standardize nb of links by dividing by the sum of the cols(here fish families)
dim(haw.matrix)
#haw.matrix.std <- haw.matrix/colSums(haw.matrix)

haw.matrix.std <- apply(haw.matrix, 2,function(x) x/sum(x)) #divie each cell by the sum of its column 2 means by column

#Let's do the plot
#plotweb(sortweb(haw.matrix, sort.order="dec"), text.rot = 90, method="normal", arrow="down") #to sort out the matrix
#Get interactions and box colors
grp1.all <- as.data.frame(haw[,c(3,33)]) #select fish fam and item grp1 col
grp1.unique <- unique(grp1.all) #get unique rows
colnames(grp1.unique) <- c("Taxon", "grp")
grp1 <- as.data.frame(unique(grp1.unique$grp))
colnames(grp1) <- "grp"
grp1.ord <- as.data.frame(grp1[order(grp1$grp),]) #Alphabetic order
colnames(grp1.ord) <- "grp"

colfunc4 <- colorRampPalette(c("mediumpurple4","mediumblue","skyblue","lightseagreen","gold","coral1","gray85","black"))
colfunc4(41) #get 16 colors betweens those given into colorRampPalette
plot(rep(1,41),col=(colfunc4(41)),pch=19,cex=2) #plot to see the colours

grp1.ord$colors <- colfunc4(41)

colorvector <- as.data.frame(merge(grp1.ord, grp1.unique, by = "grp"))
names(colorvector)[2] <- "colors"
box.colors <- as.character(grp1.ord$colors)
colorvec1 <- as.character(colorvector$colors)
head(colorvec1)
colfunc4

#with Jordan's FUN arguments
plotweb(haw.matrix.std,method="normal", empty = TRUE, arrow="no",text.rot=90,
        col.low = box.colors, bor.col.interaction = FALSE, y.width.low=0.05, y.width.high=0.03,
        labsize = 1.20, ybig = 1.1, bor.col.low = box.colors) #to sort out the matrix

plotweb(sortweb(haw.matrix.std, sort.ord="dec"), method="normal", empty = TRUE, arrow="no",text.rot=90,
        col.low = box.colors, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.20, ybig = 1.1, bor.col.low = box.colors) #to sort out the matrix

  #Madagascar
#First need to build an interaction matrix on fish families and phylum 
str(mad)
n <- unique(mad$grp1) 
m <- unique(mad$family_cor) 

c <- count(mad, grp1)

x <- with(mad, table(mad$grp1,mad$family_cor))
mad.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
mad.matrix[i1] <- x[i1]

#Standardize nb of links by dividing by the sum of the cols(here fish families)
dim(mad.matrix)
#mad.matrix.std <- mad.matrix/colSums(mad.matrix)

mad.matrix.std <- apply(mad.matrix, 2,function(x) x/sum(x)) #divie each cell by the sum of its column 2 means by column

#Let's do the plot
#plotweb(sortweb(mad.matrix, sort.order="dec"), text.rot = 90, method="normal", arrow="down") #to sort out the matrix
#Get interactions and box colors
grp1.all <- as.data.frame(mad[,c(3,33)]) #select fish fam and item grp1 col
grp1.unique <- unique(grp1.all) #get unique rows
colnames(grp1.unique) <- c("Taxon", "grp")
grp1 <- as.data.frame(unique(grp1.unique$grp))
colnames(grp1) <- "grp"
grp1.ord <- as.data.frame(grp1[order(grp1$grp),]) #Alphabetic order
colnames(grp1.ord) <- "grp"

colfunc4 <- colorRampPalette(c("mediumpurple4","mediumblue","skyblue","lightseagreen","gold","coral1","gray85","black"))
colfunc4(39) #get 16 colors betweens those given into colorRampPalette
plot(rep(1,39),col=(colfunc4(39)),pch=19,cex=2) #plot to see the colours

grp1.ord$colors <- colfunc4(39)

colorvector <- as.data.frame(merge(grp1.ord, grp1.unique, by = "grp"))
names(colorvector)[2] <- "colors"
box.colors <- as.character(grp1.ord$colors)
colorvec1 <- as.character(colorvector$colors)
head(colorvec1)
colfunc4

#with Jordan's FUN arguments
plotweb(mad.matrix.std,method="normal", empty = TRUE, arrow="no",text.rot=90,
        col.low = box.colors, bor.col.interaction = FALSE, y.width.low=0.05, y.width.high=0.03,
        labsize = 1.20, ybig = 1.1, bor.col.low = box.colors) #to sort out the matrix

plotweb(sortweb(mad.matrix.std, sort.ord="dec"), method="normal", empty = TRUE, arrow="no",text.rot=90,
        col.low = box.colors, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.20, ybig = 1.1, bor.col.low = box.colors) #to sort out the matrix

    ####Fish families x Group 2####

  #Virgin Islands
#First need to build an interaction matrix on fish families and phylum 
str(vir)
n <- unique(vir$grp2) 
m <- unique(vir$family_cor) 

c <- count(vir, grp2)

x <- with(vir, table(vir$grp2,vir$family_cor))
vir.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
vir.matrix[i1] <- x[i1]

#Standardize nb of links by dividing by the sum of the cols(here fish families)
dim(vir.matrix)
#vir.matrix.std <- vir.matrix/colSums(vir.matrix)

vir.matrix.std <- apply(vir.matrix, 2,function(x) x/sum(x)) #divie each cell by the sum of its column 2 means by column

#Let's do the plot
#plotweb(sortweb(vir.matrix, sort.order="dec"), text.rot = 90, method="normal", arrow="down") #to sort out the matrix
#Get interactions and box colors
grp2.all <- as.data.frame(vir[,c(3,34)]) #select fish fam and item grp2 col
grp2.unique <- unique(grp2.all) #get unique rows
colnames(grp2.unique) <- c("Taxon", "grp")
grp2 <- as.data.frame(unique(grp2.unique$grp))
colnames(grp2) <- "grp"
grp2.ord <- as.data.frame(grp2[order(grp2$grp),]) #Alphabetic order
colnames(grp2.ord) <- "grp"

colfunc4 <- colorRampPalette(c("mediumpurple4","mediumblue","skyblue","lightseagreen","gold","coral1","gray85","black"))
colfunc4(57) #get 16 colors betweens those given into colorRampPalette
plot(rep(1,57),col=(colfunc4(57)),pch=19,cex=2) #plot to see the colours

grp2.ord$colors <- colfunc4(57)

colorvector <- as.data.frame(merge(grp2.ord, grp2.unique, by = "grp"))
names(colorvector)[2] <- "colors"
box.colors <- as.character(grp2.ord$colors)
colorvec1 <- as.character(colorvector$colors)
head(colorvec1)
colfunc4

#with Jordan's FUN arguments
plotweb(vir.matrix.std,method="normal", empty = TRUE, arrow="no",text.rot=90,
        col.low = box.colors, bor.col.interaction = FALSE, y.width.low=0.05, y.width.high=0.03,
        labsize = 1.20, ybig = 1.1, bor.col.low = box.colors) 

plotweb(sortweb(vir.matrix.std,sort.ord="dec"), method="normal", empty = TRUE, arrow="no",text.rot=90,
        col.low = box.colors, bor.col.interaction = FALSE, y.width.low=0.05, y.width.high=0.03,
        labsize = 1.20, ybig = 1.1, bor.col.low = box.colors) #to sort out by rows and cols total

  #Marshall Islands
#First need to build an interaction matrix on fish families and phylum 
str(mari)
n <- unique(mari$grp2) 
m <- unique(mari$family_cor) 

c <- count(mari, grp2)

x <- with(mari, table(mari$grp2,mari$family_cor))
mari.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
mari.matrix[i1] <- x[i1]

#Standardize nb of links by dividing by the sum of the cols(here fish families)
dim(mari.matrix)
#mari.matrix.std <- mari.matrix/colSums(mari.matrix)

mari.matrix.std <- apply(mari.matrix, 2,function(x) x/sum(x)) #divie each cell by the sum of its column 2 means by column

#Let's do the plot
#plotweb(sortweb(mari.matrix, sort.order="dec"), text.rot = 90, method="normal", arrow="down") #to sort out the matrix
#Get interactions and box colors
grp2.all <- as.data.frame(mari[,c(3,34)]) #select fish fam and item grp2 col
grp2.unique <- unique(grp2.all) #get unique rows
colnames(grp2.unique) <- c("Taxon", "grp")
grp2 <- as.data.frame(unique(grp2.unique$grp))
colnames(grp2) <- "grp"
grp2.ord <- as.data.frame(grp2[order(grp2$grp),]) #Alphabetic order
colnames(grp2.ord) <- "grp"

colfunc4 <- colorRampPalette(c("mediumpurple4","mediumblue","skyblue","lightseagreen","gold","coral1","gray85","black"))
colfunc4(42) #get 16 colors betweens those given into colorRampPalette
plot(rep(1,42),col=(colfunc4(42)),pch=19,cex=2) #plot to see the colours

grp2.ord$colors <- colfunc4(42)

colorvector <- as.data.frame(merge(grp2.ord, grp2.unique, by = "grp"))
names(colorvector)[2] <- "colors"
box.colors <- as.character(grp2.ord$colors)
colorvec1 <- as.character(colorvector$colors)
head(colorvec1)
colfunc4

#with Jordan's FUN arguments
plotweb(mari.matrix.std,method="normal", empty = TRUE, arrow="no",text.rot=90,
        col.low = box.colors, bor.col.interaction = FALSE, y.width.low=0.05, y.width.high=0.03,
        labsize = 1.20, ybig = 1.1, bor.col.low = box.colors) 

plotweb(sortweb(mari.matrix.std,sort.ord="dec"), method="normal", empty = TRUE, arrow="no",text.rot=90,
        col.low = box.colors, bor.col.interaction = FALSE, y.width.low=0.05, y.width.high=0.03,
        labsize = 1.20, ybig = 1.1, bor.col.low = box.colors) #to sort out by rows and cols total

  #Hawai
#First need to build an interaction matrix on fish families and phylum 
str(haw)
n <- unique(haw$grp2) 
m <- unique(haw$family_cor) 

c <- count(haw, grp2)

x <- with(haw, table(haw$grp2,haw$family_cor))
haw.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
haw.matrix[i1] <- x[i1]

#Standardize nb of links by dividing by the sum of the cols(here fish families)
dim(haw.matrix)
#haw.matrix.std <- haw.matrix/colSums(haw.matrix)

haw.matrix.std <- apply(haw.matrix, 2,function(x) x/sum(x)) #divie each cell by the sum of its column 2 means by column

#Let's do the plot
#plotweb(sortweb(haw.matrix, sort.order="dec"), text.rot = 90, method="normal", arrow="down") #to sort out the matrix
#Get interactions and box colors
grp2.all <- as.data.frame(haw[,c(3,34)]) #select fish fam and item grp2 col
grp2.unique <- unique(grp2.all) #get unique rows
colnames(grp2.unique) <- c("Taxon", "grp")
grp2 <- as.data.frame(unique(grp2.unique$grp))
colnames(grp2) <- "grp"
grp2.ord <- as.data.frame(grp2[order(grp2$grp),]) #Alphabetic order
colnames(grp2.ord) <- "grp"

colfunc4 <- colorRampPalette(c("mediumpurple4","mediumblue","skyblue","lightseagreen","gold","coral1","gray85","black"))
colfunc4(46) #get 16 colors betweens those given into colorRampPalette
plot(rep(1,46),col=(colfunc4(46)),pch=19,cex=2) #plot to see the colours

grp2.ord$colors <- colfunc4(46)

colorvector <- as.data.frame(merge(grp2.ord, grp2.unique, by = "grp"))
names(colorvector)[2] <- "colors"
box.colors <- as.character(grp2.ord$colors)
colorvec1 <- as.character(colorvector$colors)
head(colorvec1)
colfunc4

#with Jordan's FUN arguments
plotweb(haw.matrix.std,method="normal", empty = TRUE, arrow="no",text.rot=90,
        col.low = box.colors, bor.col.interaction = FALSE, y.width.low=0.05, y.width.high=0.03,
        labsize = 1.20, ybig = 1.1, bor.col.low = box.colors) 

plotweb(sortweb(haw.matrix.std,sort.ord="dec"), method="normal", empty = TRUE, arrow="no",text.rot=90,
        col.low = box.colors, bor.col.interaction = FALSE, y.width.low=0.05, y.width.high=0.03,
        labsize = 1.20, ybig = 1.1, bor.col.low = box.colors) #to sort out by rows and cols total

  #Madagascar
#First need to build an interaction matrix on fish families and phylum 
str(mad)
n <- unique(mad$grp2) 
m <- unique(mad$family_cor) 

c <- count(mad, grp2)

x <- with(mad, table(mad$grp2,mad$family_cor))
mad.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
mad.matrix[i1] <- x[i1]

#Standardize nb of links by dividing by the sum of the cols(here fish families)
dim(mad.matrix)
#mad.matrix.std <- mad.matrix/colSums(mad.matrix)

mad.matrix.std <- apply(mad.matrix, 2,function(x) x/sum(x)) #divie each cell by the sum of its column 2 means by column

#Let's do the plot
#plotweb(sortweb(mad.matrix, sort.order="dec"), text.rot = 90, method="normal", arrow="down") #to sort out the matrix
#Get interactions and box colors
grp2.all <- as.data.frame(mad[,c(3,34)]) #select fish fam and item grp2 col
grp2.unique <- unique(grp2.all) #get unique rows
colnames(grp2.unique) <- c("Taxon", "grp")
grp2 <- as.data.frame(unique(grp2.unique$grp))
colnames(grp2) <- "grp"
grp2.ord <- as.data.frame(grp2[order(grp2$grp),]) #Alphabetic order
colnames(grp2.ord) <- "grp"

colfunc4 <- colorRampPalette(c("mediumpurple4","mediumblue","skyblue","lightseagreen","gold","coral1","gray85","black"))
colfunc4(48) #get 16 colors betweens those given into colorRampPalette
plot(rep(1,48),col=(colfunc4(48)),pch=19,cex=2) #plot to see the colours

grp2.ord$colors <- colfunc4(48)

colorvector <- as.data.frame(merge(grp2.ord, grp2.unique, by = "grp"))
names(colorvector)[2] <- "colors"
box.colors <- as.character(grp2.ord$colors)
colorvec1 <- as.character(colorvector$colors)
head(colorvec1)
colfunc4

#with Jordan's FUN arguments
plotweb(mad.matrix.std,method="normal", empty = TRUE, arrow="no",text.rot=90,
        col.low = box.colors, bor.col.interaction = FALSE, y.width.low=0.05, y.width.high=0.03,
        labsize = 1.20, ybig = 1.1, bor.col.low = box.colors) 

plotweb(sortweb(mad.matrix.std,sort.ord="dec"), method="normal", empty = TRUE, arrow="no",text.rot=90,
        col.low = box.colors, bor.col.interaction = FALSE, y.width.low=0.05, y.width.high=0.03,
        labsize = 1.20, ybig = 1.1, bor.col.low = box.colors) #to sort out by rows and cols total


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
