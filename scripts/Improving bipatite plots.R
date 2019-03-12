###########################
#Improving bipartite plots#
###########################
library(tidyverse)
library(bipartite)

#Getting one dataset per site
haw <- data_clean2 %>% filter(site_code=="haw")
vir <- data_clean2 %>% filter(site_code=="vir")
mad <- data_clean2 %>% filter(site_code=="mad")
mad <- data_clean2 %>% filter(site_code=="mad")

#Creating list of all families and prey items
n <- unique(data_clean2$item_class)
n <- n[order(n)]
m <- unique(data_clean2$family_cor) 
m<- m[order(m)] #alphabetic order 

    ####1. Phylum####
#Creating list of all families and prey items
n <- unique(data_clean2$item_phylum)
n <- n[order(n)]
m <- unique(data_clean2$family_cor) 
m<- m[order(m)] #alphabetic order 

#Virgin Islands
x <- with(vir, table(vir$item_phylum,vir$family_cor))
vir.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
vir.matrix[i1] <- x[i1]

dim(vir.matrix)

vir.matrix.std <- apply(vir.matrix, 2,function(x) x/sum(x))

vir.matrix.std[is.na(vir.matrix.std)] <- 0 #replace the NA values by 0

rownames(vir.matrix.std) #look for item as NA

vir.matrix.std <- vir.matrix.std[-23,] #removing items as NA

#Colors
col.fish <- data.frame(ifelse(colSums(vir.matrix.std)!= 0,"black","grey60")) #families with values = 0, grey color
colnames(col.fish) <- "color"
col.fish$color <- as.character(col.fish$color)

#col.phylum <- data.frame(ifelse(rowSums(vir.matrix.std)!= 0,box.colors,"grey60")) #if sum of rows = 0 (item_phylum), grey color
#colnames(col.phylum) <- "color"
#col.phylum$color <- as.character(col.phylum$color)

col.phylum <- unique(data.frame(data_clean2$item_phylum)) 
col.phylum <- data.frame(col.phylum[!is.na(col.phylum[1])])
names(col.phylum)[1] <- "taxon"

colfunc4 <- colorRampPalette(c("mediumpurple4","mediumorchid4","mediumblue","skyblue","lightseagreen","gold","coral1","lightpink2"))
colfunc4(22) #get 16 colors betweens those given into colorRampPalette
plot(rep(1,22),col=(colfunc4(22)),pch=19,cex=2) #plot to see the colours

col.phylum$colors <- colfunc4(22)
col.phylum$colors <- as.character(col.phylum$colors)

box.colors <- data.frame(ifelse(rowSums(vir.matrix.std)!= 0,col.phylum$colors,"grey60")) #if sum of rows = 0 (item_phylum), grey color
colnames(box.colors) <- "color"
box.colors$color <- as.character(box.colors$color)

#plot
plotweb(vir.matrix.std,method="normal", empty = FALSE, arrow="no",text.rot=90, col.interaction = "grey80",
        col.low = box.colors$color, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.1, ybig = 1.2, bor.col.low = box.colors$color, bor.col.high = col.fish$color) #empty = FALSE to keep the nodes even if no interactions


  #Marshall Islands
x <- with(mari, table(mari$item_phylum,mari$family_cor))
mari.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
mari.matrix[i1] <- x[i1]

dim(mari.matrix)

mari.matrix.std <- apply(mari.matrix, 2,function(x) x/sum(x))

mari.matrix.std[is.na(mari.matrix.std)] <- 0 #replace the NA values by 0

rownames(mari.matrix.std) #look for item as NA

mari.matrix.std <- mari.matrix.std[-23,] #removing items as NA

#Colors
col.fish <- data.frame(ifelse(colSums(mari.matrix.std)!= 0,"black","grey60")) #families with values = 0, grey color
colnames(col.fish) <- "color"
col.fish$color <- as.character(col.fish$color)

#col.phylum <- data.frame(ifelse(rowSums(mari.matrix.std)!= 0,box.colors,"grey60")) #if sum of rows = 0 (item_phylum), grey color
#colnames(col.phylum) <- "color"
#col.phylum$color <- as.character(col.phylum$color)

col.phylum <- unique(data.frame(data_clean2$item_phylum)) 
col.phylum <- data.frame(col.phylum[!is.na(col.phylum[1])])
names(col.phylum)[1] <- "taxon"

colfunc4 <- colorRampPalette(c("mediumpurple4","mediumorchid4","mediumblue","skyblue","lightseagreen","gold","coral1","lightpink2"))
colfunc4(22) #get 16 colors betweens those given into colorRampPalette
plot(rep(1,22),col=(colfunc4(22)),pch=19,cex=2) #plot to see the colours

col.phylum$colors <- colfunc4(22)
col.phylum$colors <- as.character(col.phylum$colors)

box.colors <- data.frame(ifelse(rowSums(mari.matrix.std)!= 0,col.phylum$colors,"grey60")) #if sum of rows = 0 (item_phylum), grey color
colnames(box.colors) <- "color"
box.colors$color <- as.character(box.colors$color)

#plot
plotweb(mari.matrix.std,method="normal", empty = FALSE, arrow="no",text.rot=90, col.interaction = "grey80",
        col.low = box.colors$color, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.1, ybig = 1.2, bor.col.low = box.colors$color, bor.col.high = col.fish$color)

  #Hawai
x <- with(haw, table(haw$item_phylum,haw$family_cor))
haw.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
haw.matrix[i1] <- x[i1]

dim(haw.matrix)

haw.matrix.std <- apply(haw.matrix, 2,function(x) x/sum(x))

haw.matrix.std[is.na(haw.matrix.std)] <- 0 #replace the NA values by 0

rownames(haw.matrix.std) #look for item as NA

haw.matrix.std <- haw.matrix.std[-23,] #removing items as NA

#Colors
col.fish <- data.frame(ifelse(colSums(haw.matrix.std)!= 0,"black","grey60")) #families with values = 0, grey color
colnames(col.fish) <- "color"
col.fish$color <- as.character(col.fish$color)

#col.phylum <- data.frame(ifelse(rowSums(haw.matrix.std)!= 0,box.colors,"grey60")) #if sum of rows = 0 (item_phylum), grey color
#colnames(col.phylum) <- "color"
#col.phylum$color <- as.character(col.phylum$color)

col.phylum <- unique(data.frame(data_clean2$item_phylum)) 
col.phylum <- data.frame(col.phylum[!is.na(col.phylum[1])])
names(col.phylum)[1] <- "taxon"

colfunc4 <- colorRampPalette(c("mediumpurple4","mediumorchid4","mediumblue","skyblue","lightseagreen","gold","coral1","lightpink2"))
colfunc4(22) #get 16 colors betweens those given into colorRampPalette
plot(rep(1,22),col=(colfunc4(22)),pch=19,cex=2) #plot to see the colours

col.phylum$colors <- colfunc4(22)
col.phylum$colors <- as.character(col.phylum$colors)

box.colors <- data.frame(ifelse(rowSums(haw.matrix.std)!= 0,col.phylum$colors,"grey60")) #if sum of rows = 0 (item_phylum), grey color
colnames(box.colors) <- "color"
box.colors$color <- as.character(box.colors$color)

#plot
plotweb(haw.matrix.std,method="normal", empty = FALSE, arrow="no",text.rot=90, col.interaction = "grey80",
        col.low = box.colors$color, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.1, ybig = 1.2, bor.col.low = box.colors$color, bor.col.high = col.fish$color)

  #Madagascar
x <- with(mad, table(mad$item_phylum,mad$family_cor))
mad.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
mad.matrix[i1] <- x[i1]

dim(mad.matrix)

mad.matrix.std <- apply(mad.matrix, 2,function(x) x/sum(x))

mad.matrix.std[is.na(mad.matrix.std)] <- 0 #replace the NA values by 0

rownames(mad.matrix.std) #look for item as NA

mad.matrix.std <- mad.matrix.std[-23,] #removing items as NA

#Colors
col.fish <- data.frame(ifelse(colSums(mad.matrix.std)!= 0,"black","grey60")) #families with values = 0, grey color
colnames(col.fish) <- "color"
col.fish$color <- as.character(col.fish$color)

#col.phylum <- data.frame(ifelse(rowSums(mad.matrix.std)!= 0,box.colors,"grey60")) #if sum of rows = 0 (item_phylum), grey color
#colnames(col.phylum) <- "color"
#col.phylum$color <- as.character(col.phylum$color)

col.phylum <- unique(data.frame(data_clean2$item_phylum)) 
col.phylum <- data.frame(col.phylum[!is.na(col.phylum[1])])
names(col.phylum)[1] <- "taxon"

colfunc4 <- colorRampPalette(c("mediumpurple4","mediumorchid4","mediumblue","skyblue","lightseagreen","gold","coral1","lightpink2"))
colfunc4(22) #get 16 colors betweens those given into colorRampPalette
plot(rep(1,22),col=(colfunc4(22)),pch=19,cex=2) #plot to see the colours

col.phylum$colors <- colfunc4(22)
col.phylum$colors <- as.character(col.phylum$colors)

box.colors <- data.frame(ifelse(rowSums(mad.matrix.std)!= 0,col.phylum$colors,"grey60")) #if sum of rows = 0 (item_phylum), grey color
colnames(box.colors) <- "color"
box.colors$color <- as.character(box.colors$color)

#plot
plotweb(mad.matrix.std,method="normal", empty = FALSE, arrow="no",text.rot=90, col.interaction = "grey80",
        col.low = box.colors$color, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.1, ybig = 1.2, bor.col.low = box.colors$color, bor.col.high = col.fish$color)




    ####2. Class#####
  #Virgin Islands
x <- with(vir, table(vir$item_class,vir$family_cor))
vir.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
vir.matrix[i1] <- x[i1]

dim(vir.matrix)

vir.matrix.std <- apply(vir.matrix, 2,function(x) x/sum(x))

vir.matrix.std[is.na(vir.matrix.std)] <- 0 #replace the NA values by 0

rownames(vir.matrix.std) #look for item as NA

vir.matrix.std <- vir.matrix.std[-42,] #removing items as NA

#Colors
col.fish <- data.frame(ifelse(colSums(vir.matrix.std)!= 0,"black","grey60")) #families with values = 0, grey color
colnames(col.fish) <- "color"
col.fish$color <- as.character(col.fish$color)

#col.class <- data.frame(ifelse(rowSums(vir.matrix.std)!= 0,box.colors,"grey60")) #if sum of rows = 0 (item_class), grey color
#colnames(col.class) <- "color"
#col.class$color <- as.character(col.class$color)

col.class <- unique(data.frame(data_clean2$item_class)) 
col.class <- data.frame(col.class[!is.na(col.class[1])])
names(col.class)[1] <- "taxon"

colfunc4 <- colorRampPalette(c("mediumpurple4","mediumorchid4","mediumblue","skyblue","lightseagreen","gold","coral1","lightpink2"))
colfunc4(41) #get 16 colors betweens those given into colorRampPalette
plot(rep(1,41),col=(colfunc4(41)),pch=19,cex=2) #plot to see the colours

col.class$colors <- colfunc4(41)
col.class$colors <- as.character(col.class$colors)

box.colors <- data.frame(ifelse(rowSums(vir.matrix.std)!= 0,col.class$colors,"grey60")) #if sum of rows = 0 (item_class), grey color
colnames(box.colors) <- "color"
box.colors$color <- as.character(box.colors$color)

#plot
plotweb(vir.matrix.std,method="normal", empty = FALSE, arrow="no",text.rot=90, col.interaction = "grey80",
        col.low = box.colors$color, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.1, ybig = 1.2, bor.col.low = box.colors$color, bor.col.high = col.fish$color) #empty = FALSE to keep the nodes even if no interactions


  #Marshall Islands
x <- with(mad, table(mad$item_class,mad$family_cor))
mad.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
mad.matrix[i1] <- x[i1]

dim(mad.matrix)

mad.matrix.std <- apply(mad.matrix, 2,function(x) x/sum(x))

mad.matrix.std[is.na(mad.matrix.std)] <- 0 #replace the NA values by 0

rownames(mad.matrix.std) #look for item as NA

mad.matrix.std <- mad.matrix.std[-42,] #removing items as NA

#Colors
col.fish <- data.frame(ifelse(colSums(mad.matrix.std)!= 0,"black","grey60")) #families with values = 0, grey color
colnames(col.fish) <- "color"
col.fish$color <- as.character(col.fish$color)

col.class <- unique(data.frame(data_clean2$item_class)) 
col.class <- data.frame(col.class[!is.na(col.class[1])])
names(col.class)[1] <- "taxon"

colfunc4 <- colorRampPalette(c("mediumpurple4","mediumorchid4","mediumblue","skyblue","lightseagreen","gold","coral1","lightpink2"))
colfunc4(41) #get 16 colors betweens those given into colorRampPalette
plot(rep(1,41),col=(colfunc4(41)),pch=19,cex=2) #plot to see the colours

col.class$colors <- colfunc4(41)
col.class$colors <- as.character(col.class$colors)

box.colors <- data.frame(ifelse(rowSums(mad.matrix.std)!= 0,col.class$colors,"grey60")) #if sum of rows = 0 (item_class), grey color
colnames(box.colors) <- "color"
box.colors$color <- as.character(box.colors$color)

#plot
plotweb(mad.matrix.std,method="normal", empty = FALSE, arrow="no",text.rot=90, col.interaction = "grey80",
        col.low = box.colors$color, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.1, ybig = 1.2, bor.col.low = box.colors$color, bor.col.high = col.fish$color)


  #Hawai
x <- with(haw, table(haw$item_class,haw$family_cor))
haw.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
haw.matrix[i1] <- x[i1]

dim(haw.matrix)

haw.matrix.std <- apply(haw.matrix, 2,function(x) x/sum(x))

haw.matrix.std[is.na(haw.matrix.std)] <- 0 #replace the NA values by 0

rownames(haw.matrix.std) #look for item as NA

haw.matrix.std <- haw.matrix.std[-42,] #removing items as NA

#Colors
col.fish <- data.frame(ifelse(colSums(haw.matrix.std)!= 0,"black","grey60")) #families with values = 0, grey color
colnames(col.fish) <- "color"
col.fish$color <- as.character(col.fish$color)

col.class <- unique(data.frame(data_clean2$item_class)) 
col.class <- data.frame(col.class[!is.na(col.class[1])])
names(col.class)[1] <- "taxon"

colfunc4 <- colorRampPalette(c("mediumpurple4","mediumorchid4","mediumblue","skyblue","lightseagreen","gold","coral1","lightpink2"))
colfunc4(41) #get 16 colors betweens those given into colorRampPalette
plot(rep(1,41),col=(colfunc4(41)),pch=19,cex=2) #plot to see the colours

col.class$colors <- colfunc4(41)
col.class$colors <- as.character(col.class$colors)

box.colors <- data.frame(ifelse(rowSums(haw.matrix.std)!= 0,col.class$colors,"grey60")) #if sum of rows = 0 (item_class), grey color
colnames(box.colors) <- "color"
box.colors$color <- as.character(box.colors$color)

#plot
plotweb(haw.matrix.std,method="normal", empty = FALSE, arrow="no",text.rot=90, col.interaction = "grey80",
        col.low = box.colors$color, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.1, ybig = 1.2, bor.col.low = box.colors$color, bor.col.high = col.fish$color)


  #Madagascar
x <- with(mad, table(mad$item_class,mad$family_cor))
mad.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
mad.matrix[i1] <- x[i1]

dim(mad.matrix)

mad.matrix.std <- apply(mad.matrix, 2,function(x) x/sum(x))

mad.matrix.std[is.na(mad.matrix.std)] <- 0 #replace the NA values by 0

rownames(mad.matrix.std) #look for item as NA

mad.matrix.std <- mad.matrix.std[-42,] #removing items as NA

#Colors
col.fish <- data.frame(ifelse(colSums(mad.matrix.std)!= 0,"black","grey60")) #families with values = 0, grey color
colnames(col.fish) <- "color"
col.fish$color <- as.character(col.fish$color)

col.class <- unique(data.frame(data_clean2$item_class)) 
col.class <- data.frame(col.class[!is.na(col.class[1])])
names(col.class)[1] <- "taxon"

colfunc4 <- colorRampPalette(c("mediumpurple4","mediumorchid4","mediumblue","skyblue","lightseagreen","gold","coral1","lightpink2"))
colfunc4(41) #get 16 colors betweens those given into colorRampPalette
plot(rep(1,41),col=(colfunc4(41)),pch=19,cex=2) #plot to see the colours

col.class$colors <- colfunc4(41)
col.class$colors <- as.character(col.class$colors)

box.colors <- data.frame(ifelse(rowSums(mad.matrix.std)!= 0,col.class$colors,"grey60")) #if sum of rows = 0 (item_class), grey color
colnames(box.colors) <- "color"
box.colors$color <- as.character(box.colors$color)

#plot
plotweb(mad.matrix.std,method="normal", empty = FALSE, arrow="no",text.rot=90, col.interaction = "grey80",
        col.low = box.colors$color, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.1, ybig = 1.2, bor.col.low = box.colors$color, bor.col.high = col.fish$color)



    
    


    ####3. Grp3####
  #Virgin Islands
n <- unique(data_clean2$grp3)
n <- n[order(n)]
m <- unique(data_clean2$family_cor) 
m<- m[order(m)] #alphabetic order 

x <- with(vir, table(vir$grp3,vir$family_cor))
vir.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
vir.matrix[i1] <- x[i1]

dim(vir.matrix)

vir.matrix.std <- apply(vir.matrix, 2,function(x) x/sum(x))

vir.matrix.std[is.na(vir.matrix.std)] <- 0 #replace the NA values by 0

rownames(vir.matrix.std) #look for item as NA

vir.matrix.std <- vir.matrix.std[-40,] #removing items as NA

#Colors
col.fish <- data.frame(ifelse(colSums(vir.matrix.std)!= 0,"black","grey60")) #families with values = 0, grey color
colnames(col.fish) <- "color"
col.fish$color <- as.character(col.fish$color)


col.grp3 <- unique(data.frame(data_clean2$grp3)) 
col.grp3 <- data.frame(col.grp3[!is.na(col.grp3[1])])
names(col.grp3)[1] <- "taxon"

colfunc4 <- colorRampPalette(c("mediumpurple4","mediumorchid4","mediumblue","skyblue","lightseagreen","gold","coral1","lightpink2"))
colfunc4(39) 
plot(rep(1,39),col=(colfunc4(39)),pch=19,cex=2) #plot to see the colours

col.grp3$colors <- colfunc4(39)
col.grp3$colors <- as.character(col.grp3$colors)

box.colors <- data.frame(ifelse(rowSums(vir.matrix.std)!= 0,col.grp3$colors,"grey60")) #if sum of rows = 0 (item_class), grey color
colnames(box.colors) <- "color"
box.colors$color <- as.character(box.colors$color)

#plot
plotweb(vir.matrix.std,method="normal", empty = FALSE, arrow="no",text.rot=90, col.interaction = "grey80",
        col.low = box.colors$color, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.10, ybig = 1.3, bor.col.low = box.colors$color, bor.col.high = col.fish$color)

  #Marshall Islands
n <- unique(data_clean2$grp3)
m <- unique(data_clean2$family_cor) 
m<- m[order(m)] #alphabetic order 

x <- with(mad, table(mad$grp3,mad$family_cor))
mad.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
mad.matrix[i1] <- x[i1]

dim(mad.matrix)

mad.matrix.std <- apply(mad.matrix, 2,function(x) x/sum(x))

mad.matrix.std[is.na(mad.matrix.std)] <- 0 #replace the NA values by 0

rownames(mad.matrix.std) #look for item as NA

mad.matrix.std <- mad.matrix.std[-35,] #removing items as NA

#Colors
col.fish <- data.frame(ifelse(colSums(mad.matrix.std)!= 0,"black","grey60")) #families with values = 0, grey color
colnames(col.fish) <- "color"
col.fish$color <- as.character(col.fish$color)

col.grp3 <- unique(data.frame(data_clean2$grp3)) 
col.grp3 <- data.frame(col.grp3[!is.na(col.grp3[1])])
names(col.grp3)[1] <- "taxon"

colfunc4 <- colorRampPalette(c("mediumpurple4","mediumorchid4","mediumblue","skyblue","lightseagreen","gold","coral1","lightpink2"))
colfunc4(42) #get 16 colors betweens those given into colorRampPalette
plot(rep(1,42),col=(colfunc4(42)),pch=19,cex=2) #plot to see the colours

col.grp3$colors <- colfunc4(42)
col.grp3$colors <- as.character(col.grp3$colors)

box.colors <- data.frame(ifelse(rowSums(mad.matrix.std)!= 0,col.grp3$colors,"grey60")) #if sum of rows = 0 (item_class), grey color
colnames(box.colors) <- "color"
box.colors$color <- as.character(box.colors$color)

#plot
plotweb(mad.matrix.std,method="normal", empty = FALSE, arrow="no",text.rot=90, col.interaction = "grey80",
        col.low = box.colors$color, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.10, ybig = 1.3, bor.col.low = box.colors$color, bor.col.high = col.fish$color)

