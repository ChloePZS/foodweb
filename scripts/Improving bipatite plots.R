###########################
#Improving bipartite plots#
###########################
library(tidyverse)
library(bipartite)

#Getting one dataset per site
haw <- data_clean2 %>% filter(site_code=="haw")
vir <- data_clean2 %>% filter(site_code=="vir")
mari <- data_clean2 %>% filter(site_code=="mari")
mad <- data_clean2 %>% filter(site_code=="mad")


    ####Phylum####
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

vale.std <- vir.matrix/colSums(vir.matrix)
vale.std[is.na(vale.std)] <- 0

colSums(vir.matrix.std)

colSums(vale.std)

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




    ####Class#####
n <- unique(data_clean2$item_class)
n <- n[order(n)]
m <- unique(data_clean2$family_cor) 
m<- m[order(m)] #alphabetic order 

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


    ####Grp3####
n <- unique(data_clean2$grp3)
n <- n[order(n)]
m <- unique(data_clean2$family_cor) 
m<- m[order(m)] #alphabetic order  

  #Virgin Islands
x <- with(vir, table(vir$grp3,vir$family_cor))
vir.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
vir.matrix[i1] <- x[i1]

dim(vir.matrix)

vir.matrix.std <- apply(vir.matrix, 2,function(x) x/sum(x))

vir.matrix.std[is.na(vir.matrix.std)] <- 0 #replace the NA values by 0

rownames(vir.matrix.std) #look for item as NA

vir.matrix.std <- vir.matrix.std[-41,] #removing items as NA

#Colors
col.fish <- data.frame(ifelse(colSums(vir.matrix.std)!= 0,"black","grey60")) #families with values = 0, grey color
colnames(col.fish) <- "color"
col.fish$color <- as.character(col.fish$color)

col.grp3 <- unique(data.frame(data_clean2$grp3)) 
col.grp3 <- data.frame(col.grp3[!is.na(col.grp3[1])])
names(col.grp3)[1] <- "taxon"

colfunc4 <- colorRampPalette(c("mediumpurple4","mediumorchid4","mediumblue","skyblue","lightseagreen","gold","coral1","lightpink2"))
colfunc4(40) 
plot(rep(1,40),col=(colfunc4(40)),pch=19,cex=2) #plot to see the colours

col.grp3$colors <- colfunc4(40)
col.grp3$colors <- as.character(col.grp3$colors)

box.colors <- data.frame(ifelse(rowSums(vir.matrix.std)!= 0,col.grp3$colors,"grey60")) #if sum of rows = 0 (item_class), grey color
colnames(box.colors) <- "color"
box.colors$color <- as.character(box.colors$color)

#plot
plotweb(vir.matrix.std,method="normal", empty = FALSE, arrow="no",text.rot=90, col.interaction = "grey80",
        col.low = box.colors$color, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.10, ybig = 1.3, bor.col.low = box.colors$color, bor.col.high = col.fish$color)

  #Madagascar
x <- with(mad, table(mad$grp3,mad$family_cor))
mad.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
mad.matrix[i1] <- x[i1]

dim(mad.matrix)

mad.matrix.std <- apply(mad.matrix, 2,function(x) x/sum(x))

mad.matrix.std[is.na(mad.matrix.std)] <- 0 #replace the NA values by 0

rownames(mad.matrix.std) #look for item as NA

mad.matrix.std <- mad.matrix.std[-41,] #removing items as NA

#Colors
col.fish <- data.frame(ifelse(colSums(mad.matrix.std)!= 0,"black","grey60")) #families with values = 0, grey color
colnames(col.fish) <- "color"
col.fish$color <- as.character(col.fish$color)

col.grp3 <- unique(data.frame(data_clean2$grp3)) 
col.grp3 <- data.frame(col.grp3[!is.na(col.grp3[1])])
names(col.grp3)[1] <- "taxon"

colfunc4 <- colorRampPalette(c("mediumpurple4","mediumorchid4","mediumblue","skyblue","lightseagreen","gold","coral1","lightpink2"))
colfunc4(40)
plot(rep(1,40),col=(colfunc4(40)),pch=19,cex=2) #plot to see the colours

col.grp3$colors <- colfunc4(40)
col.grp3$colors <- as.character(col.grp3$colors)

box.colors <- data.frame(ifelse(rowSums(mad.matrix.std)!= 0,col.grp3$colors,"grey60")) #if sum of rows = 0 (item_class), grey color
colnames(box.colors) <- "color"
box.colors$color <- as.character(box.colors$color)

#plot
plotweb(mad.matrix.std,method="normal", empty = FALSE, arrow="no",text.rot=90, col.interaction = "grey80",
        col.low = box.colors$color, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.10, ybig = 1.3, bor.col.low = box.colors$color, bor.col.high = col.fish$color)


  #Marshall islands
x <- with(mari, table(mari$grp3,mari$family_cor))
mari.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
mari.matrix[i1] <- x[i1]

dim(mari.matrix)

mari.matrix.std <- apply(mari.matrix, 2,function(x) x/sum(x))

mari.matrix.std[is.na(mari.matrix.std)] <- 0 #replace the NA values by 0

rownames(mari.matrix.std) #look for item as NA

mari.matrix.std <- mari.matrix.std[-41,] #removing items as NA

#Colors
col.fish <- data.frame(ifelse(colSums(mari.matrix.std)!= 0,"black","grey60")) #families with values = 0, grey color
colnames(col.fish) <- "color"
col.fish$color <- as.character(col.fish$color)

col.grp3 <- unique(data.frame(data_clean2$grp3)) 
col.grp3 <- data.frame(col.grp3[!is.na(col.grp3[1])])
names(col.grp3)[1] <- "taxon"

colfunc4 <- colorRampPalette(c("mediumpurple4","mediumorchid4","mediumblue","skyblue","lightseagreen","gold","coral1","lightpink2"))
colfunc4(40)
plot(rep(1,40),col=(colfunc4(40)),pch=19,cex=2) #plot to see the colours

col.grp3$colors <- colfunc4(40)
col.grp3$colors <- as.character(col.grp3$colors)

box.colors <- data.frame(ifelse(rowSums(mari.matrix.std)!= 0,col.grp3$colors,"grey60")) #if sum of rows = 0 (item_class), grey color
colnames(box.colors) <- "color"
box.colors$color <- as.character(box.colors$color)

#plot
plotweb(mari.matrix.std,method="normal", empty = FALSE, arrow="no",text.rot=90, col.interaction = "grey80",
        col.low = box.colors$color, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.10, ybig = 1.3, bor.col.low = box.colors$color, bor.col.high = col.fish$color)


    #Hawai
x <- with(haw, table(haw$grp3,haw$family_cor))
haw.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
haw.matrix[i1] <- x[i1]

dim(haw.matrix)

haw.matrix.std <- apply(haw.matrix, 2,function(x) x/sum(x))

haw.matrix.std[is.na(haw.matrix.std)] <- 0 #replace the NA values by 0

rownames(haw.matrix.std) #look for item as NA

haw.matrix.std <- haw.matrix.std[-41,] #removing items as NA

#Colors
col.fish <- data.frame(ifelse(colSums(haw.matrix.std)!= 0,"black","grey60")) #families with values = 0, grey color
colnames(col.fish) <- "color"
col.fish$color <- as.character(col.fish$color)

col.grp3 <- unique(data.frame(data_clean2$grp3)) 
col.grp3 <- data.frame(col.grp3[!is.na(col.grp3[1])])
names(col.grp3)[1] <- "taxon"

colfunc4 <- colorRampPalette(c("mediumpurple4","mediumorchid4","mediumblue","skyblue","lightseagreen","gold","coral1","lightpink2"))
colfunc4(40)
plot(rep(1,40),col=(colfunc4(40)),pch=19,cex=2) #plot to see the colours

col.grp3$colors <- colfunc4(40)
col.grp3$colors <- as.character(col.grp3$colors)

box.colors <- data.frame(ifelse(rowSums(haw.matrix.std)!= 0,col.grp3$colors,"grey60")) #if sum of rows = 0 (item_class), grey color
colnames(box.colors) <- "color"
box.colors$color <- as.character(box.colors$color)

#plot
plotweb(haw.matrix.std,method="normal", empty = FALSE, arrow="no",text.rot=90, col.interaction = "grey80",
        col.low = box.colors$color, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.10, ybig = 1.3, bor.col.low = box.colors$color, bor.col.high = col.fish$color)

    ####Grp1####
n <- unique(data_clean2$grp1)
n <- n[order(n)]
m <- unique(data_clean2$family_cor) 
m<- m[order(m)] #alphabetic order  

#Virgin Islands
x <- with(vir, table(vir$grp1,vir$family_cor))
vir.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
vir.matrix[i1] <- x[i1]

dim(vir.matrix)

vir.matrix.std <- apply(vir.matrix, 2,function(x) x/sum(x))

vir.matrix.std[is.na(vir.matrix.std)] <- 0 #replace the NA values by 0

rownames(vir.matrix.std) #look for item as NA --> no NA


#Colors
col.fish <- data.frame(ifelse(colSums(vir.matrix.std)!= 0,"black","grey60")) #families with values = 0, grey color
colnames(col.fish) <- "color"
col.fish$color <- as.character(col.fish$color)

col.grp1 <- unique(data.frame(data_clean2$grp1)) 
col.grp1 <- data.frame(col.grp1[!is.na(col.grp1[1])])
names(col.grp1)[1] <- "taxon"

colfunc4 <- colorRampPalette(c("mediumpurple4","mediumorchid4","mediumblue","skyblue","lightseagreen","gold","coral1","lightpink2"))
colfunc4(50) 
plot(rep(1,50),col=(colfunc4(50)),pch=19,cex=2) #plot to see the colours

col.grp1$colors <- colfunc4(50)
col.grp1$colors <- as.character(col.grp1$colors)

box.colors <- data.frame(ifelse(rowSums(vir.matrix.std)!= 0,col.grp1$colors,"grey60")) #if sum of rows = 0 (item_class), grey color
colnames(box.colors) <- "color"
box.colors$color <- as.character(box.colors$color)

#plot
plotweb(vir.matrix.std,method="normal", empty = FALSE, arrow="no",text.rot=90, col.interaction = "grey80",
        col.low = box.colors$color, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.10, ybig = 1.3, bor.col.low = box.colors$color, bor.col.high = col.fish$color)



  #Marshall Islands
x <- with(mari, table(mari$grp1,mari$family_cor))
mari.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
mari.matrix[i1] <- x[i1]

dim(mari.matrix)

mari.matrix.std <- apply(mari.matrix, 2,function(x) x/sum(x))

mari.matrix.std[is.na(mari.matrix.std)] <- 0 #replace the NA values by 0

rownames(mari.matrix.std) #look for item as NA --> no NA


#Colors
col.fish <- data.frame(ifelse(colSums(mari.matrix.std)!= 0,"black","grey60")) #families with values = 0, grey color
colnames(col.fish) <- "color"
col.fish$color <- as.character(col.fish$color)

col.grp1 <- unique(data.frame(data_clean2$grp1)) 
col.grp1 <- data.frame(col.grp1[!is.na(col.grp1[1])])
names(col.grp1)[1] <- "taxon"

colfunc4 <- colorRampPalette(c("mediumpurple4","mediumorchid4","mediumblue","skyblue","lightseagreen","gold","coral1","lightpink2"))
colfunc4(50) 
plot(rep(1,50),col=(colfunc4(50)),pch=19,cex=2) #plot to see the colours

col.grp1$colors <- colfunc4(50)
col.grp1$colors <- as.character(col.grp1$colors)

box.colors <- data.frame(ifelse(rowSums(mari.matrix.std)!= 0,col.grp1$colors,"grey60")) #if sum of rows = 0 (item_class), grey color
colnames(box.colors) <- "color"
box.colors$color <- as.character(box.colors$color)

#plot
plotweb(mari.matrix.std,method="normal", empty = FALSE, arrow="no",text.rot=90, col.interaction = "grey80",
        col.low = box.colors$color, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.10, ybig = 1.3, bor.col.low = box.colors$color, bor.col.high = col.fish$color)


  #Hawai
x <- with(haw, table(haw$grp1,haw$family_cor))
haw.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
haw.matrix[i1] <- x[i1]

dim(haw.matrix)

haw.matrix.std <- apply(haw.matrix, 2,function(x) x/sum(x))

haw.matrix.std[is.na(haw.matrix.std)] <- 0 #replace the NA values by 0

rownames(haw.matrix.std) #look for item as NA --> no NA


#Colors
col.fish <- data.frame(ifelse(colSums(haw.matrix.std)!= 0,"black","grey60")) #families with values = 0, grey color
colnames(col.fish) <- "color"
col.fish$color <- as.character(col.fish$color)

col.grp1 <- unique(data.frame(data_clean2$grp1)) 
col.grp1 <- data.frame(col.grp1[!is.na(col.grp1[1])])
names(col.grp1)[1] <- "taxon"

colfunc4 <- colorRampPalette(c("mediumpurple4","mediumorchid4","mediumblue","skyblue","lightseagreen","gold","coral1","lightpink2"))
colfunc4(50) 
plot(rep(1,50),col=(colfunc4(50)),pch=19,cex=2) #plot to see the colours

col.grp1$colors <- colfunc4(50)
col.grp1$colors <- as.character(col.grp1$colors)

box.colors <- data.frame(ifelse(rowSums(haw.matrix.std)!= 0,col.grp1$colors,"grey60")) #if sum of rows = 0 (item_class), grey color
colnames(box.colors) <- "color"
box.colors$color <- as.character(box.colors$color)

#plot
plotweb(haw.matrix.std,method="normal", empty = FALSE, arrow="no",text.rot=90, col.interaction = "grey80",
        col.low = box.colors$color, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.10, ybig = 1.3, bor.col.low = box.colors$color, bor.col.high = col.fish$color)



  #Madagascar
x <- with(mad, table(mad$grp1,mad$family_cor))
mad.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
mad.matrix[i1] <- x[i1]

dim(mad.matrix)

mad.matrix.std <- apply(mad.matrix, 2,function(x) x/sum(x))

mad.matrix.std[is.na(mad.matrix.std)] <- 0 #replace the NA values by 0

rownames(mad.matrix.std) #look for item as NA --> no NA


#Colors
col.fish <- data.frame(ifelse(colSums(mad.matrix.std)!= 0,"black","grey60")) #families with values = 0, grey color
colnames(col.fish) <- "color"
col.fish$color <- as.character(col.fish$color)

col.grp1 <- unique(data.frame(data_clean2$grp1)) 
col.grp1 <- data.frame(col.grp1[!is.na(col.grp1[1])])
names(col.grp1)[1] <- "taxon"

colfunc4 <- colorRampPalette(c("mediumpurple4","mediumorchid4","mediumblue","skyblue","lightseagreen","gold","coral1","lightpink2"))
colfunc4(50) 
plot(rep(1,50),col=(colfunc4(50)),pch=19,cex=2) #plot to see the colours

col.grp1$colors <- colfunc4(50)
col.grp1$colors <- as.character(col.grp1$colors)

box.colors <- data.frame(ifelse(rowSums(mad.matrix.std)!= 0,col.grp1$colors,"grey60")) #if sum of rows = 0 (item_class), grey color
colnames(box.colors) <- "color"
box.colors$color <- as.character(box.colors$color)

#plot
plotweb(mad.matrix.std,method="normal", empty = FALSE, arrow="no",text.rot=90, col.interaction = "grey80",
        col.low = box.colors$color, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.10, ybig = 1.3, bor.col.low = box.colors$color, bor.col.high = col.fish$color)

    ####Grp4####
n <- unique(data_clean2$grp4)
n <- n[order(n)]
m <- unique(data_clean2$family_cor) 
m<- m[order(m)] #alphabetic order  

  #Virgin Islands
x <- with(vir, table(vir$grp4,vir$family_cor))
vir.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
vir.matrix[i1] <- x[i1]

dim(vir.matrix)

vir.matrix.std <- apply(vir.matrix, 2,function(x) x/sum(x))

vir.matrix.std[is.na(vir.matrix.std)] <- 0 #replace the NA values by 0

rownames(vir.matrix.std) #look for item as NA --> no NA

#Colors
col.fish <- data.frame(ifelse(colSums(vir.matrix.std)!= 0,"black","grey60")) #families with values = 0, grey color
colnames(col.fish) <- "color"
col.fish$color <- as.character(col.fish$color)

col.grp4 <- unique(data.frame(data_clean2$grp4)) 
col.grp4 <- data.frame(col.grp4[!is.na(col.grp4[1])])
names(col.grp4)[1] <- "taxon"

colfunc4 <- colorRampPalette(c("mediumpurple4","mediumorchid4","mediumblue","skyblue","lightseagreen","gold","coral1","lightpink2"))
colfunc4(68) 
plot(rep(1,68),col=(colfunc4(68)),pch=19,cex=2) #plot to see the colours

col.grp4$colors <- colfunc4(68)
col.grp4$colors <- as.character(col.grp4$colors)

box.colors <- data.frame(ifelse(rowSums(vir.matrix.std)!= 0,col.grp4$colors,"grey60")) #if sum of rows = 0 (item_class), grey color
colnames(box.colors) <- "color"
box.colors$color <- as.character(box.colors$color)

#plot
plotweb(vir.matrix.std,method="normal", empty = FALSE, arrow="no",text.rot=90, col.interaction = "grey80",
        col.low = box.colors$color, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.10, ybig = 1.3, bor.col.low = box.colors$color, bor.col.high = col.fish$color)

  #Marshall Islands
x <- with(mari, table(mari$grp4,mari$family_cor))
mari.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
mari.matrix[i1] <- x[i1]

dim(mari.matrix)

mari.matrix.std <- apply(mari.matrix, 2,function(x) x/sum(x))

mari.matrix.std[is.na(mari.matrix.std)] <- 0 #replace the NA values by 0

rownames(mari.matrix.std) #look for item as NA --> no NA

#Colors
col.fish <- data.frame(ifelse(colSums(mari.matrix.std)!= 0,"black","grey60")) #families with values = 0, grey color
colnames(col.fish) <- "color"
col.fish$color <- as.character(col.fish$color)

col.grp4 <- unique(data.frame(data_clean2$grp4)) 
col.grp4 <- data.frame(col.grp4[!is.na(col.grp4[1])])
names(col.grp4)[1] <- "taxon"

colfunc4 <- colorRampPalette(c("mediumpurple4","mediumorchid4","mediumblue","skyblue","lightseagreen","gold","coral1","lightpink2"))
colfunc4(68) 
plot(rep(1,68),col=(colfunc4(68)),pch=19,cex=2) #plot to see the colours

col.grp4$colors <- colfunc4(68)
col.grp4$colors <- as.character(col.grp4$colors)

box.colors <- data.frame(ifelse(rowSums(mari.matrix.std)!= 0,col.grp4$colors,"grey60")) #if sum of rows = 0 (item_class), grey color
colnames(box.colors) <- "color"
box.colors$color <- as.character(box.colors$color)

#plot
plotweb(mari.matrix.std,method="normal", empty = FALSE, arrow="no",text.rot=90, col.interaction = "grey80",
        col.low = box.colors$color, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.10, ybig = 1.3, bor.col.low = box.colors$color, bor.col.high = col.fish$color)


  #Hawai
x <- with(haw, table(haw$grp4,haw$family_cor))
haw.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
haw.matrix[i1] <- x[i1]

dim(haw.matrix)

haw.matrix.std <- apply(haw.matrix, 2,function(x) x/sum(x))

haw.matrix.std[is.na(haw.matrix.std)] <- 0 #replace the NA values by 0

rownames(haw.matrix.std) #look for item as NA --> no NA

#Colors
col.fish <- data.frame(ifelse(colSums(haw.matrix.std)!= 0,"black","grey60")) #families with values = 0, grey color
colnames(col.fish) <- "color"
col.fish$color <- as.character(col.fish$color)

col.grp4 <- unique(data.frame(data_clean2$grp4)) 
col.grp4 <- data.frame(col.grp4[!is.na(col.grp4[1])])
names(col.grp4)[1] <- "taxon"

colfunc4 <- colorRampPalette(c("mediumpurple4","mediumorchid4","mediumblue","skyblue","lightseagreen","gold","coral1","lightpink2"))
colfunc4(68) 
plot(rep(1,68),col=(colfunc4(68)),pch=19,cex=2) #plot to see the colours

col.grp4$colors <- colfunc4(68)
col.grp4$colors <- as.character(col.grp4$colors)

box.colors <- data.frame(ifelse(rowSums(haw.matrix.std)!= 0,col.grp4$colors,"grey60")) #if sum of rows = 0 (item_class), grey color
colnames(box.colors) <- "color"
box.colors$color <- as.character(box.colors$color)

#plot
plotweb(haw.matrix.std,method="normal", empty = FALSE, arrow="no",text.rot=90, col.interaction = "grey80",
        col.low = box.colors$color, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.10, ybig = 1.3, bor.col.low = box.colors$color, bor.col.high = col.fish$color)

  #Madagascar
x <- with(mad, table(mad$grp4,mad$family_cor))
mad.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
mad.matrix[i1] <- x[i1]

dim(mad.matrix)

mad.matrix.std <- apply(mad.matrix, 2,function(x) x/sum(x))

mad.matrix.std[is.na(mad.matrix.std)] <- 0 #replace the NA values by 0

rownames(mad.matrix.std) #look for item as NA --> no NA

#Colors
col.fish <- data.frame(ifelse(colSums(mad.matrix.std)!= 0,"black","grey60")) #families with values = 0, grey color
colnames(col.fish) <- "color"
col.fish$color <- as.character(col.fish$color)

col.grp4 <- unique(data.frame(data_clean2$grp4)) 
col.grp4 <- data.frame(col.grp4[!is.na(col.grp4[1])])
names(col.grp4)[1] <- "taxon"

colfunc4 <- colorRampPalette(c("mediumpurple4","mediumorchid4","mediumblue","skyblue","lightseagreen","gold","coral1","lightpink2"))
colfunc4(68) 
plot(rep(1,68),col=(colfunc4(68)),pch=19,cex=2) #plot to see the colours

col.grp4$colors <- colfunc4(68)
col.grp4$colors <- as.character(col.grp4$colors)

box.colors <- data.frame(ifelse(rowSums(mad.matrix.std)!= 0,col.grp4$colors,"grey60")) #if sum of rows = 0 (item_class), grey color
colnames(box.colors) <- "color"
box.colors$color <- as.character(box.colors$color)

#plot
plotweb(mad.matrix.std,method="normal", empty = FALSE, arrow="no",text.rot=90, col.interaction = "grey80",
        col.low = box.colors$color, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.10, ybig = 1.3, bor.col.low = box.colors$color, bor.col.high = col.fish$color)

