####################################
#Re-do matrices and bipartite plots#
####################################

library(tidyverse)
library(bipartite)
library(reshape2)

#Getting one dataset per site
haw <- data_full2 %>% filter(site_code=="haw")
vir <- data_full2 %>% filter(site_code=="vir")
mari <- data_full2 %>% filter(site_code=="mari")
mad <- data_full2 %>% filter(site_code=="mad")
nca <- data_full2 %>% filter(site_code=="nca")


####Grp5####
  #Try this way
n <- unique(data_full2$grp5)
n <- n[order(n)]
m <- unique(data_full2$fish_sp) 
m<- m[order(m)] #alphabetic order 

#New Caledonia
x <- with(nca, table(nca$grp5,nca$fish_sp))
nca.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
nca.matrix[i1] <- x[i1]

nca.matrix[nca.matrix > 1] <- 1  #All values > 1 = 1, to get a binary matrix
rownames(nca.matrix)
nca.matrix <- nca.matrix[-56,]

#Now need to get the sum by families
nca2 <- melt(nca.matrix)
nca2 <- nca2 %>% dplyr::rename (grp5 = Var1, fish_sp = Var2) 

nca2 <- left_join(nca2, data_full2[,c("fish_sp", "family")], by="fish_sp") 
nca2 <- unique(nca2)

nca2 <- nca2 %>% group_by(family, grp5) %>% #sum of value by family
  mutate(sum_fam = sum(value)) 

nca2 %>% filter(family=="Acanthuridae", grp5 == "Actinopterygii")

nca2 <- nca2 %>% select(-fish_sp, -value)
nca2 <- unique(nca2)

nca.matrix.fam <- acast(nca2, grp5 ~ family , value.var = "sum_fam")
nca.matrix.fam.std <- apply(nca.matrix.fam, 2,function(x) x/sum(x))

#Check families in common for all five sites : 90 families in total, only 17 in common in all three sites
intersect_all <- function(a,b,...){
  Reduce(intersect, list(a,b,...))
}

fam_com <- intersect_all(nca$family, haw$family, vir$family, mad$family, mari$family) #Only 17 families in common between the 5 locations
fam_dif <- unique(data_full2$family[!data_full2$family %in% intersect_all(nca$family, haw$family, vir$family, mad$family, mari$family)])

nca.matrix.fam2 <- nca.matrix.fam
nca.matrix.fam2[colnames(nca.matrix.fam2) %in% fam_dif] <- 0 #doesn't work

colnames(nca.matrix.fam2) %in% fam_com
colSums(nca.matrix.fam)
colSums(nca.matrix.fam2)

#Get the plots (then put 0 to uncommon families)
#Colors
col.fish <- data.frame(ifelse(colSums(nca.matrix.fam)!= 0,"black","grey60")) #families with values = 0, grey color
colnames(col.fish) <- "color"
col.fish$color <- as.character(col.fish$color)

col.grp5 <- unique(data.frame(data_full2$grp5)) 
col.grp5 <- data.frame(col.grp5[!is.na(col.grp5[1])])
names(col.grp5)[1] <- "taxon"

colfunc4 <- colorRampPalette(c("darkorchid4","slateblue4","turquoise1","lightseagreen","gold","coral1","lightpink2"))

col.grp5$colors <- colfunc4(55)
col.grp5$colors <- as.character(col.grp5$colors)

box.colors <- data.frame(ifelse(rowSums(nca.matrix.fam)!= 0,col.grp5$colors,"grey60")) #if sum of rows = 0 (item_class), grey color
colnames(box.colors) <- "color"
box.colors$color <- as.character(box.colors$color)

#plot
plotweb(nca.matrix.fam,method="normal", empty = FALSE, arrow="no",text.rot=90, col.interaction = "grey80",
        col.low = box.colors$color, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.10, ybig = 1.3, bor.col.low = box.colors$color, bor.col.high = col.fish$color)




####Grp7####
#Try this way
n <- unique(data_full2$grp7)
n <- n[order(n)]
m <- unique(data_full2$fish_sp) 
m<- m[order(m)] #alphabetic order 

#New Caledonia
x <- with(nca, table(nca$grp7,nca$fish_sp))
nca.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
nca.matrix[i1] <- x[i1]

nca.matrix[nca.matrix > 1] <- 1  #All values > 1 = 1, to get a binary matrix
rownames(nca.matrix)
nca.matrix <- nca.matrix[-28,]

#Now need to get the sum by families
nca_grp7 <- melt(nca.matrix)
nca_grp7 <- nca_grp7 %>% dplyr::rename (grp7 = Var1, fish_sp = Var2) 

nca_grp7 <- left_join(nca_grp7, data_full2[,c("fish_sp", "family")], by="fish_sp") 
nca_grp7 <- unique(nca_grp7)

nca_grp7 <- nca_grp7 %>% group_by(family, grp7) %>% #sum of value by family
  mutate(sum_fam = sum(value)) 

nca_grp7 %>% filter(family=="Acanthuridae", grp7 == "Actinopterygii")

nca_grp7 <- nca_grp7 %>% select(-fish_sp, -value)
nca_grp7 <- unique(nca_grp7)

nca.matrix.fam <- reshape2::acast(nca_grp7, grp7 ~ family , value.var = "sum_fam")
nca.matrix.fam.std <- apply(nca.matrix.fam, 2,function(x) x/sum(x))

#Check families in common for all five sites : 90 families in total, only 17 in common in all three sites
intersect_all <- function(a,b,...){
  Reduce(intersect, list(a,b,...))
}

fam_com <- intersect_all(nca$family, haw$family, vir$family, mad$family, mari$family) #Only 17 families in common between the 5 locations
fam_dif <- unique(data_full2$family[!data_full2$family %in% intersect_all(nca$family, haw$family, vir$family, mad$family, mari$family)])

nca.matrix.fam2 <- nca.matrix.fam
nca.matrix.fam2[colnames(nca.matrix.fam2) %in% fam_dif] <- 0 #doesn't work

colnames(nca.matrix.fam2) %in% fam_com
colSums(nca.matrix.fam)
colSums(nca.matrix.fam2)

#Get the plots (then put 0 to uncommon families)
#Colors
col.fish <- data.frame(ifelse(colSums(nca.matrix.fam)!= 0,"black","grey60")) #families with values = 0, grey color
colnames(col.fish) <- "color"
col.fish$color <- as.character(col.fish$color)

col.grp7 <- unique(data.frame(data_full2$grp7)) 
col.grp7 <- data.frame(col.grp7[!is.na(col.grp7[1])])
names(col.grp7)[1] <- "taxon"

colfunc4 <- colorRampPalette(c("darkorchid4","slateblue4","turquoise1","lightseagreen","gold","coral1","lightpink2"))

col.grp7$colors <- colfunc4(27)
col.grp7$colors <- as.character(col.grp7$colors)

box.colors <- data.frame(ifelse(rowSums(nca.matrix.fam)!= 0,col.grp7$colors,"grey60")) #if sum of rows = 0 (item_class), grey color
colnames(box.colors) <- "color"
box.colors$color <- as.character(box.colors$color)

#plot
plotweb(nca.matrix.fam,method="normal", empty = FALSE, arrow="no",text.rot=90, col.interaction = "grey80",
        col.low = box.colors$color, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.10, ybig = 1.3, bor.col.low = box.colors$color, bor.col.high = col.fish$color)



 

#Best group seems to the Grp6

    ####Grp6####
n <- unique(data_full2$grp6)
n <- n[order(n)]
m <- unique(data_full2$fish_sp) 
m<- m[order(m)] #alphabetic order 

  #New Caledonia
x <- with(nca, table(nca$grp6,nca$fish_sp))
nca.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
nca.matrix[i1] <- x[i1]

nca.matrix[nca.matrix > 1] <- 1  #All values > 1 = 1, to get a binary matrix
rownames(nca.matrix)
nca.matrix <- nca.matrix[-39,]

#Now need to get the sum by families
nca_grp6 <- melt(nca.matrix)
nca_grp6 <- nca_grp6 %>% dplyr::rename (grp6 = Var1, fish_sp = Var2) 

nca_grp6 <- left_join(nca_grp6, data_full2[,c("fish_sp", "family")], by="fish_sp") 
nca_grp6 <- unique(nca_grp6)

nca_grp6 <- nca_grp6 %>% group_by(family, grp6) %>% #sum of value by family
  mutate(sum_fam = sum(value)) 

nca_grp6 %>% filter(family=="Acanthuridae", grp6 == "Actinopterygii")

nca_grp6 <- nca_grp6 %>% select(-fish_sp, -value)
nca_grp6 <- unique(nca_grp6)

nca.matrix.fam <- reshape2::acast(nca_grp6, grp6 ~ family , value.var = "sum_fam")

#Get the plots (then put 0 to uncommon families)
#Colors
col.fish <- data.frame(ifelse(colSums(nca.matrix.fam)!= 0,"black","grey60")) #families with values = 0, grey color
colnames(col.fish) <- "color"
col.fish$color <- as.character(col.fish$color)

col.grp6 <- unique(data.frame(data_full2$grp6)) 
col.grp6 <- data.frame(col.grp6[!is.na(col.grp6[1])])
names(col.grp6)[1] <- "taxon"

colfunc4 <- colorRampPalette(c("darkorchid4","slateblue4","turquoise1","lightseagreen","gold","coral1","lightpink2"))

col.grp6$colors <- colfunc4(38)
col.grp6$colors <- as.character(col.grp6$colors)

box.colors <- data.frame(ifelse(rowSums(nca.matrix.fam)!= 0,col.grp6$colors,"grey60")) #if sum of rows = 0 (item_class), grey color
colnames(box.colors) <- "color"
box.colors$color <- as.character(box.colors$color)

#plot
plotweb(nca.matrix.fam,method="normal", empty = FALSE, arrow="no",text.rot=90, col.interaction = "grey80",
        col.low = box.colors$color, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.10, ybig = 1.3, bor.col.low = box.colors$color, bor.col.high = col.fish$color)


#Check families in common for all five sites : 90 families in total, only 17 in common in all three sites
intersect_all <- function(a,b,...){
  Reduce(intersect, list(a,b,...))
}

fam_com <- intersect_all(nca$family, haw$family, vir$family, mad$family, mari$family) #Only 17 families in common between the 5 locations
fam_dif <- unique(data_full2$family[!data_full2$family %in% intersect_all(nca$family, haw$family, vir$family, mad$family, mari$family)])

nca.matrix.fam2 <- nca.matrix.fam
nca.matrix.fam2[,colnames(nca.matrix.fam2) %in% fam_dif] <- 0 #work

colSums(nca.matrix.fam2) 

plotweb(nca.matrix.fam2,method="normal", empty = FALSE, arrow="no",text.rot=90, col.interaction = "grey80",
        col.low = box.colors$color, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.10, ybig = 1.3, bor.col.low = box.colors$color, bor.col.high = col.fish$color)


  #Virgin Islands
x <- with(vir, table(vir$grp6,vir$fish_sp))
vir.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
vir.matrix[i1] <- x[i1]

vir.matrix[vir.matrix > 1] <- 1  #All values > 1 = 1, to get a binary matrix
rownames(vir.matrix)
vir.matrix <- vir.matrix[-39,]

#Now need to get the sum by families
vir_grp6 <- melt(vir.matrix)
vir_grp6 <- vir_grp6 %>% dplyr::rename (grp6 = Var1, fish_sp = Var2) 

vir_grp6 <- left_join(vir_grp6, data_full2[,c("fish_sp", "family")], by="fish_sp") 
vir_grp6 <- unique(vir_grp6)

vir_grp6 <- vir_grp6 %>% group_by(family, grp6) %>% #sum of value by family
  mutate(sum_fam = sum(value)) 

vir_grp6 %>% filter(family=="Acanthuridae", grp6 == "Actinopterygii")

vir_grp6 <- vir_grp6 %>% select(-fish_sp, -value)
vir_grp6 <- unique(vir_grp6)

vir.matrix.fam <- reshape2::acast(vir_grp6, grp6 ~ family , value.var = "sum_fam")


#Get the plots (then put 0 to uncommon families)
#Colors
col.fish <- data.frame(ifelse(colSums(vir.matrix.fam)!= 0,"black","grey60")) #families with values = 0, grey color
colnames(col.fish) <- "color"
col.fish$color <- as.character(col.fish$color)

col.grp6 <- unique(data.frame(data_full2$grp6)) 
col.grp6 <- data.frame(col.grp6[!is.na(col.grp6[1])])
names(col.grp6)[1] <- "taxon"

colfunc4 <- colorRampPalette(c("darkorchid4","slateblue4","turquoise1","lightseagreen","gold","coral1","lightpink2"))

col.grp6$colors <- colfunc4(38)
col.grp6$colors <- as.character(col.grp6$colors)

box.colors <- data.frame(ifelse(rowSums(vir.matrix.fam)!= 0,col.grp6$colors,"grey60")) #if sum of rows = 0 (item_class), grey color
colnames(box.colors) <- "color"
box.colors$color <- as.character(box.colors$color)

#plot
plotweb(vir.matrix.fam,method="normal", empty = FALSE, arrow="no",text.rot=90, col.interaction = "grey80",
        col.low = box.colors$color, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.10, ybig = 1.3, bor.col.low = box.colors$color, bor.col.high = col.fish$color)


#Check families in common for all five sites : 90 families in total, only 17 in common in all five sites
vir.matrix.fam2 <- vir.matrix.fam
vir.matrix.fam2[,colnames(vir.matrix.fam2) %in% fam_dif] <- 0 #work

colSums(vir.matrix.fam2) 

plotweb(vir.matrix.fam2,method="normal", empty = FALSE, arrow="no",text.rot=90, col.interaction = "grey80",
        col.low = box.colors$color, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.10, ybig = 1.3, bor.col.low = box.colors$color, bor.col.high = col.fish$color)


  #Marshall Islands
x <- with(mari, table(mari$grp6,mari$fish_sp))
mari.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
mari.matrix[i1] <- x[i1]

mari.matrix[mari.matrix > 1] <- 1  #All values > 1 = 1, to get a binary matrix
rownames(mari.matrix)
mari.matrix <- mari.matrix[-39,]

#Now need to get the sum by families
mari_grp6 <- melt(mari.matrix)
mari_grp6 <- mari_grp6 %>% dplyr::rename (grp6 = Var1, fish_sp = Var2) 

mari_grp6 <- left_join(mari_grp6, data_full2[,c("fish_sp", "family")], by="fish_sp") 
mari_grp6 <- unique(mari_grp6)

mari_grp6 <- mari_grp6 %>% group_by(family, grp6) %>% #sum of value by family
  mutate(sum_fam = sum(value)) 

mari_grp6 %>% filter(family=="Acanthuridae", grp6 == "Actinopterygii")

mari_grp6 <- mari_grp6 %>% select(-fish_sp, -value)
mari_grp6 <- unique(mari_grp6)

mari.matrix.fam <- reshape2::acast(mari_grp6, grp6 ~ family , value.var = "sum_fam")


#Get the plots (then put 0 to uncommon families)
#Colors
col.fish <- data.frame(ifelse(colSums(mari.matrix.fam)!= 0,"black","grey60")) #families with values = 0, grey color
colnames(col.fish) <- "color"
col.fish$color <- as.character(col.fish$color)

col.grp6 <- unique(data.frame(data_full2$grp6)) 
col.grp6 <- data.frame(col.grp6[!is.na(col.grp6[1])])
names(col.grp6)[1] <- "taxon"

colfunc4 <- colorRampPalette(c("darkorchid4","slateblue4","turquoise1","lightseagreen","gold","coral1","lightpink2"))

col.grp6$colors <- colfunc4(38)
col.grp6$colors <- as.character(col.grp6$colors)

box.colors <- data.frame(ifelse(rowSums(mari.matrix.fam)!= 0,col.grp6$colors,"grey60")) #if sum of rows = 0 (item_class), grey color
colnames(box.colors) <- "color"
box.colors$color <- as.character(box.colors$color)

#plot
plotweb(mari.matrix.fam,method="normal", empty = FALSE, arrow="no",text.rot=90, col.interaction = "grey80",
        col.low = box.colors$color, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.10, ybig = 1.3, bor.col.low = box.colors$color, bor.col.high = col.fish$color)


#Check families in common for all five sites : 90 families in total, only 17 in common in all five sites
mari.matrix.fam2 <- mari.matrix.fam
mari.matrix.fam2[,colnames(mari.matrix.fam2) %in% fam_dif] <- 0 #work

colSums(mari.matrix.fam2) 

plotweb(mari.matrix.fam2,method="normal", empty = FALSE, arrow="no",text.rot=90, col.interaction = "grey80",
        col.low = box.colors$color, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.10, ybig = 1.3, bor.col.low = box.colors$color, bor.col.high = col.fish$color)


  #Hawai
x <- with(haw, table(haw$grp6,haw$fish_sp))
haw.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
haw.matrix[i1] <- x[i1]

haw.matrix[haw.matrix > 1] <- 1  #All values > 1 = 1, to get a binary matrix
rownames(haw.matrix)
haw.matrix <- haw.matrix[-39,]

#Now need to get the sum by families
haw_grp6 <- melt(haw.matrix)
haw_grp6 <- haw_grp6 %>% dplyr::rename (grp6 = Var1, fish_sp = Var2) 

haw_grp6 <- left_join(haw_grp6, data_full2[,c("fish_sp", "family")], by="fish_sp") 
haw_grp6 <- unique(haw_grp6)

haw_grp6 <- haw_grp6 %>% group_by(family, grp6) %>% #sum of value by family
  mutate(sum_fam = sum(value)) 

haw_grp6 %>% filter(family=="Acanthuridae", grp6 == "Actinopterygii")

haw_grp6 <- haw_grp6 %>% select(-fish_sp, -value)
haw_grp6 <- unique(haw_grp6)

haw.matrix.fam <- reshape2::acast(haw_grp6, grp6 ~ family , value.var = "sum_fam")


#Get the plots (then put 0 to uncommon families)
#Colors
col.fish <- data.frame(ifelse(colSums(haw.matrix.fam)!= 0,"black","grey60")) #families with values = 0, grey color
colnames(col.fish) <- "color"
col.fish$color <- as.character(col.fish$color)

col.grp6 <- unique(data.frame(data_full2$grp6)) 
col.grp6 <- data.frame(col.grp6[!is.na(col.grp6[1])])
names(col.grp6)[1] <- "taxon"

colfunc4 <- colorRampPalette(c("darkorchid4","slateblue4","turquoise1","lightseagreen","gold","coral1","lightpink2"))

col.grp6$colors <- colfunc4(38)
col.grp6$colors <- as.character(col.grp6$colors)

box.colors <- data.frame(ifelse(rowSums(haw.matrix.fam)!= 0,col.grp6$colors,"grey60")) #if sum of rows = 0 (item_class), grey color
colnames(box.colors) <- "color"
box.colors$color <- as.character(box.colors$color)

#plot
plotweb(haw.matrix.fam,method="normal", empty = FALSE, arrow="no",text.rot=90, col.interaction = "grey80",
        col.low = box.colors$color, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.10, ybig = 1.3, bor.col.low = box.colors$color, bor.col.high = col.fish$color)


#Check families in common for all five sites : 90 families in total, only 17 in common in all five sites
haw.matrix.fam2 <- haw.matrix.fam
haw.matrix.fam2[,colnames(haw.matrix.fam2) %in% fam_dif] <- 0 #work

write.csv(haw.matrix.fam2, "haw.matrix.fam2.csv") #just for example to send to Giovanni

colSums(haw.matrix.fam2) 

plotweb(haw.matrix.fam2,method="normal", empty = FALSE, arrow="no",text.rot=90, col.interaction = "grey80",
        col.low = box.colors$color, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.10, ybig = 1.3, bor.col.low = box.colors$color, bor.col.high = col.fish$color)


  #Madagascar
x <- with(mad, table(mad$grp6,mad$fish_sp))
mad.matrix <- matrix(0,ncol=length(m),nrow=length(n),dimnames=list(n,m))
i1 <- as.matrix(expand.grid(rownames(x),colnames(x)))
mad.matrix[i1] <- x[i1]

mad.matrix[mad.matrix > 1] <- 1  #All values > 1 = 1, to get a binary matrix
rownames(mad.matrix)
mad.matrix <- mad.matrix[-39,]

#Now need to get the sum by families
mad_grp6 <- melt(mad.matrix)
mad_grp6 <- mad_grp6 %>% dplyr::rename (grp6 = Var1, fish_sp = Var2) 

mad_grp6 <- left_join(mad_grp6, data_full2[,c("fish_sp", "family")], by="fish_sp") 
mad_grp6 <- unique(mad_grp6)

mad_grp6 <- mad_grp6 %>% group_by(family, grp6) %>% #sum of value by family
  mutate(sum_fam = sum(value)) 

mad_grp6 %>% filter(family=="Acanthuridae", grp6 == "Actinopterygii")

mad_grp6 <- mad_grp6 %>% select(-fish_sp, -value)
mad_grp6 <- unique(mad_grp6)

mad.matrix.fam <- reshape2::acast(mad_grp6, grp6 ~ family , value.var = "sum_fam")


#Get the plots (then put 0 to uncommon families)
#Colors
col.fish <- data.frame(ifelse(colSums(mad.matrix.fam)!= 0,"black","grey60")) #families with values = 0, grey color
colnames(col.fish) <- "color"
col.fish$color <- as.character(col.fish$color)

col.grp6 <- unique(data.frame(data_full2$grp6)) 
col.grp6 <- data.frame(col.grp6[!is.na(col.grp6[1])])
names(col.grp6)[1] <- "taxon"

colfunc4 <- colorRampPalette(c("darkorchid4","slateblue4","turquoise1","lightseagreen","gold","coral1","lightpink2"))

col.grp6$colors <- colfunc4(38)
col.grp6$colors <- as.character(col.grp6$colors)

box.colors <- data.frame(ifelse(rowSums(mad.matrix.fam)!= 0,col.grp6$colors,"grey60")) #if sum of rows = 0 (item_class), grey color
colnames(box.colors) <- "color"
box.colors$color <- as.character(box.colors$color)

#plot
plotweb(mad.matrix.fam,method="normal", empty = FALSE, arrow="no",text.rot=90, col.interaction = "grey80",
        col.low = box.colors$color, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.10, ybig = 1.3, bor.col.low = box.colors$color, bor.col.high = col.fish$color)


#Check families in common for all five sites : 90 families in total, only 17 in common in all five sites
mad.matrix.fam2 <- mad.matrix.fam
mad.matrix.fam2[,colnames(mad.matrix.fam2) %in% fam_dif] <- 0 #work

colSums(mad.matrix.fam2) 

plotweb(mad.matrix.fam2,method="normal", empty = FALSE, arrow="no",text.rot=90, col.interaction = "grey80",
        col.low = box.colors$color, bor.col.interaction = FALSE,y.width.low=0.05, y.width.high=0.03,
        labsize = 1.10, ybig = 1.3, bor.col.low = box.colors$color, bor.col.high = col.fish$color)

data_full2_grp6 <- data_full2 %>% select(-grp1, -grp3, -grp4, -grp5, -grp7)
write.csv(data_full2_grp6, "data/data_full_grp6.csv")

   