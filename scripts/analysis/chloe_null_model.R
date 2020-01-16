
chloe_null.model <- function(x, perm) {
  
  column.index <- 1:dim(x)[2]
  
  lapply(1:perm, function (y) {
    
    a <- x[,sample(column.index, size= dim(x)[2])]
    colnames (a) <- colnames(x)
    
    a
    
  }) 
  
}

chloe_null.model <- function(x, perm) {
  
  column.index <- 1:dim(x)[2]
  
  lapply(1:perm, function (y) {
    
    a <- x[,sample(column.index, size= dim(x)[2], replace=T)]
    colnames (a) <- colnames(x)
    
    a
    
  })
}


#Nina's function to suffle columns of each matrice in the list
shuffle_colums <- function(list_of_matrices){
  new_list <- lapply(1:length(list_of_matrices), function (z) {
    x <- list_of_matrices[[z]]
    a <- x[,sample(column.index, size = dim(x)[2],replace = F)]
    colnames (a) <- 1:ncol(a)
    return(a)
  })
  return(new_list)
}  

test <- shuffle_colums(rand_vir_sp)  

colSums(rand_vir_sp[[2]])

colSums(test[[2]])
rowSums(test[[2]])


length(which(vir_ISmatrix_sp2[,7] != 0))
length(which(rand_vir_sp[[4]][,7] != 0))
length(which(test[[7]][,7] != 0))

