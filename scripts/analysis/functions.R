###########
#Functions#
###########


mean_IC <- function(x){
  r <- c(mean(x), mean(x)-1.96*sd(x)/sqrt(length(x)), mean(x) +1.96*sd(x)/sqrt(length(x)))
  names(r) <- c("y","ymin","ymax")
  r
}





chloe_null.model <- function(x, perm) {
  
  column.index <- 1:dim(x)[2]
  
  lapply(1:perm, function (y) {
    
    a <- x[,sample(column.index, size= dim(x)[2], replace = T)]
    colnames (a) <- colnames(x)
    
    a
    
  }) 
  
}