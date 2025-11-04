
n_dice <- function(n=2, size=100){
 res <- list()
 for(i in 1:n){
  res[[i]] <- sample(1:6, size=size, replace=T)
 }
 res <- Reduce(f="+", x=res)
 return(res)
}
