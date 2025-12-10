
two_dice <- function(size=100){
 res1 <- sample(1:6, size=size, replace=TRUE)
 res2 <- sample(1:6, size=size, replace=TRUE)
 res <- res1+res2
 return(res)
}

