dice_6_wins <- function(size=100){
 res <- as.character(sample(1:6, size=size, replace=T))
 
 for(i in 1:size){
 if (res[i]=="6"){res[i] <- "win"} else {
 res[i] <- "lose"}
  }
 return(res)
}
dice_6_wins()
