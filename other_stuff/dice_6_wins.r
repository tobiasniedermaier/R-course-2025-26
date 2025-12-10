dice_6_wins <- function(size=100){
 res <- as.character(sample(1:6, size=size, replace=TRUE))
 
 for(i in seq_len(size)){
  if (res[i]=="6"){res[i] <- "win"} else {
  res[i] <- "lose"}
 }
 return(res)
}
dice_6_wins()

#Or without for-loop:
dice_6_wins <- function(size = 100) {
  dice <- sample(1:6, size = size, replace = TRUE)
  res  <- ifelse(dice == 6, "win", "lose")
  return(res)
}
dice_6_wins()
