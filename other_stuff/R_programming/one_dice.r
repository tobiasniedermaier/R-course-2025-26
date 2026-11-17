
# Simulating a dice throw in R

# Author: Tobias Niedermaier

one_dice <- function(size=100){
 sample(1:6, size=size, replace=TRUE)
}
