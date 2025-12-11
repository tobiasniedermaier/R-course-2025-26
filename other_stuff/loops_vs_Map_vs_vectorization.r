
#Loops vs. higher-order functions vs. vectorization:

#Let's say we want to convert different columns from miles per gallon to liters per 100 km:

#We can use the function mpg_to_lp100km for that, but I also want to show how to use a for()- and a while()- loop.

dat <- mtcars[,"mpg", drop=FALSE]

#Let's create a second and third mpg variable to which some random noise was added. Think of it as a series of measurements
set.seed(1)
dat$mpg2 <- mtcars$mpg + rnorm(mtcars$mpg, mean=0, sd=2)
set.seed(2)
dat$mpg3 <- mtcars$mpg + rnorm(mtcars$mpg, mean=0, sd=2)
dat_backup <- dat

#Now we have three columns with measurements in mpg that we want to convert to l/100 km. But we don't want to do that one by one (because, what if we had 100 variables to convert?)

#1st way: using a for-loop:
for (i in seq_len(ncol(dat))){
 dat[,i] <- dat[,i] * 1.6
 dat[,i] <- dat[,i] / 3.7
 dat[,i] <- 1/dat[,i]
 dat[,i] <- dat[,i] * 100
}

#2nd way: using a while-loop:
dat <- dat_backup
i <- 1
while(i <= ncol(dat)){
 dat[,i] <- dat[,i] * 1.6
 dat[,i] <- dat[,i] / 3.7
 dat[,i] <- 1/dat[,i]
 dat[,i] <- dat[,i] * 100
 i <- i+1
}

#3rd way: using a repeat-loop:
dat <- dat_backup
i <- 1
repeat {
  dat[, i] <- dat[, i] * 1.6
  dat[, i] <- dat[, i] / 3.7
  dat[, i] <- 1 / dat[, i]
  dat[, i] <- dat[, i] * 100
  i <- i + 1
  if (i > ncol(dat)) break
}

#Intermediate step for 4th way:

## From other_stuff/mpg_to_lp100km.r:
mpg_to_lp100km <- function(data=NULL, mpgvar=NULL, plausib_bound=10){
 if(is.null(data)) stop("No data set provided!")
 if(!is.numeric(data[,mpgvar])) stop("Variable needs to be numeric!")
 if(min(data[,mpgvar]) <= plausib_bound){
 print(data[data[,mpgvar] <= plausib_bound,])
 warning("At least one observation has an unrealistically high fuel consumption.\nIn addition to liters per 100km, the implausible obervations are returned.\nPlease check your input data and/or exclude those observations..")
 }
 kmpg <- data[,mpgvar] * 1.6 #km per gallon
 tmp1 <- kmpg / 3.7 # km per liter
 tmp2 <- 1/tmp1 #liter per km
 res <- tmp2*100 #liter per 100 km
 return(res)
}

#4th way: using the higher-order function Map()
dat <- dat_backup
do.call("cbind", Map(mpg_to_lp100km, c("mpg","mpg2","mpg3"), MoreArgs=list(data=dat)))

#5th way: using vectorization
dat <- dat_backup
FUN <- function(x){
 x <- x*1.6
 x <- x/3.7
 x <- 1/x
 x <- x*100
 return(x)
}
vFUN <- Vectorize(FUN)
vFUN(dat)

#Although here, it would also work without explicit Vectorize():
mpg_to_lp100km_vec <- function(x) {
  100 / (x * 1.6 / 3.7)
}

dat <- dat_backup
dat <- mpg_to_lp100km_vec(dat)

