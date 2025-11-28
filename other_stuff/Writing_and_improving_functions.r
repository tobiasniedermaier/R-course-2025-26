
#Writing and improving functions

dat <- mtcars #small data set included with every R installation

kmpg <- dat$mpg * 1.6 #km per gallon
tmp1 <- kmpg / 3.7 # km per liter
tmp2 <- 1/tmp1 #liter per km
res <- tmp2*100 #liter per 100 km

test1 <- function(){
 kmpg <- dat$mpg * 1.6 #km per gallon
 tmp1 <- kmpg / 3.7 # km per liter
 tmp2 <- 1/tmp1 #liter per km
 res <- tmp2*100 #liter per 100 km
 return(res)
}
test1() #Put all the calculations in a function without variables

test2 <- function(data=NULL){
 kmpg <- data$mpg * 1.6 #km per gallon
 tmp1 <- kmpg / 3.7 # km per liter
 tmp2 <- 1/tmp1 #liter per km
 res <- tmp2*100 #liter per 100 km
 return(res)
}
test2(data=dat) #Make the function independent of the data set used

test3 <- function(data=NULL, mpgvar=NULL){
 kmpg <- data[,mpgvar] * 1.6 #km per gallon
 tmp1 <- kmpg / 3.7 # km per liter
 tmp2 <- 1/tmp1 #liter per km
 res <- tmp2*100 #liter per 100 km
 return(res)
}
test3(data=dat, mpgvar="mpg") #Make the function independent of the variable used
test3(data=iris, mpgvar="Species") #Uninformative error

mpg_to_lp100km <- function(data=NULL, mpgvar=NULL, plausib_bound=10){
 if(is.null(data)) stop("No data set provided!")
 if(!is.numeric(data[,mpgvar])) stop("Variable needs to be numeric!")
 if(min(data[,mpgvar]) < plausib_bound){
  print(data[data[,mpgvar] < plausib_bound,])
  warning("At least one observation has an unrealistically high fuel consumption.\nInstead of liters per 100km, the implausible obervations are returned.\nPlease check your input data and/or exclude those observations..")
  } 
 kmpg <- data[,mpgvar] * 1.6 #km per gallon
 tmp1 <- kmpg / 3.7 # km per liter
 tmp2 <- 1/tmp1 #liter per km
 res <- tmp2*100 #liter per 100 km
 return(res)
}
mpg_to_lp100km(data=dat, mpgvar="mpg") #ok
mpg_to_lp100km(data=dat, mpgvar="mpg", plausib_bound=15) #Warning
mpg_to_lp100km(data=dat, mpgvar="mpg", plausib_bound=10) #No warning
x <- mpg_to_lp100km(data=dat, mpgvar="mpg", plausib_bound=15) #Warning, but it still performs the calculation and one can work with it like normal.
mpg_to_lp100km(data=iris, mpgvar="Species") #Informative error

