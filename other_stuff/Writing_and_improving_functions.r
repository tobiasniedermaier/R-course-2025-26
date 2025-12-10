
#Writing and improving functions

dat <- mtcars #small data set included with every R installation

# Manual calculation of liters per 100 km ------------------------------
kmpg <- dat$mpg * 1.6 #km per gallon
tmp1 <- kmpg / 3.7    # km per liter
tmp2 <- 1/tmp1        #liter per km
res <- tmp2*100       #liter per 100 km

# 1) Put all the calculations in a function without arguments ----------
test1 <- function(){
 kmpg <- dat$mpg * 1.6 # km per gallon
 tmp1 <- kmpg / 3.7    # km per liter
 tmp2 <- 1/tmp1        # liter per km
 res <- tmp2*100       # liter per 100 km
 return(res)
}
test1() #Put all the calculations in a function without variables

# 2) Make the function independent of the data set used ----------------
test2 <- function(data=NULL){
 kmpg <- data$mpg * 1.6 # km per gallon
 tmp1 <- kmpg / 3.7     # km per liter
 tmp2 <- 1/tmp1         # liter per km
 res <- tmp2*100        # liter per 100 km
 return(res)
}
test2(data=dat) #Make the function independent of the data set used

# 3) Make the function independent of the variable used ----------------
test3 <- function(data=NULL, mpgvar=NULL){
 kmpg <- data[,mpgvar] * 1.6 # km per gallon
 tmp1 <- kmpg / 3.7          # km per liter
 tmp2 <- 1/tmp1              # liter per km
 res <- tmp2*100             # liter per 100 km
 return(res)
}
test3(data=dat, mpgvar="mpg") #Make the function independent of the variable used
test3(data=iris, mpgvar="Species") #Uninformative error (on purpose)

# 4) Improved version with checks and informative errors ---------------
mpg_to_lp100km <- function(data=NULL, mpgvar=NULL, plausib_bound=10){
  ## Argument checks ----------------------------------------------------

  if (is.null(data)) {
    stop("No data set provided (argument 'data' is NULL).")
  }
 
  if (is.null(mpgvar)) {
    stop("No variable specified (argument 'mpgvar' is NULL).")
  }
 
  if (!mpgvar %in% names(data)) {
    stop("Variable '", mpgvar, "' not found in 'data'.")
  }

 if(!is.numeric(data[,mpgvar])) stop("Variable needs to be numeric!")

  ## Plausibility check -------------------------------------------------
  ## Very small mpg values imply very high fuel consumption.
 
 if(min(data[,mpgvar]) < plausib_bound){
  print(data[data[,mpgvar] < plausib_bound,])
  warning("At least one observation has an unrealistically high fuel consumption.\nInstead of liters per 100km, the implausible obervations are returned.\nPlease check your input data and/or exclude those observations..")
  } 

 ## Conversion: mpg -> liters per 100 km -------------------------------
 kmpg <- data[,mpgvar] * 1.6 # km per gallon
 tmp1 <- kmpg / 3.7          # km per liter
 tmp2 <- 1/tmp1              # liter per km
 res <- tmp2*100             # liter per 100 km
 return(res)
}

mpg_to_lp100km(data=dat, mpgvar="mpg")                   #ok
mpg_to_lp100km(data=dat, mpgvar="mpg", plausib_bound=15) #Warning
mpg_to_lp100km(data=dat, mpgvar="mpg", plausib_bound=10) #No warning
x <- mpg_to_lp100km(data=dat, mpgvar="mpg", plausib_bound=15) #Warning, but calculation is still performed and can be used as usual.
mpg_to_lp100km(data=iris, mpgvar="Species") #Informative error
