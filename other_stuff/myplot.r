
myplot <- function(x=NULL, y=NULL, ...){
if(any(is.na(x))){
 warning("NA or NaN values detected in x variable.")
}

if(any(is.na(y))){
 warning("NA or NaN values detected in y variable.")
}
 plot(x, y, ...)
}
