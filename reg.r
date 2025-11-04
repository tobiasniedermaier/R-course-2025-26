
#A function to perform a (logistic or other) regression without using the equation syntax. Advantage: can be used to perform a number of different regressions, with different data sets and/or covariates adjusted for, using the Map() function.

reg <- function(data, outcome, covariates, family="binomial"){
 mylist <- list() #create an empty list to store regression equation
 for(i in c(covariates)){mylist[[i]] <- paste("+", data, "$", i, sep="")} #paste together regression equation, step 1 --> "+ data$var1 + data$var2 + ..."
 res1 <- paste(noquote(unlist(mylist))) #remove list and quotes
 res2 <- paste(res1, collapse=" ")
 rhs <- substr(res2, 2, nchar(res2)) #remove trailing "+"

 lhs <- paste(data, "$", outcome, sep="") #left-hand side of equation
 equation <- paste(lhs, "~", rhs) #put together regression equation
 res <- glm(eval(parse(text=paste(equation))), family=family) #conduct any regression from type "generalized linear model"
 return(res)
}
(a <- reg(data="mtcars", outcome="am", covariates=c("cyl","mpg")))
(b <- glm(am ~ cyl + mpg, data=mtcars, family="binomial")) #same result
