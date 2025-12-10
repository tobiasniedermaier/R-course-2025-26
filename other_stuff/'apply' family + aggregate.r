
#https://www.r-bloggers.com/using-apply-sapply-lapply-in-r/
#- apply:

# - Example: 
	m <- matrix(data=cbind(rnorm(30, 0), rnorm(30, 2), rnorm(30, 5)), nrow=30, ncol=3)
#	 "how many negative numbers in 1st column?" 
#	 Alternative, complicated way: length(m[which(m[,1]<0)])
#	 Better way with apply: apply(m, 2, function(x) length(x[x<0]))

# - Example: 
	m <- matrix(data=1:100, nrow=10)
#	 Mean of each column?
	 
#	Complicated way: 
	for (i in 1: ncol(m)){print(mean(m[,i]))}
	result <- vector("numeric", 10)
	for (i in 1: ncol(m)){result[[i]] <- mean(m[,i])}
	 
	#Easier with apply: 
	apply(m, 2, function(x) mean(x))
	#Even easier: 
	apply(m, 2, mean)

set.seed(1)
x <- cbind(x1 = round(runif(8,1,8)), x2 = c(4:1, 2:5))
dimnames(x)[[1]] <- letters[1:8]


#https://nsaunders.wordpress.com/2010/08/20/a-brief-introduction-to-apply-in-r/
#- lapply:

#Example: 
l <- list(a = 1:10, b = 11:20) #"the mean of the values in each element"
#Complicated way: 
mean(as.numeric(l[[1]])) #and
mean(as.numeric(l[[2]]))
#Better way with lapply: 
lapply(l, mean)
#Even nicer: 
sapply(l, mean)

#mapply:
(x <- mapply(rep, 1:4, 4:1))
gdata::is.what(x) #is.list, is.recursive u.a.
#2*x #funktioniert nicht, "Error in x * 2 : non-numeric argument to binary operator"
lapply(x, function(x){2*x}) #Identical: 
mapply(function(x){2*x}, x)

#rapply:
rapply(x, print)
rapply(x, sum) #Identical: mapply(sum, x)
rapply(x, length) #identical: mapply(length, x) #Or: unlist(lapply(x, length)) #or: sapply(x, length)
rapply(x, function(x){2*x})

#More useful examples (s. http://theautomatic.net/2018/11/13/those-other-apply-functions/):
lapply(iris, mean) #NA bei nicht-numerischer Variable + warning (works nonetheless)
sapply(iris[sapply(iris, is.numeric)], mean) #how to solve it without rapply
rapply(iris, mean, class = "numeric") #elegant rapply solution


#- by() is a wrapper for tapply():
#http://rfunction.com/archives/2330

library(openintro)
#data() lists all available example datasets contained in currently loaded packages
data(textbooks)
head(textbooks, 2)
by(textbooks$diff, textbooks$more, summary)
#entspricht:
tapply(textbooks$diff, textbooks$more, summary)

#- aggregate function:
#aggregate(x, by, fun), input DF, output DF. Bsp.:
aggregate(mpg ~ cyl, data=mtcars, FUN=summary)
#Same, but not as nice:
aggregate(mtcars$mpg ~ mtcars$cyl, FUN=summary)
#And a third way of achieving the same:
aggregate(mtcars$mpg, by=list(mtcars$cyl), FUN=summary)
#Alternatively a variant with tapply:
tapply(mtcars$mpg, mtcars$cyl, summary)
#Another one with split and lapply:
(mysplit <- split(mtcars$mpg, mtcars$cyl))
lapply(mysplit, summary)
#aggregate has the advantage of returning a data.frame and not a list like tapply. And one can do more complex things (stratified analyses) slightly easier than with tapply. Example: Get the mean MPG for Transmission grouped by Cylinder:
aggregate(mpg ~ am + cyl, data=mtcars, mean)

#Summarize and stratify by two variables:
tapply(mtcars$mpg, list(mtcars$am, mtcars$cyl), FUN=mean)
aggregate(mpg ~ am + cyl, data=mtcars, mean)

#How can I loop through a list of strings as variables in a model?
library(mada)
data(AuditC)
(dat1 <- AuditC[1:7,])
(dat2 <- AuditC[8:14,])
(data <- list(dat1, dat2))
lapply(data, reitsma)
summary_reitsma <- function(x, ...){summary(reitsma(x, ...))}
(x <- lapply(data, summary_reitsma)) #or: for (i in 1:2) {x[[i]] <- summary_reitsma(data[i])} #or: for (i in 1:2) {x[[i]] <- summary(reitsma(data[i]))}

#Filter the numeric variables:
dat <- read.csv2("C:\\path\\to\\your\\file\\TeachingRatings.csv", header=T, sep=",")
#1.
nums <- sapply(dat, is.numeric)
dat[,nums]
#2.
dat[,sapply(dat, is.numeric)]
#3.
numerics <- Filter(is.numeric, dat)

#Apply a function to them:
sapply(numerics, mean)
as.data.frame(sapply(numerics, mean))
