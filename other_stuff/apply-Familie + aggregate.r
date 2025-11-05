
#https://www.r-bloggers.com/using-apply-sapply-lapply-in-r/
#- apply:

# - Bsp.: 
	m <- matrix(data=cbind(rnorm(30, 0), rnorm(30, 2), rnorm(30, 5)), nrow=30, ncol=3)
#	 "how many negative numbers in 1st column?" 
#	 Umständlicher Weg von mir: length(m[which(m[,1]<0)])
#	 besserer Weg mit apply: apply(m, 2, function(x) length(x[x<0]))

# - Bsp.: 
	m <- matrix(data=1:100, nrow=10)
#	 Mittelwert von jeder Spalte?
	 
#	Umständlicher Weg von mir: 
	for (i in 1: ncol(m)){print(mean(m[,i]))}
	result <- vector("numeric", 10)
	for (i in 1: ncol(m)){result[[i]] <- mean(m[,i])}
	 
	#besserer Weg mit apply: 
	apply(m, 2, function(x) mean(x))
	#noch einfacher: 
	apply(m, 2, mean)

set.seed(1)
x <- cbind(x1 = round(runif(8,1,8)), x2 = c(4:1, 2:5))
dimnames(x)[[1]] <- letters[1:8]


#https://nsaunders.wordpress.com/2010/08/20/a-brief-introduction-to-apply-in-r/
#- lapply:

#Bsp: 
l <- list(a = 1:10, b = 11:20) #"the mean of the values in each element"
#Umständlicher Weg von mir: 
mean(as.numeric(l[[1]])) #und
mean(as.numeric(l[[2]]))
#besserer Weg mit lapply: 
lapply(l, mean)
#noch besser: 
sapply(l, mean)

#mapply:
(x <- mapply(rep, 1:4, 4:1))
gdata::is.what(x) #is.list, is.recursive u.a.
#2*x #funktioniert nicht, "Error in x * 2 : non-numeric argument to binary operator"
lapply(x, function(x){2*x}) #funktioniert! Identisch: 
mapply(function(x){2*x}, x)

#rapply:
rapply(x, print)
rapply(x, sum) #identisch: mapply(sum, x)
rapply(x, length) #identisch: mapply(length, x) #oder auch: unlist(lapply(x, length)) #oder: sapply(x, length)
rapply(x, function(x){2*x})

#sinnvolleres Beispiel (s. http://theautomatic.net/2018/11/13/those-other-apply-functions/):
lapply(iris, mean) #NA bei nicht-numerischer Variable + Warnmeldung (funktioniert aber auch)
sapply(iris[sapply(iris, is.numeric)], mean) #wie ich es lösen würde ohne rapply
rapply(iris, mean, class = "numeric") #elegantere rapply-Lösung


#- by() ist im Prinzip ähnlich wie tapply():
#http://rfunction.com/archives/2330

library(openintro)
#data() lists all available example datasets contained in currently loaded packages
data(textbooks)
head(textbooks, 2)
by(textbooks$diff, textbooks$more, summary)
#entspricht:
tapply(textbooks$diff, textbooks$more, summary)

#- aggregate-Funktion:
#aggregate(x, by, fun), input DF, output DF. Bsp.:
aggregate(mpg ~ cyl, data=mtcars, FUN=summary)
#äquivalent, aber weniger schön:
aggregate(mtcars$mpg ~ mtcars$cyl, FUN=summary)
#und noch eine dritte mögliche Schreibweise:
aggregate(mtcars$mpg, by=list(mtcars$cyl), FUN=summary)
#und alternativ eine Variante mit tapply:
tapply(mtcars$mpg, mtcars$cyl, summary)
#und noch eine Variante mit split und lapply:
(splitted <- split(mtcars$mpg, mtcars$cyl))
lapply(splitted, summary)
#aggregate hat den Vorzug, einen data.frame auszugeben und keine Liste wie tapply. Außerdem kann man damit wohl auch komplexere Sachen machen. Bsp.: Get the mean MPG for Transmission grouped by Cylinder:
aggregate(mpg~am+cyl, data=mtcars, mean)

#Summarize and stratify by two variables:
tapply(mtcars$mpg, list(mtcars$am, mtcars$cyl), FUN=mean)
aggregate(mpg~am+cyl, data=mtcars, mean)

#How can I loop through a list of strings as variables in a model?
library(mada)
data(AuditC)
(dat1 <- AuditC[1:7,])
(dat2 <- AuditC[8:14,])
(data <- list(dat1, dat2))
lapply(data, reitsma)
summary_reitsma <- function(x, ...){summary(reitsma(x, ...))}
(x <- lapply(data, summary_reitsma)) #oder: for (i in 1:2) {x[[i]] <- summary_reitsma(data[i])} #oder: for (i in 1:2) {x[[i]] <- summary(reitsma(data[i]))}

#numerische Variablen herausfiltern:
dat <- read.csv2("D:\\Daten Samsung Notebook\\Statistik\\R üben\\R Kurs LMU\\TeachingRatings.csv", header=T, sep=",")
#1.
nums <- sapply(dat, is.numeric)
dat[,nums]
#2.
dat[,sapply(dat,is.numeric)]
#3.
numerics <- Filter(is.numeric, dat)

#auf diese eine Funktion anwenden:
sapply(numerics, mean) #oder besser, weil data.frame als Output und ansonsten identisch:
Kmisc::dapply(numerics, mean)
as.data.frame(sapply(numerics, mean))
