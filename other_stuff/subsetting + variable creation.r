
#The various ways of subsetting a data frame:
#Built-in data set mtcars; Cars with >= 6 cylinders, more than 150 PS and less than 3.5 tons:
#1.) extremely complicated:
mtcars[which(mtcars$cyl>=6 & mtcars$hp>150 & mtcars$wt<3.5),]
#2.) Less complicated:
mtcars[mtcars$cyl >= 6 & mtcars$hp > 150 & mtcars$wt < 3.5, ]
#2.) Less complicated, but additional steps before and after:
attach(mtcars)
mtcars[cyl >=6 & hp >150 & wt <3.5,]
detach(mtcars)
#3.) Best solution using Base R:
subset(mtcars, cyl >=6 & hp >150 & wt <3.5)
#4.) Another solution using only Base R:
with(mtcars, mtcars[cyl >=6 & hp >150 & wt <3.5,]) #Rarely used.
#5.) Using the data.table package:
library(data.table)
dat <- as.data.table(mtcars)
dat[cyl >=6 & hp >150 & wt <3.5] #data.table is extremely efficient/fast, especially when handling very large data sets.
#6.) Using the dplyr-Paket:
dplyr::filter(mtcars, cyl >=6, hp >150, wt <3.5)
#7.) Using the sqldf-Paket:
library(sqldf)
sqldf("select * from mtcars where cyl>=6 and hp >150 and w t<3.5") #Useful for people who worked with SQL previously.
#For reference of sqldf, see.  https://jasdumas.github.io/tech-short-papers/sqldf_tutorial.html#wild_card_match_queries

#within is practically unnecessary because the same can be done using with() or transform(). Only the syntax is different and slightly shorter on case of several transformation steps. See https://www.r-bloggers.com/friday-function-triple-bill-with-vs-within-vs-transform/

#The various ways of creating a new variable based on two existing variables:
library(MASS)
head(anorexia); ls.str(anorexia)
anorexia$wtDiff1 <- anorexia$Postwt - anorexia$Prewt
#base-R and attach and detach:
attach(anorexia)
anorexia$wtDiff2 <- Postwt - Prewt
detach(anorexia) #It is discouraged to work with attach and detach because they may lead to errors which are hard to analyze and debug.
#base-R and within:
anorexia <- within(anorexia, wtDiff3 <- Postwt - Prewt)
#base-R and transform:
anorexia <- transform(anorexia, wtDiff4 = Postwt - Prewt)
#dplyr and mutate:
anorexia <- dplyr::mutate(anorexia, wtDiff5 = Postwt - Prewt)
#data.table:
anorexia <- as.data.table(anorexia)
anorexia[,wtDiff6 := Postwt - Prewt]

#- One can also assign the same value to several variables at a time:
(a <- b <- 5)



