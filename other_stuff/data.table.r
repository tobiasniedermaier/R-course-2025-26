
##########################
# The data.table package #
##########################

#https://www.r-bloggers.com/intro-to-the-data-table-package/

DF <- mtcars

library(data.table)
 
DT <- data.table(DF)
class(DT)

#Means of mpg:
#base R:
mean(DT$mpg)
#data.table:
DT[,mean(mpg)]

#Subsetting:
#base R:
DF[DF$mpg>20 & DF$hp>100,] #(or one of the many other ways)
#data.table:
DT[DT$mpg>20 & DT$hp>100,] #You can work with data tables just like you would with data frames.
DT[mpg>20 & hp>100,] #It is not necessary to repeat the data set name in data.table, unlike base R. Practically like a "built-in" attach/detach.

#Means of mpg in am groups:
#base R:
tapply(DF$mpg, DF$am, mean)
aggregate(mpg ~ am, data=DF, mean)
#data.table:
DT[,mean(mpg), by=am]

#Stratified by am+cyl:
#base R:
aggregate(mpg ~ am + cyl, data=DF, mean)
#data.table:
DT[,mean(mpg), by=.(am,cyl)]
#Identical, but "V1" labeled as"avg":
DT[,.(avg=mean(mpg)), by=.(am,cyl)]

#Number of rows in the data set:
#base R:
nrow(DF)
#data.table:
DT[, .N]

#How many cars in each cylinder group:
#base R:
table(DF$cyl)
#data.table
DT[, .N, by=cyl]

#Present the 5 cars with the best MPG
#base R:
head(DF[order(DF$mpg, decreasing=T),], 5)
#data.table:
head(DT[order(-mpg)],5)

#Create a new variable (kw computed from horse powers):
DT[,kw := hp * 0.735499]

# For rows where the wt is &gt; 1.5 tons count the number of cars by
# transmission type.
dt[wt > 1.5, .(count=.N), by=am]
