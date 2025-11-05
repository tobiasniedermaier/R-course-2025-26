
########################
# das data.table-Paket #
########################

#https://www.r-bloggers.com/intro-to-the-data-table-package/

DF <- mtcars

library(data.table)
 
DT <- data.table(DF)
class(DT)

#Mittelwerte von mpg:
#base R:
mean(DT$mpg)
#data.table:
DT[,mean(mpg)]

#Subsets bilden:
#base R:
DF[DF$mpg>20 & DF$hp>100,] #(oder eine der vielen anderen Möglichkeiten)
#data.table:
DT[DT$mpg>20 & DT$hp>100,] #man kann mit data tables auch wie bisher mit data frames arbeiten
DT[mpg>20 & hp>100,] #die Wiederholung des Namens des Datensatzes braucht es aber nicht. Ist im Pinzip wie "eingebautes" attach/detach.

#Mittelwerte von mpg in am-Gruppen:
#base R:
tapply(DF$mpg, DF$am, mean)
aggregate(mpg ~ am, data=DF, mean)
#data.table:
DT[,mean(mpg), by=am]

#gruppiert nach am+cyl:
#base R:
aggregate(mpg ~ am + cyl, data=DF, mean)
#data.table:
DT[,mean(mpg), by=.(am,cyl)]
#identisch, aber "V1" als "avg" gelebeled:
DT[,.(avg=mean(mpg)), by=.(am,cyl)]

#Anzahl der Zeilen im Datensatz:
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

#neue Variable erstellen (Kilowatt aus PS errechnet):
DT[,kw := hp * 0.735499]

# For rows where the wt is &gt; 1.5 tons count the number of cars by
# transmission type.
dt[wt > 1.5, .(count=.N), by=am]
