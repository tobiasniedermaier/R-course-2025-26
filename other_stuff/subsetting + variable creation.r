
#The various ways of subsetting a data frame:
#Built-in data set mtcars; Cars with >= 6 cylinders, more than 150 PS and less than 3.5 tons:
#1.) extremely complicated:
mtcars[which(mtcars$cyl>=6 & mtcars$hp>150 & mtcars$wt<3.5),]
#2.) Less complicated, but additional steps before and after:
attach(mtcars)
mtcars[which(cyl>=6 & hp>150 & wt<3.5),]
detach(mtcars)
#3.) Best soltion using Base R:
subset(mtcars, cyl>=6 & hp>150 & wt<3.5)
#4.) Another solution using only Base R:
with(mtcars, mtcars[cyl>=6 & hp>150 & wt<3.5,]) #Rarely used.
#5.) Using the data.table package:
library(data.table)
dat <- as.data.table(mtcars)
dat[cyl>=6 & hp>150 & wt<3.5] #data.table is extremely efficient/fast, especially when handling very large data sets.
#6.) Using the dplyr-Paket:
library(dplyr)
filter(mtcars, cyl>=6, hp>150, wt<3.5)
#7.) Using the sqldf-Paket:
library(sqldf)
sqldf("select * from mtcars where cyl>=6 and hp>150 and wt<3.5") #Useful for people who worked with SQL previously.
#zu den weiteren Funktionen von sqldf s.  https://jasdumas.github.io/tech-short-papers/sqldf_tutorial.html#wild_card_match_queries

#within ist im Prinzip überflüssig, da genauso gut alles mit with oder transform machbar. Nur die Syntax ist anders und kürzer, falls mehrere Transformationsschritte. S. https://www.r-bloggers.com/friday-function-triple-bill-with-vs-within-vs-transform/

#die verschiedenen Möglichkeiten, eine neue Variable aus zwei existierenden Variablen zu bilden:
library(MASS)
head(anorexia); ls.str(anorexia)
anorexia$wtDiff1 <- anorexia$Postwt - anorexia$Prewt
#oder mit base-R und attach und detach:
attach(anorexia)
anorexia$wtDiff2 <- Postwt - Prewt
detach(anorexia) #Es wird aber allgemein nicht empfohlen, mit attach und detach zu arbeiten, weil sie zu schwer nachvollziehbaren Fehlern führen können.
#oder mit base-R und within:
anorexia <- within(anorexia, wtDiff3 <- Postwt - Prewt)
#oder mit base-R und transform:
anorexia <- transform(anorexia, wtDiff4 = Postwt - Prewt)
#oder mit dplyr und mutate:
anorexia <- mutate(anorexia, wtDiff5 = Postwt - Prewt)
#oder mit data.table:
anorexia <- as.data.table(anorexia)
anorexia[,wtDiff6 := Postwt - Prewt]

#- man kann mehreren Variablen "in einem Rutsch" den gleichen Wert zuweisen:
(a <- b <- 5) # weist a den Wert von b zu und b wird 5 zugewiesen.

