
#die verschiedenen Möglichkeiten, ein Subset zu bilden:
#eingebauter Datensatz mtcars; Autos mit mind. 6 Zylindern, mehr als 150 PS und weniger als 3.5 Tonnen:
#1.) extrem umständlich, ehemals die einzige Variante, die ich kannte
mtcars[which(mtcars$cyl>=6 & mtcars$hp>150 & mtcars$wt<3.5),]
#2.) weniger umständliches Subsetten an sich, dafür Schritt vor- und nachher nötig
attach(mtcars)
mtcars[which(cyl>=6 & hp>150 & wt<3.5),]
detach(mtcars)
#3.) beste Variante, die noch mit base-R machbar ist:
subset(mtcars, cyl>=6 & hp>150 & wt<3.5)
#4.) noch eine Variante mit base-R, die ich aber nicht schön finde:
with(mtcars, mtcars[cyl>=6 & hp>150 & wt<3.5,]) #Selten verwendet.
#5.) mit dem data.table-Paket:
library(data.table)
dat <- as.data.table(mtcars)
dat[cyl>=6 & hp>150 & wt<3.5] #data.table ist extrem effizient, erfordert aber eine gewisse Umgewöhnung vgl. mit Base R.
#6.) mit dem dplyr-Paket:
library(dplyr)
filter(mtcars, cyl>=6, hp>150, wt<3.5)
#7.) mit dem sqldf-Paket:
library(sqldf)
sqldf("select * from mtcars where cyl>=6 and hp>150 and wt<3.5") #Nützlich für SQL-Umsteiger.
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
