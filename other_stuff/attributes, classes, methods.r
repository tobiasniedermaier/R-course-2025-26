
(x <- x_backup <- 1:3 %o% 3:1) #"outer product"

#und wenn man wirklich nur die Zahlenfolge ohne Matrix-Anordnung haben will:
#sichere erst die Attribute der Matrix:
(attrib_x <- attributes(x))
attributes(x) <- NULL
x
#Wiederherstellen der Matrix-Anordnung:
attributes(x) <- attrib_x
x #!

#Matrix-Anordnung abspeichern, etwas Struktuzerstörendes mit der Matrix machen und dann die abgespeicherte Struktur wiederherstellen:
x+1 #naheliegender, einfacher und vektorierter, daher schneller Weg, zu jedem Matrixelement 1 dazuzuzählen.
#dumme Alternative mit for-Schleife, die aber die Matrix-Anordnung bewahrt:
for(i in 1:length(x)){x[[i]] <- (x[i] + 1)}
x <- x_backup

#jetzt eine dumme Alternative mit for-Schleife:
for(i in 1:length(x)){x[i] <- print(x[i] + 1)}
x #Matrix-Anordnung bleibt erhalten.
(y <- lapply(x+1, print(sum))) #y=x+1, but as list
(z <- unlist(y)) #Vector
attributes(z) <- attrib_x
z #3x3-Matrix!

#Classes:
x <- 10
class(x) #"numeric"
class(x) <- "character"
x # "10"
gdata::is.what(x) #is.character

#Bsp.:
library(mada) #Paket mit Bsp.-Daten laden
data(AuditC) #Bsp.-Daten laden
dat <- AuditC #Datensatz erstellen
class(madad) #"function"
result <- madad(dat)
class(result) #"madad"
class("madad") #character
print(result) #mit der print-Methode, die in der Klasse "madad" definiert ist
(result)[] #zerlegt in die einzelnen Listenbestandteile
gdata::is.what(result) #Liste, Objekt, recursive
rapply(result, cbind)
unlist(result) #identisch mit obigem rapply-Befehl
print.default(result) #wendet nicht mehr die Methoden der Klasse madad an, sondern die default-Methode. Mgl. Vorteil: Leichter ersichtlich, was eine Sub-Liste wovon ist, daher leichter zerleg- und wieder zusammensetzbar.
unclass(result) #faktisch identisch zur Zeile drüber

#A gentle introduction to "methods":
#On the one hand, R is a functional programming language. Everything that happens is a function call (even assignments or simple calculations like 1+1), functions are "first-class citizens" that can be stored in objects, there are higher-order functions, function-generating functions ("generators") etc.)
#On the other hand, R is also an object-oriented programming language. Everything that exists is an object (even functions are objects). Objects have "classes". Think of them as a tag that tells a function how to treat this object.
#Background: There are "generic" functions like plot(), summary(), print(). They look like one function each. But in fact, there are various summary functions and the object class determines which summary function should be applied.
#Example:
summary(iris) #iris is a data frame with four numeric variables (Sepal.Length, Sepal.Width, Petal.Length. Petal.Width), so summary(iris) will return Min, Mean, Median, Max, 25th and 75th quintile for those variables. 
But iris also contains one factor variable (Species), so summary() will return the number of observations in the variable for each factor level.
summary.data.frame(iris) #You can also select the summary function directly so that R doesn't have to "guess which summary function to select".
summary(lm(mpg ~ hp, data=mtcars) #lm(mpg ~ hp, data=mtcars) is an object of class "lm". summary(lm(mpg ~ hp, data=mtcars)) generates an object of class "summary.lm". R recognizes that the summary function is applied to a object of class "lm".
R then selects the appropriate summary function for that class.
#Some examples for summary functions: summary.lm, summary.glm, summary.reitsma (from the 3rd party package "mada" for meta-analysis of diagnostic accuracy studies).
#Another example:
library(mada)
data(AuditC)
summary(reitsma(AuditC))
class(reitsma(AuditC)) #"reitsma". A custom class created by the autor of the mada package in order to present the output of the reitsma function in the desired way.
(reitsma_auditc <- reitsma(AuditC)) #Nicely formatted because the package author created a method for objects of class reitsma.
class(reitsma_auditc) <- NULL #We now delete that class.
class(reitsma_auditc)
reitsma_auditc #The output is a basic (unformatted) list now.
class(reitsma_auditc) <- "reitsma" #We can reassign the original class.
reitsma_auditc #And now the output is formatted just like originally again!
		
#Example for working with classes:

library(ggplot2)
(dat <- msleep) # A tibble: 83 x 11 #nun finde ich diese "tibbles" ziemlich dämlich und hätte gerne wieder einen ganz "normalen" data.frame
class(dat) #[1] "tbl_df"     "tbl"        "data.frame"
old_dat_classes <- class(dat) #Backup
class(dat) <- NULL #lösche alle Klassen-Attribute. dat ist jetzt nur noch eine Liste
class(dat) <- old_dat_classes[3] #weise nur das 3. (=data.frame) Attribut wieder zu
dat
gdata::is.what(dat) #"is.data.frame" "is.list"       "is.object"     "is.recursive"
#Easier, but same results:
dat <- msleep
class(dat) <- "data.frame" #This simply overwrites the three classes "tbl_df", "tbl", and "data.frame" with only "data.frame", i.e. practically removes the other two classes.



#My first own "method": (s. http://www.programiz.com/r-programming/S3-class)
print.reitsma.se <- function(result){
 cat(100*round(result$sens$sens,1), 100*round(result$sens$sens.ci["2.5%"],1), 100*round(result$sens$sens.ci["97.5%"],1))
 }
attr(result, "class") <- "reitsma.se"
print(result) # wird geprintet mit der eben erstellten Definition/Methode, also als gerundete Zahlenfolge ohne besonderes Format.
methods(print) #mit neuer Methode "reitsma.se"
attr(result, "class") <- NULL #lösche vom Ergebnis wieder die eigene Methode
print(result) #default-Methode wird wieder verwendet
print.reitsma.se(result) #außer natürlich, wenn man explizit die eigene Funktion verwendet.

#From R Advanced:

####################
# R6 classes in R: #
####################

rm(list=ls(all=TRUE))
library(R6)
Accumulator <- R6Class("Accumulator", list(
	sum=0, #sum is a "field" of the "class" Accumulator
	add=function(x=1){
		self$sum <- self$sum + x
		invisible(self)
		} #add ist eine "method"
	)
)

x <- Accumulator$new()
str(x)
# Classes 'Accumulator', 'R6' <Accumulator>
  # Public:
    # add: function (x = 1) 
    # clone: function (deep = FALSE) 
    # sum: 0
	
x$add(4) #Has the "side-effect" that the value of "sum" is increased by the respective amount.
str(x)
# Classes 'Accumulator', 'R6' <Accumulator>
  # Public:
    # add: function (x = 1) 
    # clone: function (deep = FALSE) 
    # sum: 4 
# >
x$sum #4

x$add(1)$add(-2)$sum #3 (chaning)

#Each 'method' in its own row (readability):
x$
	add(-5)$
	add(10)$
	sum
#0
#(="method chaning")
