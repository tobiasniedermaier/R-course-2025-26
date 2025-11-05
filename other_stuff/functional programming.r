
#functional programming
#http://www.quantide.com/ramarro-chapter-05/

#anonymous functions:
(function(x) x^2)(x=5) #25
#Idee: wenn man Funktion nur ein oder zwei mal braucht und es sich nicht lohnt dafür eine Funktion mit Namen zu schreiben.


#We may consider a set of functions, say f1(x), f2(x), ..., fi(x), ..., fn{x} that add 1,2,...,i,...,n to the x argument. Clearly, we do not want to define all these functions but we want to define a unique function f(i):
f <- function(i){
  function(x) {x+i}
}

f1 <-  f(1) #f1 rechnet jetzt also x+1
f1(3) #4

f2 <- f(2) #f2 rechnet x+2
f2(4) #6

f_min_5 <- f(-5) #f_min_5 rechnet x-5
f_min_5(8) #3

f(2)(3) #5 ("Currying")

#noch eine Stufe "Currying":
f <- function(a){
  function(b){
    function(c)
      a+b+c
   }
  }
f(2)(3)(4)
g <- f(2)
g(3)(4)
h <- g(3)
h(4)

#to be continued, s. http://www.grappa.univ-lille3.fr/~staworko/teaching/r18/01.r.pdf


#mapply:
mapply(mean, mtcars) #entspricht sapply(mtcars, mean)
#(noch keinen echter Nutzen/Vorteil)
add_n <- function(x, y=1){x + y}
sapply(mtcars, add_n)
add_n(mtcars) #identisch, weil einfache Funktion, die von sich aus vektorisiert ist
add_n <- function(x, y){x + y}
add_n(mtcars, y=1) #würde gehen, weil einfache Funktion, die von sich aus vektorisiert ist
sapply(mtcars, add_n(y=1)) #geht nicht
mapply(add_n, MoreArgs=list(y=1), mtcars) #geht
sapply(mtcars, add_n, y=1) #geht auch!
mapply(add_n, MoreArgs=list(y=seq(1,32,1)), mtcars)
sapply(mtcars, add_n, y=seq(1,32,1)) #identisch

#Functionals are functions that take a function as input and return a data object as output.
l <- list(x = 1:4, y = 4:1)
Reduce(rbind, l)
#entspricht im Prinzip:
do.call(rbind, l)

#We may even define a function fapply() that applies all functions of a list to the same set of arguments
fapply <- function(X, FUN, ...){
  lapply(FUN, function(f, ...){f(...)}, X, ...)
}

basic_stat <- list(mean = mean, median = median, sd = sd)
fapply(1:10, basic_stat)
unlist(fapply(1:10, basic_stat))

#wie ich es gemacht hätte:
basicstats <- function(x, ...){
 res <- t(data.frame(mean(x, ...), median(x, ...), sd(x, ...)))
 rownames(res) <- c("Mean","Median","SD")
 return(res)
}
basicstats(c(1, 4, 5, 6, 7, 10, 20))

#"selbstgebaute" Version von sapply: (Bsp. aus Thomas Mailund's Buch "Functional Programming in R", Kap. 4 "Higher-Order Functions")
myapply <- function(x, f, ...){
  result <- x
  for (i in seq_along(x)) {result[i] <- f(x[i])}
  result
}
myapply(1:4, sqrt)
myapply(mtcars, add_n, y=1)

#higher order functions:
#https://renkun.me/2014/03/15/a-brief-introduction-to-higher-order-functions-in-r/
add <- function(x, y, z) {
  x+y+z
}

#oder besser, damit beliebig viele Summanden:
add <- function(x, ...){
  sum(x, ...)
}
add(3,4,5,6)

product <- function(x, y, z) {
  x*y*z
}

aggreg <- function(x, y, z, fun) {
  fun(x,y,z)
}

aggreg(1,2,3, product)

#higher order functions II:
#http://www.johnmyleswhite.com/notebook/2010/09/23/higher-order-functions-in-r/

#http://www.r-bloggers.com/higher-order-functions-exercises/

#Exercise 3
##Vektor 1:10. Ich will alle Zahlen mit 2 und mit 4 multiplizieren. Wie?
Map(`*`, list(1:10), c(2,4)) #"Musterlösung"
do.call("cbind", lapply(1:10, `*`,c(2,4))) #meine Lösung
for (i in c(2,4)){print((1:10)*i)} #"Schleifenlösung" von mir
mapply(`*`, MoreArgs=list(c(2,4)), 1:10) #meine mapply-Lösung (Map und mapply sind dasselbe, nur Map hat simplify=FALSE als default, mapply dagegen simplify=TRUE)
sapply(1:10, `*`,c(2,4)) #noch eine Lösung von mir

#Exercise 4
#Expression sample(LETTERS, 5, replace=TRUE) obtains 5 random letters.
#Generate a list with 10 elements, where first element contains 1 random letter, second element 2 random letters and so on.
#Note: use a fixed random seed: set.seed(14)
set.seed(14)
FUN <- function(i){sample(LETTERS, i, replace=TRUE)}
lapply(1:10, FUN) #gleiches Ergebnis wie in "Musterlösung" ebd. verwendet aber Map:
set.seed(14)
Map(sample, list(LETTERS), 1:10, replace=TRUE)
#for-Schleifenlösung:
res <- list()
for (i in 1:10){
  res[[i]] <- sample(LETTERS, i, replace=TRUE)
}

#Exercise 5
#Find all prime numbers between 100 and 200
is.prime <- Vectorize(function(n){
  ifelse(sum(n %% (1:n) == 0) > 2, FALSE, TRUE)
}, vectorize.args = "n")
c(100:200)[is.prime(100:200)]
Filter(is.prime, 100:200)
res <- sapply(100:200, is.prime) * 100:200
res <- res[res>0] #wenn man aus irgendeinem Grund weder c() noch Filter() verwenden will

#Exercise 6:
words <- read.table("D:\\Daten Samsung Notebook\\Statistik\\R üben\\wordsEn.txt")
#a. Using a function that checks if a given words contains any vowels:
containsVowel <- function(x) grepl("a|o|e|i|u", x)
#find all words that do not contain any vowels.
noVowel <- Negate(containsVowel)
noVowel2 <- function(x){!containsVowel(x)} #wie man es ohne Negate() machen könnte
lapply(words, containsVowel) #gedanklicher Zwischenschritt
words[sapply(words, noVowel)] #ok.
#Musterlösung wäre angeblich: Filter(Negate(containsVowel), words)
#funktioniert aber nicht

#Exercise 7:
#a) find the smallest number between 10000 and 20000 that is divisible by 1234
numbers <- seq(10000, 20000, 1)
divisible_by_1234 <- function(x){
  if (x %% 1234 == 0) {return(TRUE)} else {return(FALSE)}
}
#oder kürzer:
div_by_1234 <- function(x){
  x %% 1234 == 0
}

Filter(divisible_by_1234, numbers)[1]

#b) find the largest number between 10000 and 20000 that is divisible by 1234
Filter(divisible_by_1234, numbers)[length(Filter(divisible_by_1234, numbers))] #braucht 0.27 Sek. auf Netbook
#oder:
last <- function(x){x[length(x)]}
last(Filter(divisible_by_1234, numbers)) #braucht 0.14 Sek. auf Netbook
#Musterlösung:
Find(divisible_by_1234, 10000:20000, right=TRUE) #braucht nur 0.01 Sek. auf Netbook! Würde sich also durchaus lohnen, die "Find"-Funktion in mein Repertoire aufzunehmen.

#Exercise 8:
library(babynames)
babynames
namesData <- split(babynames$name, babynames$year)
#a) Obtain a set of names that were present in every year (gemeint: Schnittmenge, also Namen, die in jedem Jahr wieder auftauchen)

intersect(namesData[[1]], namesData[[2]]) #Namen, die in den ersten beiden Jahren auftauchen
Reduce(intersect, namesData)

#b) names that are only present in year 2014:
names_2014 <- namesData$`2014`
#eigenartige Musterlösung, die ich nicht verstehe und ich komme auch auf keine einfachere.

#Exercise 9:
moreThan3 <- function(x) nchar(x) > 3
#Inside each year list leave only the names that have 3 letters or less.
lessThan4 <- Negate(moreThan3)
lapply(namesData, lessThan4) #Zwischenschritt

#Musterlösung, auf die ich nie gekommen wäre:
namesLessThan4 <- Map(Filter, list(Negate(moreThan3)), namesData)

#Alternative Lösung von mir:
which_moreThan3 <- function(x){x[nchar(x)>3]}
lapply(namesData, which_moreThan3)
#tats. Anzahl der Buchstaben in jedem Namen:
lapply(lapply(namesData, which_moreThan3), nchar)
#Probe:
which_lessThan4 <- function(x){x[nchar(x)<=3]}
lapply(namesData, which_lessThan4)
#und wenn man lieber eine Funktion in der Form f(x) anwenden will anstatt lapply(x, f), kann man auch folgendes machen:
x_which_lessThan4 <- function(data){
  lapply(data, which_lessThan4)
}
x_which_lessThan4(namesData)


x_which_less_than_n <- function(data, n){
  which_lessThan_n <- function(x){x[nchar(x)<n]}
  lapply(data, which_lessThan_n)
}
x_which_less_than_n(data=namesData, n=4)


#mehr Interessantes aus #http://www.quantide.com/ramarro-chapter-05/
function_list <- list(m=mean, s=sd) #erstellt eine Liste von Funktionen
x <- 1:10
function_list[[1]] #1. Funktion in der Liste (mean)
function_list[[1]](1:10) #1. Funktion auf 1:10 angewendet
function_list$m(1:10) #Zugriff auf die 1. Funktion (m) über den Namen anstatt über Position in Liste
function_list[[2]](1:10) #2. Funktion auf 1:10 angewendet
mymean <- function_list[[1]] #1. Funktion einem Objekt zugewiesen
mymean(1:10) #5.5
fun <- function(f, ...){f(...)} #function that takes a function f() as argument along with any other arguments ‘’...’’ and returns f(...). 
lapply(function_list, fun, 1:10)


#Regressionen "automatisiert" machen:
xyform <- function(y_var, x_vars){
 as.formula(sprintf("%s ~ %s",
 y_var, paste(x_vars, collapse="+")))
} 

lm(xyform("mpg",c("cyl","hp","wt")), data=mtcars)
glm(xyform("am",c("cyl","hp","wt")), data=mtcars)

#y-Variable deparsed, sodass man keine Anführungszeichen dafür braucht:
xyform <- function(y_var, x_vars){
 y_var <- deparse(substitute(y_var))
 as.formula(sprintf("%s ~ %s",
 y_var, paste(x_vars, collapse="+")))
} 
lm(xyform(mpg,c("cyl","hp","wt")), data=mtcars)

library(Hmisc) #um auch keine Anführungszeichen für die x-Variablen zu brauchen:
lm(xyform(mpg, Cs(cyl,hp,wt)), data=mtcars)
