
#####################
# das "gdata"-Paket #
#####################

library(gdata)
#nützliche Funktionen:
is.what() #gibt einem aus, was für is.()-Abfragen TRUE sind, also AIO von is.numeric, is.list, is.vector, is.array etc.
#df[df == -99] <- NA (base R, um als -99 kodierte missing in NA umzukodieren)
#df <- unknownToNA(df, -99, warning=FALSE) #Pendant in gdata (eigentlich nicht besser)
x <- c(1,2,3,5,NA,6,7,1,NA )
length(x) #ok, aber wenn man nur die Anzahl der Nicht-NA-Werte wissen will?
length(x[which(!is.na(x))]) #base-R
nobs(x) #gdata-Äquivalent
n_obs <- function(x){sum(!is.na(x))} #selbstgeschriebenes Pendant
#cbindX funktioniert wie cbind, kann aber auch Vektoren/Matrizen unterschiedlicher 
#Länge/Dimensionen zusammenfügen, "Leerstellen" werden mit NA aufgefüllt.
ls.funs() #returns a character vector giving the names of function objects in the specified environment (gibt also alle selbstdefinierten Funktionen aus)
ls.funs("package:gdata") #zeigt alle Funktionen im Paket gdata an.
#mehrere Variablen auf einmal umbenennen:
mtcars2 <- rename.vars(mtcars, from=c("mpg","cyl","disp"), to=c("MPG","Cylinders","Dispersion"))
#Objekte umbenennen:
(a <- 1:10)
mv("a", "A")
a
A
#Vektor aus Matrix erstellen:
(m <- matrix(letters[1:10], ncol=5))
dim(m) <- NULL #base-R
m
(m <- matrix(letters[1:10], ncol=5))
unmatrix(m) #gdata-Äquivalent

#Argumente einer Funktion anzeigen lassen:
args(cat) #base-R. Nachteil: werden mit Komma getrennt nebeneinander augelistet, daher schwer lesbar.
Args(cat) #Pendant v. gdata, bei dem die Argumente schön untereinander aufgelistet werden und die Ausprägungen daneben.
Args(cat, sort=T) #sortiert die Argumente alphabetisch
ll() #zeigt alle Objekte im Workspace an, wie ls(). Unterschied: zeigt sie schön sortiert untereinander an, zusammen mit der Klasse und der Größe in KB.
matchcols(iris, with="Sepal") #gibt alle Spalten aus, in denen "Sepal" vorkommt.
matchcols(iris, with="Sepal", without="Length") #gibt alle Spalten aus, in denen "Sepal" vorkommt, aber nicht "Length".



######################
# das "gtools"-Paket #
######################

require(gtools) || install.packages("gtools", dependencies=T)

#ask (entspricht readline(prompt()) in base R):
silly <- function(){
 age <- ask("How old are you?\n")
 age <- as.numeric(age)
 cat("In 10 years you will be ", age+10, " years old!\n")
}

#geladene Pakete anzeigen lassen:
loadedPackages()

df1 <- data.frame(A=1:10, B=LETTERS[1:10], C=rnorm(10) )
df2 <- data.frame(A=11:20, D=rnorm(10), E=letters[1:10] )
smartbind(df1, df2) #entspricht rbind, aber wenn Variablen im anderen DF nicht vorkommen, funktioniert es trotzdem; diese werden mit NA aufgefüllt.

#Das Paket kann eigentlich doch nicht so viel Sinnvolles, wie ich dachte.

######################
# das 'Kmisc'-Paket: #
######################

#dapply() ist wie l/sapply, gibt aber einen data.frame aus. Bsp.:
dat <- mtcars
#Mittelwert jeder Variablen:
lapply(dat, mean) #sehr hässlich
sapply(dat, mean) #schöner, aber kein data.frame
library(Kmisc)
dapply(dat, mean) #sieht genauso aus wie die sapply-Version, ist aber ein data.frame, s. 
is.data.frame(dapply(dat, mean))
list2df(lapply(dat, mean)) #auch aus 'Kmisc'
(x <- c(1,2,3,"A","B","C",4,5,6,NA,NA,NA,7,8,9))
remove_chars(x)
remove_digits(x)
remove_na(x)
# tapply_ entspricht tapply, ist aber schneller
# factor_ entspricht factor, ist aber schneller
# anatomy entspricht str, ist aber schneller


###########################################
# das 'Hmisc'-Paket 
#(das noch einige weitere nützliche Funktionen beinhaltet):
###########################################

library(Hmisc)
test2[1:5,c("INC88","TAX88")] #base-R
test2[1:5,Cs(INC88,TAX88)] #Pendant in Hmisc (man spart sich die Anführungszeichen bei jeder Variablen)
describe(test2)

#####################
# das 'psych'-Paket #
#####################

library(psych)
describe(mtcars)
describeBy(mtcars, mtcars$am)
headTail(mtcars)
corPlot(mtcars)
corr.test(mtcars)

