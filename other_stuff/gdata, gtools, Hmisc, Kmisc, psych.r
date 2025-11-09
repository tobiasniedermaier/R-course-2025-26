
#######################
# The "gdata" package #
#######################

library(gdata)
#useful functions:
is.what() #returns which is.() queries are TRUE. Practically an all-in-one function of is.numeric, is.list, is.vector, is.array etc.
#df[df == -99] <- NA (base R solution to recode -99 to NA)
#df <- unknownToNA(df, -99, warning=FALSE) #Same in gdata. (Not much better if you ask me.)
x <- c(1, 2, 3, 5, NA, 6, 7, 1, NA)
length(x) #OK, but what if you only want to know the number of values which are not NA?
length(x[which(!is.na(x))]) #base-R
nobs(x) #gdata equivalent
n_obs <- function(x){sum(!is.na(x))} #self-written equivalent
#cbindX works like cbind, but can also concatenate vectors/matrices of different length. 
#Missing/"empty" values are filled up with NA.
ls.funs() #returns a character vector giving the names of (user-defined) function objects in the specified environment
ls.funs("package:gdata") #Lists all the functions of the gdata package.
#Rename several variables at once:
mtcars2 <- rename.vars(mtcars, from=c("mpg","cyl","disp"), to=c("MPG","Cylinders","Dispersion"))
#Rename objects:
(a <- 1:10)
mv("a", "A")
a
A
#Create a vector from a matrix:
(m <- matrix(letters[1:10], ncol=5))
dim(m) <- NULL #base-R
m
(m <- matrix(letters[1:10], ncol=5))
unmatrix(m) #gdata

#Show arguments of a function:
args(cat) #base-R. Drawback: Arguments are comma-seperated, thus hard to read.
Args(cat) #Same in gdata, arguments are nicely listed in a column, along with their possible values next to them.
Args(cat, sort=T) #Arguments sorted alphabetically.
ll() #Shows all objects in the work space, similar to ls(). Difference: Shows them nicely sorted, along with their class and size in KB.
matchcols(iris, with="Sepal") #Shows all columns containing "Sepal".
matchcols(iris, with="Sepal", without="Length") #Shows all columns containing "Sepal" but not "Length".



#######################
# The "gtools" packge #
#######################

require(gtools) || install.packages("gtools", dependencies=T)

#ask (entspricht readline(prompt()) in base R):
silly <- function(){
 age <- ask("How old are you?\n")
 age <- as.numeric(age)
 cat("In 10 years you will be ", age+10, " years old!\n")
}

#Show loaded packages:
loadedPackages()

df1 <- data.frame(A=1:10, B=LETTERS[1:10], C=rnorm(10) )
df2 <- data.frame(A=11:20, D=rnorm(10), E=letters[1:10] )
smartbind(df1, df2) #Similar to rbind. However, it also works if variable do not exist in the other data frame, those are filled up with NA.

########################
# The 'Kmisc' package: #
########################

dat <- mtcars
library(Kmisc)
dapply(dat, mean) #sieht genauso aus wie die sapply-Version, ist aber ein data.frame, s. 
is.data.frame(dapply(dat, mean))
list2df(lapply(dat, mean)) #auch aus 'Kmisc'
(x <- c(1,2,3,"A","B","C",4,5,6,NA,NA,NA,7,8,9))
remove_chars(x)
remove_digits(x)
remove_na(x)
# tapply_ equals tapply, but is faster
# factor_ equals factor, but is faster
# anatomy equals str, but is faster


###########################################
# The 'Hmisc' package 
#(which has a lot more useful functions):
###########################################

library(Hmisc)
test2[1:5,c("INC88","TAX88")] #base-R
test2[1:5,Cs(INC88,TAX88)] #Same with Hmisc (where you don't need the quotation marks for each variable)
describe(test2)

#######################
# The 'psych' package #
#######################

library(psych)
describe(mtcars)
describeBy(mtcars, mtcars$am)
headTail(mtcars)
corPlot(mtcars)
corr.test(mtcars)


