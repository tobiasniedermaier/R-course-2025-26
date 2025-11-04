
#Load a package whicih contains a data set:
library(ISLR)
head(Auto)

dat <- Auto

#1. How to check if a variable is a character or a numeric?

## Check if a variable is character or numeric:
is.numeric(dat$mpg)
is.numeric(dat$name)
sapply(dat, is.numeric)
is.character(iris$Species)

dat <- Credit #Credit is a data set that is included in the package ISLR which we loaded with library(ISLR). If the package is not installed, first type install.packages("ISLR", dependencies=TRUE)

#How to check for all variables at once?
sapply(dat, is.numeric)
sapply(dat, is.character)
sapply(dat, is.factor)

#Standard deviation for all variables:
lapply(mtcars, sd)
sapply(mtcars, sd)

sd(mtcars) #This does not work because the function sd() is not vectorized. However, we can turn it into a vectorized function:
vsd <- Vectorize(sd)
vsd(mtcars) #same as:
Vectorize(sd)(mtcars)

## Data cleaning:

head(airquality)
#check for missing values:
summary(airquality)
sapply(airquality, function(x) sum(is.na(x)))
mean(airquality$Ozone) #Doesn't work because of NA values
mean(airquality$Ozone, na.rm=TRUE)

#Another example with the Credit data set (remember that we assigned it to an object called dat):

#Let's assume that more than 5 cards are implausible and we need to set them to missing (NA):
table(dat$Cards)
dat[dat$Cards>5,]$Cards <- NA

#Let's assume that the combination of Income <100 and Limit >3500 is implausible and we know that Income is reliable and there is a limit of 6000 for those individuals.
dat[dat$Income <100 & dat$Limit >6000,]
dim(dat[dat$Income <100 & dat$Limit >6000,])
nrow(dat[dat$Income <100 & dat$Limit >6000,])
ncol(dat[dat$Income <100 & dat$Limit >6000,])
dat[dat$Income <100 & dat$Limit >6000,]$Limit <- 6000
dat[dat$Income <100 & dat$Limit >6000,]

## Data distribution

hist(dat$Income)
hist(dat$Income, breaks=30)
boxplot(dat$Income)
boxplot(dat$Income, horizontal=TRUE)
lattice::densityplot(dat$Income)
library(lattice)
densityplot(dat$Income)
bwplot(dat$Income)
xyplot(Limit ~ Married, data=dat)
qqnorm(dat$Income)

#Create a uniformly distributed variable:
unifdist <- runif(n=10000, min=10, max=30)
hist(unifdist)

#Create a normally distributed variable:
normdist <- rnorm(n=1000, mean=5, sd=3)
hist(normdist)

## Create a dummy variable (dichotomized):
median(dat$Income)
dat$Income_bin <- ifelse(dat$Income > 33, "high", "low")
dat$Income_factor <- factor(dat$Income_bin)
#more examples:
mtcars$newvar <- ifelse(mtcars$mpg >20 & mtcars$hp>80 & mtcars$cyl==6, 1, 0)
mtcars$newvar2 <- ifelse(mtcars$mpg >20 & (mtcars$hp>80 | mtcars$cyl==6), 1, 0)

## Create a new variable from conditions of several other variables:
dat$married_man <- NA
levels(dat$Gender) #We first want to fix (remove) the trailing " " for Male:
levels(dat$Gender)[1] <- "Male" #If we had a "normal" (non-factor) variable, we would recode it as follows:
#dat[dat$Gender==" Male",]$Gender <- "Male"

dat[dat$Gender == "Male" & dat$Married == "Yes",]$married_man <- 1
dat[is.na(dat$married_man),]$married_man <- 0

dat$married_woman <- NA
dat[dat$Gender == "Female" & dat$Married == "Yes",]$married_woman <- 1
dat[is.na(dat$married_woman),]$married_woman <- 0

dat$not_married <- NA
dat[dat$Married == "No",]$not_married <- 1
dat[dat$Married == "Yes",]$not_married <- 0

dat$rich_caucasian_man <- NA
dat[dat$Gender=="Male" & dat$Ethnicity=="Caucasian" & dat$Income_factor=="high",]$rich_caucasian_man <- 1
dat[is.na(dat$rich_caucasian_man),]$rich_caucasian_man <- 0

#Or:
dat$rich_caucasian_man2 <- ifelse(dat$Gender=="Male" & dat$Ethnicity=="Caucasian" & dat$Income_factor=="high", 1, 0)
table(dat$rich_caucasian_man, dat$rich_caucasian_man2)

#ifelse() is easier/less code to write if you want only two categories (dichotomization). If you have three or more categories, I would recommend conditional subsetting like the definition of the variables "married_man" etc. from above.

#Another example (different data set):
dat <- women #built-in in R
dat$height_m <- dat$height * 2.54 /100 #heigth is recorded in inch and we want to convert it to m
dat$weight_kg <- dat$weight * 0.4535924
dat$bmi <- dat$weight_kg / dat$height_m^2
       
# An example from a different data set (also built-in in R):

head(airquality)
#check for missing values:
summary(airquality)
sapply(airquality, function(x) sum(is.na(x)))
mean(airquality$Ozone) #Doesn't work because of NA values
mean(airquality$Ozone, na.rm=TRUE)

#Get an impression of the data set (mean, median, min, max etc. of all variables):
summary(airquality)
boxplot(airquality)

#Dichotomize a variable:
airquality$Ozone_dicho <- ifelse(airquality$Ozone >40, "high", "low")

#Practically remove outliers by setting them to the xth and 1-xth percentile:
library(psych)
airquality$Ozone_w05 <- winsor(airquality$Ozone, trim=0.05)
hist(airquality$Ozone)
hist(airquality$Ozone_w05)


#t-test:
#Are men richer than women?
t.test(Income ~ Gender, data=dat) #Compares two groups from the same variable (Income stratified by Gender).
#Another example from a different data set:
t.test(iris$Sepal.Length, iris$Petal.Length) #Compares mean of Sepal.Length with mean of Petal.Length in the iris data set.
       
#Cross-tabulation:
table(dat$Income_factor, dat$Gender)
table(dat$Income_factor, dat$Gender, exclude=NULL) #Would show NAs if there were any.
table(dat[,c("Income_factor","Gender")])
table(dat[,c("Income_factor","Gender","Married")])
#More examples:
table(mtcars$cyl, mtcars$am)
table(mtcars[,c("cyl","am")])
table(mtcars$cyl, mtcars$am, mtcars$vs)

#Logistic regression:
datc <- dat[complete.cases(dat),]
dim(dat)
dim(datc) #we lose 17 cases but otherwise we cannot do an anove.

logreg <- glm(Married ~ Income + Education, data=datc, family="binomial") #Don't forget family="binomial"! If you leave it away, R will use the default option, which is "gaussian", i.e. a linear regression. 
summary(logreg)
coef(logreg) #log odds!
confint(logreg) #log CIs of odds!
exp(coef(logreg)) #normal scale
exp(confint(logreg)) #normal scale
#plot(logreg) #Model diagnostics

#Mean, mode, median, Chi-squared, anova
dat <- mtcars
mean(dat$mpg)
median(dat$mpg)
#With NAs:
dat[5,3] <- NA
dat
mean(dat$disp)
mean(dat$disp, na.rm=TRUE)

#Another example:
summary(datc)    
median(datc$Income)
#Income stratified by Gender:
tapply(datc$Income, datc$Gender, FUN=median)
aggregate(Income ~ Gender, data=datc, FUN=median)
#Income stratified by Gender and marital status:
aggregate(Income ~ Gender + Married, data=datc, FUN=median)
tapply(datc$Income, list(datc$Gender, datc$Married), FUN=median)

#Mean of ALL variables:
sapply(dat, mean)
sapply(dat, mean, na.rm=TRUE)

#Mode: Strangely, there is no built-in function in R to calculate the mode. But we can write one:
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}
find_mode(datc$Cards)
table(datc$Cards)
# In the following, I "decomposed" the function to explain the individual calculation steps of the function.
x <- datc$Cards #This is just so that we can work with an object "x" and don't need to write datc$Cards every time.
u <- unique(x) #unique(datc$Cards) returns the different possible values of the variable, in the order in which they first appear in the data set. (You could get the same - but in another data type (character) with names(table(datc$Cards)).)
x_u_matched <- match(x, u) #match() returns the position at which it finds the values of x in u. The first 5 elements of x are 2, 3, 4, 3, 2. in u (2, 3, 4, 5, 1), those are the positions 1, 2, 3, 2, 1. "5" occurs for the first time in the 9th value of x. It is the 4th value of u. So the 9th value of match(x, u) is 4. Every other "5" in the vector x will also get the value 4 as outcome of match(x, u).
tab <- tabulate(x_u_matched) #tabulate() counts the number of times each integer occurs in a vector. You could also use as.numeric(table(x_u_matched)).
u[tab == max(tab)] #max(tab) returns the maximum of tab, i.e. the value of tabulate(x_u_matched) with the most counts / which occurs most frequently. tab == max(tab) returns TRUE for the largest value (maximum), i.e. for the value with the most occurances, and FALSE for all other values. And finally, u[tab == max(tab)] subsets the vector u (the possible values) with the position that returns TRUE, i.e. the most frequent value. And this is the definition of the mode of a vector.

#A slightly different implementation:
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
vec <- c(-5, 3, 6, 3, 7)

#For understanding:
ux <- unique(vec)
ux[which.max(tabulate(match(vec, ux)))]
match(vec, ux)
tabulate(match(vec, ux))
which.max(tabulate(match(vec, ux)))
ux[which.max(tabulate(match(vec, ux)))]
#tabulate takes the integer-valued vector bin and counts the number of times each integer occurs in it.

       
#Chi-squared test:
mytab <- table(datc$Income_factor, datc$Married)
chisq.test(mytab)

#Fisher's exact test:
fisher.test(mytab)

#ANOVA:
logreg2 <- glm(Married ~ Income + Education + Cards, data=datc, family="binomial")
anova(logreg, logreg2) #Note that anova() for model comparison is only valid in case of nested models. I.e. you use the same data, the same response variable, and model 1 contains all the variables that are included in model 2, but model 2 has one or more further variables. (I.e. the smaller model is a true subset of the larger model.) If this is not the case, R will still produce an output and a p value, but this is not a valid comparison. A valid model comparison for models that are not nested would be AIC(model1, model2) or BIC(model1, model2), i.e. the Akaike and the Bayes Information Criterion (smaller is better).

#Another example with linear regresson:
lm1 <- lm(mpg ~ wt + cyl + hp, data=mtcars)
lm2 <- lm(mpg ~ wt + cyl, data=mtcars)
anova(lm1, lm2)

       
## Visualization:
boxplot(dat$Age) #Check ?boxplot for the parameters that you can add. For example:
boxplot(dat$Age, main="Boxplot of 'Age' in dat")
boxplot(dat$Age, ylim=c(0,120))
boxplot(dat$Age, col="yellow")
boxplot(dat$Age, horizontal = TRUE)
boxplot(datc$Age ~ datc$Income_bin)
boxplot(datc$Age ~ datc$Income_bin, width=c(0.5,2))

plot(dat$Ethnicity)
hist(dat$Balance)
hist(dat$Rating, col="blue")
hist(dat$Rating, breaks=20)
hist(dat$Rating, nclass=20) #For historical reasons (R originates from S-PLUS), basically the same as breaks.
densityplot(dat$Rating)
histogram(dat$Rating)
qqnorm(dat$Balance)
qqnorm(dat$Age)
qqplot(dat$Balance, dat$Age)
#qqplot() makes a two-sample Q–Q plot: it compares the empirical quantiles of two numeric variables.
#So qqplot(dat$Balance, dat$Age) plots the quantiles of Balance (x-axis) against the matching quantiles of Age (y-axis).
#Use qqplot(x, y) to compare two samples; use qqnorm(x) (or qqplot(x, qnorm(ppoints(length(x))))) to compare one sample vs Normal.

#More plotting examples with the iris dataset:

plot(iris) #bivariate scatter plot of all variables in a data frame.
plot(iris$Sepal.Length, iris$Petal.Width) #bivariate plot
plot(iris$Sepal.Length, iris$Petal.Width, pch=2, col="darkblue", main="Scatterplot", ylab="Petal Width", xlab="Sepal Length", ylim=c(0,3), yaxs="i")

hist(iris$Sepal.Length)
hist(iris$Sepal.Length, breaks=20)
#Check the help page of plot() and hist() to see the various options (function arguments) that you can add.

dens <- density(iris$Sepal.Length)
hist(iris$Sepal.Length, breaks=20, freq=FALSE)
lines(dens)

boxplot(mtcars$mpg, main="My first boxplot")
boxplot(mtcars$mpg, main="My second boxplot\nText in second row", sub="This is a subtitle", col="red", log="y")
boxplot(mtcars$mpg, main="My third boxplot", horizontal=TRUE)
boxplot(mpg ~ am, data=mtcars)


#Various plot types:
x <- rnorm(500)
y <- x + rnorm(500)

par(mfrow = c(2, 3))

# Data
my_ts <- ts(matrix(rnorm(500), nrow = 500, ncol = 1),
              start = c(1950, 1), frequency = 12)

my_dates <- seq(as.Date("2005/1/1"), by = "month", length = 50)

my_factor <- factor(mtcars$cyl)

square <- function(x){x^2}

# Scatterplot
plot(x, y, main = "Scatterplot")

# Barplot
plot(my_factor, main = "Barplot")

# Boxplot
plot(my_factor, rnorm(32), main = "Boxplot")

# Time series plot
plot(my_ts, main = "Time series")

# Time-based plot
plot(my_dates, rnorm(50), main = "Time based plot")

# Plot R function
plot(square, 0, 10, main = "Plot a function")
fun2 <- function(x){abs(x)}

x_values <- seq(-5, 5, 0.1)
y_values <- square(seq(-5, 5, 0.1))
plot(x_values, y_values, main = "Plot a function", type="l")
abline(h=10)
abline(v=0, lty=2)
       
#see also: 
#https://intro2r.com/simple-base-r-plots.html
#https://r-charts.com/base-r/line-types/
#https://r-graph-gallery.com/

## Manupulating variable names:
       
#Let's say you want all variable names to start with a small letter instead of a capital letter:
colnames(dat) #Only calling a function without assignment usually does not change the values of any variable or data set. (There are exceptions, though.)
colnames(dat) <- tolower(colnames(dat))

#And now you want them all to be in capital letters:
colnames(dat) <- toupper(colnames(dat))

#And now again the fist letter capitalized and the rest small:
colnames(dat) <- paste0(toupper(substring(colnames(dat), first=1, last=1)), tolower(substring(colnames(dat), 2)))
#substring(colnames(dat), 2) is short for substring(colnames(dat), first=2, last=nchar(colnames(dat))). It takes every character starting from the second.

#String manipulation:
gsub("Y", "y", dat$Married)
#More explicit:
gsub(pattern="Y", replacement="y", x=dat$Married) #pattern = what to replace. replacement = with what to replace the pattern. x = the vector in which something should be replaced.

gsub("N", "n", dat$Married)

#Vectorized version of gsub from an addon-package:
library(kutils)
mgsub(c("Y","N"), c("y","n"), dat$Married) #Needs the addon-package kutils. If it is not installed: install.packages("kutils", dep=T); library(kutils)




