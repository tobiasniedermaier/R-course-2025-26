
#1. How to check if a variable is a character or a numeric?

dat <- iris
is.numeric(dat$Sepal.Length)
is.character(dat$Sepal.Length)
is.numeric(dat$Species)
is.character(dat$Species)

#How to check for all variables at once?

sapply(dat, is.numeric)
sapply(dat, is.character)

#2. Data cleaning

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

#3. Data distribution

#Create a uniformly distributed variable:
unifdist <- runif(n=10000, min=10, max=30)
hist(unifdist)

#Create a normally distributed variable:
normdist <- rnorm(n=1000, mean=5, sd=3)
hist(normdist)

#Create a new variable based on two individual variables:
dat <- women #built-in
dat$height_m <- dat$height * 2.54 /100 #heigth is recorded in inch and we want to convert it to m
dat$weight_kg <- dat$weight * 0.4535924
dat$bmi <- dat$weight_kg / dat$height_m^2

#Create a new dummy variable based on three individual variables:
mtcars$newvar <- ifelse(mtcars$mpg >20 & mtcars$hp>80 & mtcars$cyl==6, 1, 0)
mtcars$newvar2 <- ifelse(mtcars$mpg >20 & (mtcars$hp>80 | mtcars$cyl==6), 1, 0)

#t.test (2-sample):
t.test(mpg ~ am, data=mtcars) #Compares two groups from the same variable (mpg stratified by am).
t.test(iris$Sepal.Length, iris$Petal.Length) #Compares mean of Sepal.Length with mean of Petal.Length.

#4. Cross-tabulation:
table(mtcars$cyl, mtcars$am)
table(mtcars[,c("cyl","am")])
table(mtcars$cyl, mtcars$am, mtcars$vs)

#5. Logistic regression:
dat <- airquality
dat$Ozone_dicho <- ifelse(dat$Ozone >40, 1, 0)
#glm(Ozone_dicho ~ Temp + Wind, data=dat) #Attention! This is a LINEAR regression, same as:
#lm(Ozone_dicho ~ Temp + Wind, data=dat)

#But we want a logistic regression!
mylogreg <- glm(Ozone_dicho ~ Temp + Wind, data=dat, family="binomial")
mylogreg
summary(mylogreg)
coef(mylogreg) #log odds!
confint(mylogreg) #log CIs of odds!
exp(coef(mylogreg)) #normal scale
exp(confint(mylogreg)) #normal scale


#Mean, mode, median, Chi-squared, anova
dat <- mtcars
mean(dat$mpg)
#With NAs:
dat[5,3] <- NA
dat
mean(dat$disp)
mean(dat$disp, na.rm=TRUE)

#Mean of ALL variables:
sapply(dat, mean)
sapply(dat, mean, na.rm=TRUE)

#Mode:
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

#Median:
median(vec)

#Chi-squared:
mytab <- table(mtcars$cyl, mtcars$am)
chisq.test(mytab)
fisher.test(mytab)

#ANOVA:
lm1 <- lm(mpg ~ wt + cyl + hp, data=mtcars)
lm2 <- lm(mpg ~ wt + cyl, data=mtcars)
anova(lm1, lm2)

##Visualisation:

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
#see also https://intro2r.com/simple-base-r-plots.html


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
