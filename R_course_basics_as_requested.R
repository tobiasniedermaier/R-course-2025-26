
# ===============================================================
# R WORKSHOP: Essentials Cheat Sheet
# ===============================================================
# This script covers:
# - Type checks (numeric, character, factor) incl. for all columns
# - Data cleaning (missing values, implausible values, winsorization, column name fixes, string ops)
# - Distributions & quick exploratory data analysis
# - Dummy variables & new variables from multiple conditions
# - t-tests, cross-tabulations
# - Logistic regression (with model comparison)
# - Mean, median, mode; Chi-squared; ANOVA
# - Visualization (base R + lattice)
#
# Notes:
# - Requires some optional packages: psych, ISLR, lattice, kutils (install if needed)
# - Examples use built-in datasets (iris, mtcars, airquality, women) and ISLR datasets (Auto, Credit)
# ===============================================================

## ---------------------------------------------------------------
## 0) Packages & Data
## ---------------------------------------------------------------
# install.packages(c("psych","ISLR","lattice","kutils"), dependencies = TRUE)
suppressPackageStartupMessages({
  library(psych)    # winsor()
  library(ISLR)     # Auto, Credit
  library(lattice)  # densityplot, bwplot, xyplot, histogram
  # library(kutils) # mgsub(); uncomment if installed
})

# Quick peeks at ISLR data (will load with library(ISLR))
head(Auto)
dat_isrl <- Auto
head(Credit)
dat_credit <- Credit

## ---------------------------------------------------------------
## 1) Type checks (character vs numeric etc.)
## ---------------------------------------------------------------
dat <- iris
# Single variable:
is.numeric(dat$Sepal.Length)
is.character(dat$Sepal.Length)
is.numeric(dat$Species)
is.character(dat$Species)
is.factor(dat$Species)

# All variables at once:
sapply(dat, is.numeric)
sapply(dat, is.character)
sapply(dat, is.factor)

# From ISLR example:
sapply(dat_isrl, is.numeric)

## ---------------------------------------------------------------
## 2) Data cleaning
## ---------------------------------------------------------------
# 2a) Missing values (airquality)
head(airquality)
summary(airquality)                                   # NAs overview
sapply(airquality, function(x) sum(is.na(x)))         # NA counts

# Mean with/without NAs:
mean(airquality$Ozone)                                # NA
mean(airquality$Ozone, na.rm = TRUE)

# Quick data overview:
summary(airquality)

# 2b) Dichotomize variable:
airquality$Ozone_dicho <- ifelse(airquality$Ozone > 40, "high", "low")

# 2c) Winsorize outliers (practical trimming at 5%):
airquality$Ozone_w05 <- winsor(airquality$Ozone, trim = 0.05)
hist(airquality$Ozone)
hist(airquality$Ozone_w05)

# 2d) Standard deviation for all variables (vectorization demo)
lapply(mtcars, sd)
sapply(mtcars, sd)

# sd() is not vectorized over data frames by default:
# vsd <- Vectorize(sd) ; vsd(mtcars)
vsd <- Vectorize(sd)
vsd(mtcars)

# 2e) Implausible values (ISLR::Credit)
table(dat_credit$Cards)
# Suppose >5 cards is implausible → set to NA
dat_credit$Cards[dat_credit$Cards > 5] <- NA

# Suppose Income <100 AND Limit >6000 is implausible; set Limit to 6000 in those rows
nrow(dat_credit[dat_credit$Income < 100 & dat_credit$Limit > 6000, ]) #before
dat_credit[dat_credit$Income < 100 & dat_credit$Limit > 6000, "Limit"] <- 6000 #same as dat_credit[dat_credit$Income < 100 & dat_credit$Limit > 6000,]$Limit <- 6000
nrow(dat_credit[dat_credit$Income < 100 & dat_credit$Limit > 6000, ]) #after

# 2f) Column name cleanups
colnames(dat_credit)
colnames(dat_credit) <- tolower(colnames(dat_credit))               # all lower case
colnames(dat_credit) <- toupper(colnames(dat_credit))               # all upper case
# Capitalize first letter, lower case for the rest:
colnames(dat_credit) <- paste0(toupper(substring(colnames(dat_credit), 1, 1)),
                               tolower(substring(colnames(dat_credit), 2)))

# 2g) String manipulation (e.g., normalize Y/N)
# gsub("Y", "y", dat_credit$Married)
# gsub("N", "n", dat_credit$Married)
# If you have kutils installed:
# mgsub(c("Y","N"), c("y","n"), dat_credit$Married)

## ---------------------------------------------------------------
## 3) Data distribution & quick EDA
## ---------------------------------------------------------------
# Simulated uniform & normal:
unifdist <- runif(n = 10000, min = 10, max = 30)
hist(unifdist)

normdist <- rnorm(n = 1000, mean = 5, sd = 3)
hist(normdist)

# Density overlay (iris):
dens <- density(iris$Sepal.Length)
hist(iris$Sepal.Length, breaks = 20, freq = FALSE, main = "Sepal.Length with density")
lines(dens)

# Lattice quick looks (ISLR::Credit):
hist(dat_credit$Income)
hist(dat_credit$Income, breaks = 30)
boxplot(dat_credit$Income, horizontal = TRUE)
densityplot(dat_credit$Income)
bwplot(dat_credit$Income)
xyplot(Limit ~ Married, data = dat_credit)
qqnorm(dat_credit$Income); qqline(dat_credit$Income)

# Time-series & time-based examples
x <- rnorm(500); y <- x + rnorm(500)
par(mfrow = c(2, 3))
my_ts <- ts(matrix(rnorm(500), nrow = 500, ncol = 1), start = c(1950, 1), frequency = 12)
my_dates <- seq(as.Date("2005/1/1"), by = "month", length = 50)
my_factor <- factor(mtcars$cyl)
square <- function(z) z^2

plot(x, y, main = "Scatterplot")
plot(my_factor, main = "Barplot")
plot(my_factor, rnorm(32), main = "Boxplot")
plot(my_ts, main = "Time series")
plot(my_dates, rnorm(50), main = "Time-based plot")
plot(square, 0, 10, main = "Plot a function")
par(mfrow = c(1, 1))

## ---------------------------------------------------------------
## 4) Create new variables (dummy & multi-condition)
## ---------------------------------------------------------------
# 4a) From units (women dataset → BMI)
dat_w <- women                    # built-in data
dat_w$height_m <- dat_w$height * 2.54 / 100 #heigth is recorded in inch and we want to convert it to m
dat_w$weight_kg <- dat_w$weight * 0.4535924 #weight is recorded in pounds (lbs) and we want to convert it to kg
dat_w$bmi <- dat_w$weight_kg / dat_w$height_m^2

# 4b) Dummy from median (ISLR::Credit Income)
median_income <- median(dat_credit$Income, na.rm = TRUE)
dat_credit$Income_bin    <- ifelse(dat_credit$Income > median_income, "high", "low")
dat_credit$Income_factor <- factor(dat_credit$Income_bin)

# 4c) Multi-condition dummy (mtcars)
mtcars$newvar  <- ifelse(mtcars$mpg > 20 & mtcars$hp > 80 & mtcars$cyl == 6, 1, 0)
mtcars$newvar2 <- ifelse(mtcars$mpg > 20 & (mtcars$hp > 80 | mtcars$cyl == 6), 1, 0)

# 4d) Multi-condition dummies (ISLR::Credit)
# Fix factor level (remove stray space if needed) and create marital status/group flags
dat_credit$married_man <- NA
# Example of level fix if there were trailing spaces:
# levels(dat_credit$Gender)[1] <- "Male"
dat_credit$married_man[dat_credit$Gender == "Male"   & dat_credit$Married == "Yes"] <- 1
dat_credit$married_man[is.na(dat_credit$married_man)] <- 0

dat_credit$married_woman <- NA
dat_credit$married_woman[dat_credit$Gender == "Female" & dat_credit$Married == "Yes"] <- 1
dat_credit$married_woman[is.na(dat_credit$married_woman)] <- 0

dat_credit$not_married <- NA
dat_credit$not_married[dat_credit$Married == "No"]  <- 1
dat_credit$not_married[dat_credit$Married == "Yes"] <- 0

dat_credit$rich_caucasian_man <-
  ifelse(dat_credit$Gender == "Male" & dat_credit$Ethnicity == "Caucasian" &
           dat_credit$Income_factor == "high", 1, 0)

#Or:
dat$rich_caucasian_man <- NA
dat[dat$Gender=="Male" & dat$Ethnicity=="Caucasian" & dat$Income_factor=="high",]$rich_caucasian_man <- 1
dat[is.na(dat$rich_caucasian_man),]$rich_caucasian_man <- 0
table(dat$rich_caucasian_man, dat$rich_caucasian_man2)

## ---------------------------------------------------------------
## 5) t-tests
## ---------------------------------------------------------------
# Two groups within a variable (mtcars: mpg by am):
t.test(mpg ~ am, data = mtcars)

# Two numeric vectors:
t.test(iris$Sepal.Length, iris$Petal.Length)

# Example from ISLR::Credit: Are men richer than women?
t.test(Income ~ Gender, data = dat_credit)

## ---------------------------------------------------------------
## 6) Cross-tabulation
## ---------------------------------------------------------------
table(mtcars$cyl, mtcars$am)
table(mtcars[, c("cyl","am")])
table(mtcars$cyl, mtcars$am, mtcars$vs)

# NAs shown or not:
table(dat_credit$Income_factor, dat_credit$Gender) #No NAs shown.
table(dat_credit$Income_factor, dat_credit$Gender, exclude = NULL) #Would show NAs if there were any.
table(dat_credit[, c("Income_factor","Gender","Married")])

## ---------------------------------------------------------------
## 7) Logistic regression
## ---------------------------------------------------------------
# 7a) airquality: ozone high (1) vs. predictors
dat_aq <- airquality
dat_aq$Ozone_dicho <- ifelse(dat_aq$Ozone > 40, 1, 0)

# GLM with binomial family (logistic regression)
mylogreg <- glm(Ozone_dicho ~ Temp + Wind, data = dat_aq, family = "binomial") #Attention: If you forget family="binomial", R will perform a linear regression instead of a logistic regression!
mylogreg
summary(mylogreg)
coef(mylogreg)                  # log-odds
confint(mylogreg)               # CI on log-odds
exp(coef(mylogreg))             # odds ratios
exp(confint(mylogreg))          # CI for ORs
#plot(logreg) #Model diagnostics

# 7b) ISLR::Credit: Married ~ Income + Education (+ Cards)
datc <- dat_credit[complete.cases(dat_credit),]
logreg1 <- glm(Married ~ Income + Education, data = datc, family = "binomial")
logreg2 <- glm(Married ~ Income + Education + Cards, data = datc, family = "binomial")

summary(logreg1)
exp(coef(logreg1)); exp(confint(logreg1))

# Model comparison (ANOVA for nested models only!)
anova(logreg1, logreg2, test = "Chisq")
# Note that anova() for model comparison is only valid in case of nested models. I.e. you use the same data, the same response variable, and model 1 contains all the variables that are included in model 2, but model 2 has one or more further variables. (I.e. the smaller model is a true subset of the larger model.) If this is not the case, R will still produce an output and a p value, but this is not a valid comparison.

# For non-nested comparisons, use Akaike and the Bayes Information Criterion (smaller is better):
AIC(logreg1, logreg2); BIC(logreg1, logreg2)

## ---------------------------------------------------------------
## 8) Mean, median, mode; Chi-squared; ANOVA
## ---------------------------------------------------------------
dat_mtc <- mtcars
mean(dat_mtc$mpg)

# NA-safe mean demos
dat_mtc[5,3] <- NA
mean(dat_mtc$disp)                 # NA
mean(dat_mtc$disp, na.rm = TRUE)   # OK

# Means for all variables:
sapply(dat_mtc, mean)
sapply(dat_mtc, mean, na.rm = TRUE)

# Mode (two implementations):
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
find_mode <- function(x) {
  u   <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}
vec <- c(-5, 3, 6, 3, 7)
Mode(vec); find_mode(vec)

# In the following, I "decomposed" the second function to explain the individual calculation steps of the function.
x <- datc$Cards #This is just so that we can work with an object "x" and don't need to write datc$Cards every time.
u <- unique(x) #unique(datc$Cards) returns the different possible values of the variable, in the order in which they first appear in the data set. (You could get the same - but in another data type (character) with names(table(datc$Cards)).)
x_u_matched <- match(x, u) #match() returns the position at which it finds the values of x in u. The first 5 elements of x are 2, 3, 4, 3, 2. in u (2, 3, 4, 5, 1), those are the positions 1, 2, 3, 2, 1. "5" occurs for the first time in the 9th value of x. It is the 4th value of u. So the 9th value of match(x, u) is 4. Every other "5" in the vector x will also get the value 4 as outcome of match(x, u).
tab <- tabulate(x_u_matched) #tabulate() counts the number of times each integer occurs in a vector. You could also use as.numeric(table(x_u_matched)).
u[tab == max(tab)] #max(tab) returns the maximum of tab, i.e. the value of tabulate(x_u_matched) with the most counts / which occurs most frequently. tab == max(tab) returns TRUE for the largest value (maximum), i.e. for the value with the most occurances, and FALSE for all other values. And finally, u[tab == max(tab)] subsets the vector u (the possible values) with the position that returns TRUE, i.e. the most frequent value. And this is the definition of the mode of a vector.


# Median:
median(vec)

# Stratified summaries (ISLR::Credit):
summary(datc)
median(datc$Income)
tapply(datc$Income, datc$Gender, FUN = median)
aggregate(Income ~ Gender, data = datc, FUN = median)
aggregate(Income ~ Gender + Married, data = datc, FUN = median)
tapply(datc$Income, list(datc$Gender, datc$Married), FUN = median)

# Chi-squared test:
mytab1 <- table(mtcars$cyl, mtcars$am)
chisq.test(mytab1)

mytab2 <- table(datc$Income_factor, datc$Married)
chisq.test(mytab2)

#exact Fisher test:
fisher.test(mytab1)

# ANOVA (linear models) & model comparison:
lm1 <- lm(mpg ~ wt + cyl + hp, data = mtcars)
lm2 <- lm(mpg ~ wt + cyl, data = mtcars)
anova(lm1, lm2)

## ---------------------------------------------------------------
## 9) Visualization (base R + lattice)
## ---------------------------------------------------------------
# Base scatterplots:
plot(iris)  # matrix of pairwise plots
plot(iris$Sepal.Length, iris$Petal.Width)
plot(iris$Sepal.Length, iris$Petal.Width,
     pch = 2, col = "darkblue", main = "Scatterplot",
     ylab = "Petal Width", xlab = "Sepal Length", ylim = c(0,3), yaxs = "i")

# Histograms:
hist(iris$Sepal.Length)
hist(iris$Sepal.Length, breaks = 20, freq=FALSE)
#Check the help page of plot() and hist() to see the various options (function arguments) that you can add.

# Density overlay above (see Section 3)
dens <- density(iris$Sepal.Length)
lines(dens)

#Lattice graphics:
densityplot(dat_credit$Rating)
histogram(dat_credit$Rating)

# Boxplots:
boxplot(mtcars$mpg, main = "Boxplot of mpg")
boxplot(mtcars$mpg, main = "Boxplot with subtitle", sub = "This is a subtitle", col = "red", log = "y")
boxplot(mtcars$mpg, main = "Horizontal boxplot", horizontal = TRUE)
boxplot(mpg ~ am, data = mtcars)

# Lattice gallery (Credit):
boxplot(dat_credit$Age, main = "Age (base boxplot)")
boxplot(dat_credit$Age ~ dat_credit$Income_bin, main = "Age by Income_bin")
boxplot(dat_credit$Age ~ dat_credit$Income_bin, width = c(0.5, 2))

plot(dat_credit$Ethnicity)
hist(dat_credit$Balance)
hist(dat_credit$Rating, col = "blue")
hist(dat_credit$Rating, breaks = 20)
hist(dat_credit$Rating, nclass = 20)

densityplot(dat_credit$Rating)
histogram(dat_credit$Rating)

# QQ diagnostics:
qqnorm(dat_credit$Balance); qqline(dat_credit$Balance)
qqnorm(dat_credit$Age);     qqline(dat_credit$Age)
qqplot(dat_credit$Balance, dat_credit$Age)  # two-sample QQ plot

#qqplot() makes a two-sample Q–Q plot: it compares the empirical quantiles of two numeric variables.
#So qqplot(dat_credit$Balance, dat_credit$Age) plots the quantiles of Balance (x-axis) against the matching quantiles of Age (y-axis).
#Use qqplot(x, y) to compare two samples; use qqnorm(x) (or qqplot(x, qnorm(ppoints(length(x))))) to compare one sample vs Normal.


# Helpful resources:
# https://intro2r.com/simple-base-r-plots.html
# https://r-charts.com/base-r/line-types/
# https://r-graph-gallery.com/
# ===============================================================
