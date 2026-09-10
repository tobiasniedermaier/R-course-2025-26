# ===============================================================
# R WORKSHOP: Essentials (Didactic Master)
# ===============================================================
#
# Guiding principle of this version:
# - concepts are introduced before they are used in less transparent ways
# - built-in/source data sets are not modified directly
# - examples that modify data work on explicit copies
# - compact R shortcuts are shown only after the transparent version
# - examples that write files use tempdir() so they do not clutter the
#   current working directory unless you deliberately adapt them
#
# Topics covered
# - Packages and data; quick data inspection
# - Vector and data-frame indexing; logical subsetting; missing values
# - Type checks
# - Data cleaning and safe use of copies
# - Creating dummy and multi-category variables
# - Descriptive summaries and distributions
# - t-tests and cross-tabulations
# - Logistic regression and nested model comparison
# - Defining functions and package namespaces
# - Mean, median, statistical mode; grouped summaries; Chi-squared; ANOVA
# - Base R and lattice graphics; QQ, time-series and function plots
# - Working directory and file I/O
# - Arrays / higher-dimensional objects
#
# Optional add-on packages used below: psych, ISLR, lattice
# Install them ONCE if needed:
# install.packages(c("psych", "ISLR", "lattice"), dependencies = TRUE)
# ===============================================================


## ---------------------------------------------------------------
## 0) Packages, data and first inspection
## ---------------------------------------------------------------

# R already contains several example data sets, e.g. mtcars and iris.
mtcars
head(mtcars)            # first 6 observations
head(mtcars, 10)        # first 10 observations
tail(mtcars)            # last 6 observations
summary(mtcars)         # quick descriptive overview
str(mtcars)             # structure: dimensions, variable names and types
names(mtcars)            # variable names
nrow(mtcars)             # number of rows
ncol(mtcars)             # number of columns
dim(mtcars)              # rows and columns together

# Assignment creates another object. For a data frame, this allows us to modify
# the new object later without deliberately changing the source object mtcars.
dat_mtcars <- mtcars

# Add-on packages provide further functions and data sets.
# library(package) attaches a package for the current R session.
# package::object accesses one function/data object explicitly without attaching
# the whole package. This notation is especially clear in teaching scripts.
dat_auto   <- ISLR::Auto
dat_credit <- ISLR::Credit

head(dat_auto)
head(dat_credit)
names(dat_credit)


## ---------------------------------------------------------------
## 1) Basic indexing / subsetting
## ---------------------------------------------------------------

# --- 1a) Subsetting a vector ---
a_vec <- c(4, 6, 8, 5, 2, 6, 7, 89, 65, 4,
           3, 6, 8, 9, 5, 3, 35, 83, 75, 6)
a_vec
length(a_vec)                 # number of elements

# Positive indices select positions:
a_vec[1]                      # first element
a_vec[2:5]                    # elements 2 to 5

# Negative indices exclude positions:
a_vec[-1]                     # all except the first element
a_vec[-c(3, 6, 8)]            # exclude elements 3, 6 and 8

# Comparisons produce logical vectors (TRUE/FALSE):
a_vec == 65
a_vec > 60

# A logical vector can itself be used as an index:
a_vec[a_vec == 65]            # values equal to 65
a_vec[a_vec > 60]             # values > 60
a_vec[a_vec > 60 | a_vec < 8] # | means OR
a_vec[a_vec > 60 & a_vec < 80]# & means AND

# Create and reuse an index vector:
selection <- seq(from = 1, to = length(a_vec), by = 2)
selection
a_vec[selection]              # every second element, starting with the first

# Equivalent ways of writing seq(); named arguments are clearest for beginners:
seq(from = 1, to = length(a_vec), by = 2)
seq(1, length(a_vec), 2)
seq(to = length(a_vec), from = 1, by = 2)

# --- 1b) Missing values ---
# NA means a missing / unavailable value.
a_with_na <- a_vec
a_with_na[c(3, 8)] <- NA
a_with_na

is.na(a_with_na)              # TRUE where values are missing
which(is.na(a_with_na))       # positions of the missing values
!is.na(a_with_na)             # ! means NOT
a_with_na[!is.na(a_with_na)]  # keep only non-missing values
sum(is.na(a_with_na))         # number of missing values

# --- 1c) Subsetting a data frame ---
# In data_frame[rows, columns], the comma separates rows from columns.
dat_mtcars[, 3]               # third column; simplified to a vector
dat_mtcars[, "disp"]          # the same column selected by name
dat_mtcars[1:5, ]             # first five rows, all columns

# Keep the result as a one-column data frame instead of simplifying to a vector:
dat_mtcars[, 3, drop = FALSE]

# Select rows 2 and 4, but exclude columns 1 to 3:
mysubset <- dat_mtcars[c(2, 4), -c(1:3)]
mysubset
summary(mysubset)

# $ is a convenient way to select one named column:
dat_mtcars$mpg


## ---------------------------------------------------------------
## 2) Variable types
## ---------------------------------------------------------------

dat_iris <- iris

# Single variables:
is.numeric(dat_iris$Sepal.Length)
is.character(dat_iris$Sepal.Length)
is.numeric(dat_iris$Species)
is.character(dat_iris$Species)
is.factor(dat_iris$Species)

# sapply(data_frame, function) applies the named function to every column and
# usually simplifies the results to a vector:
sapply(dat_iris, is.numeric)
sapply(dat_iris, is.character)
sapply(dat_iris, is.factor)
sapply(dat_auto, is.numeric)


## ---------------------------------------------------------------
## 3) Data cleaning -- use copies so examples stay independent
## ---------------------------------------------------------------

# --- 3a) Missing values ---
dat_air <- airquality
head(dat_air)
summary(dat_air)

# is.na(dat_air) creates a TRUE/FALSE matrix. colSums() counts TRUE values in
# each column because TRUE is treated as 1 and FALSE as 0.
colSums(is.na(dat_air))

# Mean with/without missing values:
mean(dat_air$Ozone)                   # NA because Ozone contains missing values
mean(dat_air$Ozone, na.rm = TRUE)     # ignore NAs

# --- 3b) Dichotomize a variable ---
# Work on a copy rather than modifying airquality itself.
dat_air_clean <- dat_air
dat_air_clean$Ozone_dicho <- ifelse(dat_air_clean$Ozone > 40,
                                    "high", "low")
table(dat_air_clean$Ozone_dicho, exclude = NULL)

# --- 3c) Winsorize outliers ---
# psych::winsor() explicitly identifies the package that supplies winsor().
dat_air_clean$Ozone_w05 <- psych::winsor(dat_air_clean$Ozone, trim = 0.05)

# --- 3d) Apply one summary function to every column ---
# lapply() returns a list; sapply() usually simplifies the result.
lapply(mtcars, sd)
sapply(mtcars, sd)

# Vectorize(sd) is intentionally NOT used here. sapply() already expresses the
# intended operation directly: apply sd() separately to every column.

# --- 3e) Detect and change implausible values ---
# Again, modify a copy; dat_credit itself remains unchanged.
dat_credit_clean <- dat_credit

# Example rule: suppose more than 5 cards is considered implausible.
table(dat_credit_clean$Cards)
dat_credit_clean$Cards[dat_credit_clean$Cards > 5] <- NA
table(dat_credit_clean$Cards, exclude = NULL)

# Example rule involving two conditions:
sum(dat_credit_clean$Income < 100 & dat_credit_clean$Limit > 6000)
dat_credit_clean$Limit[
  dat_credit_clean$Income < 100 & dat_credit_clean$Limit > 6000
] <- 6000
sum(dat_credit_clean$Income < 100 & dat_credit_clean$Limit > 6000)

# --- 3f) Column-name utilities ---
# Show transformations without changing the main dat_credit object.
tolower(names(dat_credit))
toupper(names(dat_credit))

# Example: capitalize first letter and lower-case the rest.
dat_names <- dat_credit
new_names <- paste0(
  toupper(substring(names(dat_names), first = 1, last = 1)),
  tolower(substring(names(dat_names), first = 2))
)
names(dat_names) <- new_names
names(dat_names)

# --- 3g) String manipulation ---
# gsub(pattern, replacement, x) replaces matching text in a character vector.
married_lower <- gsub(pattern = "Y", replacement = "y", x = dat_credit$Married)
married_lower <- gsub(pattern = "N", replacement = "n", x = married_lower)
head(married_lower)


## ---------------------------------------------------------------
## 4) Create new variables
## ---------------------------------------------------------------

# --- 4a) Derive variables from existing variables ---
dat_w <- women
dat_w$height_m  <- dat_w$height * 2.54 / 100
dat_w$weight_kg <- dat_w$weight * 0.4535924
dat_w$bmi       <- dat_w$weight_kg / dat_w$height_m^2
head(dat_w)

# --- 4b) Binary variable based on a median ---
dat_credit_vars <- dat_credit
median_income <- median(dat_credit_vars$Income, na.rm = TRUE)
dat_credit_vars$Income_bin <- ifelse(dat_credit_vars$Income > median_income,
                                     "high", "low")
dat_credit_vars$Income_factor <- factor(dat_credit_vars$Income_bin)
table(dat_credit_vars$Income_factor)

# --- 4c) Variables based on several conditions ---
# Work on a copy rather than adding columns directly to mtcars.
dat_mtcars_vars <- mtcars

dat_mtcars_vars$newvar <- ifelse(
  dat_mtcars_vars$mpg > 20 &
    dat_mtcars_vars$hp > 80 &
    dat_mtcars_vars$cyl == 6,
  1, 0
)

dat_mtcars_vars$newvar2 <- ifelse(
  dat_mtcars_vars$mpg > 20 &
    (dat_mtcars_vars$hp > 80 | dat_mtcars_vars$cyl == 6),
  1, 0
)

table(dat_mtcars_vars$newvar)
table(dat_mtcars_vars$newvar2)

# --- 4d) Stepwise dummy creation by indexing ---
dat_credit_vars$married_man <- NA_integer_
dat_credit_vars$married_man[
  dat_credit_vars$Gender == "Male" & dat_credit_vars$Married == "Yes"
] <- 1
dat_credit_vars$married_man[is.na(dat_credit_vars$married_man)] <- 0

dat_credit_vars$married_woman <- NA_integer_
dat_credit_vars$married_woman[
  dat_credit_vars$Gender == "Female" & dat_credit_vars$Married == "Yes"
] <- 1
dat_credit_vars$married_woman[is.na(dat_credit_vars$married_woman)] <- 0

dat_credit_vars$not_married <- ifelse(dat_credit_vars$Married == "No", 1, 0)

# The same idea can be written directly with ifelse():
dat_credit_vars$rich_caucasian_man <- ifelse(
  dat_credit_vars$Gender == "Male" &
    dat_credit_vars$Ethnicity == "Caucasian" &
    dat_credit_vars$Income_factor == "high",
  1, 0
)

# --- 4e) Add an initially empty variable and fill it ---
dat2 <- mtcars
dat2$transmission <- NA_character_

# In mtcars, am = 0 means automatic and am = 1 means manual.
dat2$transmission[dat2$am == 1] <- "manual"
dat2$transmission[dat2$am == 0] <- "automatic"

# Alternative using ifelse():
dat2$transmission2 <- ifelse(dat2$am == 1, "manual", "automatic")
identical(dat2$transmission, dat2$transmission2)

# Sorting:
sort(dat2$mpg)                 # ascending
rev(sort(dat2$mpg))            # descending

# --- 4f) Multi-category variable and an intentionally missed boundary ---
dat2$fuel_consumption <- NA_character_
dat2$fuel_consumption[dat2$mpg > 21] <- "low"
dat2$fuel_consumption[dat2$mpg <= 21 & dat2$mpg > 15] <- "medium"
dat2$fuel_consumption[dat2$mpg < 15] <- "high"

# mpg == 15 was accidentally omitted. It therefore remains NA.
table(dat2$fuel_consumption, exclude = NULL)
sum(is.na(dat2$fuel_consumption))
dat2$mpg[is.na(dat2$fuel_consumption)]

# Fix the boundary condition:
dat2$fuel_consumption[dat2$mpg <= 15] <- "high"
sum(is.na(dat2$fuel_consumption))

# Logical conditions can be counted directly:
dat2$mpg > 21
sum(dat2$mpg > 21)
sum(dat2$mpg <= 21)
sum(!(dat2$mpg > 21))          # equivalent to mpg <= 21 here


## ---------------------------------------------------------------
## 5) Descriptive summaries and distributions
## ---------------------------------------------------------------

summary(dat_credit)
mean(dat_credit$Income)
median(dat_credit$Income)

# Reproducible random examples:
set.seed(1)
unifdist <- runif(n = 10000, min = 10, max = 30)
normdist <- rnorm(n = 1000, mean = 5, sd = 3)

summary(unifdist)
summary(normdist)

# Histograms and density plots are shown in more detail in Section 11.


## ---------------------------------------------------------------
## 6) t-tests
## ---------------------------------------------------------------

# Formula notation: response ~ grouping_variable
# Compare mean mpg between automatic (am = 0) and manual (am = 1) cars.
t.test(mpg ~ am, data = mtcars)

# The same comparison written as two explicit numeric vectors:
t.test(
  mtcars$mpg[mtcars$am == 0],
  mtcars$mpg[mtcars$am == 1]
)

# Another example: compare mean Income by Gender in Credit.
t.test(Income ~ Gender, data = dat_credit)


## ---------------------------------------------------------------
## 7) Cross-tabulation
## ---------------------------------------------------------------

table(mtcars$cyl)
table(mtcars$cyl, mtcars$am)
table(mtcars[, c("cyl", "am")])
table(mtcars$cyl, mtcars$am, mtcars$vs)

# With derived Credit variables:
table(dat_credit_vars$Income_factor, dat_credit_vars$Gender)
table(dat_credit_vars$Income_factor, dat_credit_vars$Gender, exclude = NULL)
table(dat_credit_vars[, c("Income_factor", "Gender", "Married")])


## ---------------------------------------------------------------
## 8) Logistic regression
## ---------------------------------------------------------------

# --- 8a) airquality: high ozone (1) vs predictors ---
# Start from the original data set again so this example does not depend on
# anything that happened in the data-cleaning section.
dat_aq <- airquality
dat_aq$Ozone_dicho <- ifelse(dat_aq$Ozone > 40, 1, 0)

# family = binomial specifies logistic rather than linear regression.
mylogreg <- glm(
  Ozone_dicho ~ Temp + Wind,
  data = dat_aq,
  family = binomial
)

mylogreg
summary(mylogreg)
coef(mylogreg)                  # coefficients on the log-odds scale
confint(mylogreg)               # confidence intervals on log-odds scale
exp(coef(mylogreg))             # odds ratios
exp(confint(mylogreg))          # confidence intervals for odds ratios

# --- 8b) Nested logistic models using the SAME observations ---
# A model comparison should not be driven by different rows being removed.
# Therefore determine complete cases using every variable needed by EITHER model.
model_vars <- c("Married", "Income", "Education", "Cards")
keep <- complete.cases(dat_credit[, model_vars])
datc <- dat_credit[keep, ]

logreg_small <- glm(
  Married ~ Income + Education,
  data = datc,
  family = binomial
)

logreg_large <- glm(
  Married ~ Income + Education + Cards,
  data = datc,
  family = binomial
)

summary(logreg_small)
exp(coef(logreg_small))
exp(confint(logreg_small))

# Likelihood-ratio comparison of nested models:
anova(logreg_small, logreg_large, test = "Chisq")

# "Nested" means that the smaller model contains a subset of the predictors in
# the larger model, with the same response and the same observations.
# For non-nested model comparison, information criteria such as AIC/BIC can be used:
AIC(logreg_small, logreg_large)
BIC(logreg_small, logreg_large)


## ---------------------------------------------------------------
## 9) Defining functions and package namespaces
## ---------------------------------------------------------------

# A function has a name, one or more arguments, and a body.
# Curly braces are optional for a single expression, but using them consistently
# is clearer for beginners.
square <- function(x) {
  x^2
}

# The function works on one value or on a numeric vector because ^ is vectorized:
square(4)
square(4:6)
numbers <- 4:6
square(numbers)

# Quotation marks produce character text, so this would fail:
# square("numbers")

# package::function identifies exactly where an add-on function comes from.
psych::describe(mtcars)

# Hmisc also contains a function named describe(). If Hmisc is installed:
# Hmisc::describe(mtcars)


## ---------------------------------------------------------------
## 10) Mean, median, statistical mode; grouped summaries; tests
## ---------------------------------------------------------------

# --- 10a) Mean and median ---
dat_mtc <- mtcars
mean(dat_mtc$mpg)
median(dat_mtc$mpg)

# Create one missing value on a COPY of mtcars:
dat_mtc$disp[5] <- NA
mean(dat_mtc$disp)                 # NA
mean(dat_mtc$disp, na.rm = TRUE)   # ignore NA

# Means for all columns:
sapply(dat_mtc, mean)
sapply(dat_mtc, mean, na.rm = TRUE)

# --- 10b) Statistical mode ---
# Important: base R's mode() function is about an object's storage mode; it is
# NOT a function for calculating the statistical mode (most frequent value).

# Version 1: return one mode. If there is a tie, which.max() returns the first.
Mode_first <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Version 2: return all modes in case of a tie.
find_modes <- function(x) {
  ux <- unique(x)
  counts <- tabulate(match(x, ux))
  ux[counts == max(counts)]
}

vec <- c(-5, 3, 6, 3, 7)
Mode_first(vec)
find_modes(vec)
median(vec)

# Tie example:
vec_tie <- c(3, 6, 3, 6, 7)
Mode_first(vec_tie)   # only the first tied mode
find_modes(vec_tie)   # both 3 and 6

# Step-by-step decomposition using the small tie example:
x <- vec_tie
ux <- unique(x)
ux

matched_positions <- match(x, ux)
matched_positions
# match(x, ux) replaces every value in x by the position at which that value
# appears in ux.

counts <- tabulate(matched_positions)
counts
# tabulate() counts how often the integers 1, 2, 3, ... occur.

max(counts)
counts == max(counts)
ux[counts == max(counts)]
# The last line selects the value(s) whose count equals the largest count.

# --- 10c) Grouped summaries ---
tapply(dat_credit$Income, dat_credit$Gender, FUN = median)
aggregate(Income ~ Gender, data = dat_credit, FUN = median)
aggregate(Income ~ Gender + Married, data = dat_credit, FUN = median)
tapply(dat_credit$Income,
       list(dat_credit$Gender, dat_credit$Married),
       FUN = median)

# --- 10d) Chi-squared and Fisher's exact test ---
mytab1 <- table(mtcars$cyl, mtcars$am)
mytab1
chisq.test(mytab1)
# With small expected cell counts, chisq.test() may warn that the approximation
# is inaccurate. Fisher's exact test is an alternative for small tables:
fisher.test(mytab1)

mytab2 <- table(dat_credit_vars$Income_factor, dat_credit_vars$Married)
chisq.test(mytab2)

# --- 10e) Nested linear models / ANOVA model comparison ---
lm_small <- lm(mpg ~ wt + cyl, data = mtcars)
lm_large <- lm(mpg ~ wt + cyl + hp, data = mtcars)
anova(lm_small, lm_large)


## ---------------------------------------------------------------
## 11) Visualization (base R + lattice)
## ---------------------------------------------------------------

# --- 11a) Scatterplots ---
plot(dat_iris$Sepal.Length, dat_iris$Petal.Width)
plot(
  dat_iris$Sepal.Length,
  dat_iris$Petal.Width,
  pch = 2,
  col = "darkblue",
  main = "Scatterplot",
  ylab = "Petal Width",
  xlab = "Sepal Length",
  ylim = c(0, 3),
  yaxs = "i"
)

# Pairwise scatterplots of all numeric iris variables:
pairs(dat_iris[, 1:4], main = "Pairwise scatterplots")
# plot(iris) is a shorter generic alternative, but pairs() makes the intention
# more explicit for a beginner.

# --- 11b) Histograms and density ---
hist(dat_iris$Sepal.Length)
hist(dat_iris$Sepal.Length, breaks = 20)

hist(normdist, breaks = 20, freq = FALSE,
     main = "Normal sample with density estimate")
dens <- density(normdist)
lines(dens)
# freq = FALSE puts density rather than counts on the y-axis so that density()
# can be overlaid on the histogram.

# --- 11c) Barplots and boxplots ---
my_factor <- factor(mtcars$cyl)
barplot(table(my_factor), main = "Barplot of cylinder categories")

set.seed(1)
random_y <- rnorm(nrow(mtcars))
boxplot(
  random_y ~ my_factor,
  main = "Boxplot of a numeric variable by cylinder category",
  xlab = "Number of cylinders",
  ylab = "Random values"
)

# Generic plot() can choose methods automatically based on the object class.
# These shortcuts work, but are less transparent than the explicit forms above:
# plot(my_factor)
# plot(my_factor, random_y)

boxplot(mtcars$mpg, main = "Boxplot of mpg")
boxplot(
  mtcars$mpg,
  main = "Boxplot with subtitle",
  sub = "This is a subtitle",
  col = "red",
  log = "y",
  ylim = c(1, 50)
)
boxplot(mtcars$mpg, main = "Horizontal boxplot", horizontal = TRUE)
boxplot(
  mpg ~ am,
  data = mtcars,
  main = "mpg by transmission",
  xlab = "am (0 = automatic, 1 = manual)",
  ylab = "mpg"
)

# --- 11d) Credit graphics: base R and lattice ---
boxplot(dat_credit$Age, main = "Age (base boxplot)")
boxplot(dat_credit$Age ~ dat_credit_vars$Income_bin,
        main = "Age by Income_bin")
boxplot(dat_credit$Age ~ dat_credit_vars$Income_bin,
        width = c(0.5, 2))

barplot(table(dat_credit$Ethnicity), main = "Ethnicity")
hist(dat_credit$Balance)
hist(dat_credit$Rating, col = "blue")
hist(dat_credit$Rating, breaks = 20)
hist(dat_credit$Rating, nclass = 20)

lattice::densityplot(~ Rating, data = dat_credit)
lattice::histogram(~ Rating, data = dat_credit)
lattice::bwplot(~ Income, data = dat_credit)
lattice::xyplot(Limit ~ Married, data = dat_credit)

# --- 11e) QQ diagnostics ---
qqnorm(dat_credit$Balance)
qqline(dat_credit$Balance)
qqnorm(dat_credit$Age)
qqline(dat_credit$Age)
qqplot(dat_credit$Balance, dat_credit$Age)
# qqnorm(x): sample quantiles vs theoretical Normal quantiles.
# qqplot(x, y): empirical quantiles of one numeric variable vs another.

# --- 11f) Time-series and date-based plots ---
set.seed(1)
my_ts <- ts(rnorm(120), start = c(2017, 1), frequency = 12)
plot(my_ts, main = "Time series")
# Because my_ts has class "ts", plot() uses a time-series-specific method.

my_dates <- seq(as.Date("2005-01-01"), by = "month", length.out = 50)
plot(my_dates, rnorm(50),
     main = "Date-based plot",
     xlab = "Date",
     ylab = "Random value")
# Because x has class "Date", R formats the x-axis as dates.

# --- 11g) Plotting a mathematical function ---
# Beginner-friendly explicit version: create x values, calculate y values,
# then plot one against the other.
x_values <- seq(from = -5, to = 5, by = 0.1)
y_values <- square(x_values)

plot(
  x_values,
  y_values,
  main = "square(x) = x^2",
  xlab = "x",
  ylab = "square(x)",
  type = "l"
)
abline(h = 10)
abline(v = 0, lty = 2)

# Shortcut after the transparent version:
# plot() is generic. Because its first argument is a function, R dispatches to
# plot.function(), which creates evaluation points internally.
plot(square, from = 0, to = 10, n = 101,
     main = "Function plotted via plot.function()")
# n = 101 means that the function is evaluated at 101 x values and the points
# are connected. The curve therefore LOOKS continuous; it is not evaluated at
# infinitely many points.

# The shorthand plot(square, 0, 10) also works, but positional matching hides
# what 0 and 10 mean. Named from= and to= arguments are much clearer.

# Discrete evaluation points instead of a connected line:
plot(square, from = 0, to = 10, n = 11, type = "p",
     main = "Function evaluated at 11 points")

# Helpful plotting resources:
# https://intro2r.com/simple-base-r-plots.html
# https://r-charts.com/base-r/line-types/
# https://r-graph-gallery.com/


## ---------------------------------------------------------------
## 12) Working directory and file I/O
## ---------------------------------------------------------------

getwd()                       # current working directory
# setwd("...")               # change it deliberately if needed

# IMPORTANT: write.csv() and save() CREATE files. To keep this teaching script
# from cluttering your working directory, the examples below use tempdir().
# Replace tempdir() with getwd() or your own path when you actually want to keep
# the files.

demo_csv <- file.path(tempdir(), "example_data.csv")
write.csv(mtcars, file = demo_csv, row.names = FALSE)
d_loaded <- read.csv(demo_csv)
head(d_loaded)

# save() stores R objects; load() loads the stored object(s) into the workspace.
a_list <- list(answer = 42, data = iris, comment = "whatever")
demo_rdata <- file.path(tempdir(), "example_object.RData")
save(a_list, file = demo_rdata)

rm(a_list)                    # remove it so the effect of load() is visible
exists("a_list")
load(demo_rdata)
exists("a_list")
a_list$answer

# Be careful with arbitrary .RData files: load() may create several objects and
# can overwrite objects with the same names in your current workspace.


## ---------------------------------------------------------------
## 13) Arrays / higher-dimensional objects
## ---------------------------------------------------------------

# Arrays generalize vector/matrix indexing to more than two dimensions.
a_array <- array(1:12, dim = c(3, 2, 2))
a_array
dim(a_array)

# Three-dimensional indexing: [row, column, layer]
a_array[, , 1]                # first layer
a_array[1, , ]                # first row across columns and layers

# This topic is deliberately placed near the end: vector and data-frame
# subsetting from Section 1 should be understood first.
