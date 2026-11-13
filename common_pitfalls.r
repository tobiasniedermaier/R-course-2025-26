
# Common pitfalls or DON’Ts in R programming and data analysis in general:

# 1.	Forgetting to look at the data before doing any analysis (plots, outlier checks, variable coding, data types etc.).
#(To be added.)

#======================================================================

# 2.	Ignoring missing data or handling them incorrectly.
set.seed(123)
dat <- data.frame(weight = rnorm(n=1000, mean=75, sd=10), age=rnorm(1000, mean=65, sd=9))
summary(dat)
#Let's introduce some missings:
# plogis() converts its input into a probability between 0 and 1.
# The probability that weight is missing increases with actual weight.
#
p_missing_weight <- plogis((dat$weight - 75))
weight_missing <- runif(n=1000) < p_missing_weight
dat$weight[weight_missing] <- NA

p_missing_age <- plogis((dat$age - 65))
age_missing <- runif(n=1000) < p_missing_age
dat$age[age_missing] <- NA

summary(dat)
#Note that around 50% of the weight and age observations are missing. But not necessarily in the same participants!

dat$outcome <- rbinom(1000, 1, 0.2)
glm(outcome ~ age, data=dat, family=binomial) #522 missings
glm(outcome ~ weight, data=dat, family=binomial) #511 missings
glm(outcome ~ age + weight, data=dat, family=binomial) #774 missings

#Lesson: In a real data set, you will have missing values. But different variables may be missing in different individuals. The regression will automatically delete all observations where *any* of the included predictors (or the outcome variable, of course) are missing. Implication: Even if you have only relatively few missings per variable, you might lose a lot of observations if you include all those variables (and if the missing values are hardly correlated between the variables).

#Most common solutions:
#1.) Restrict your analysis to the predictors you actually need (easy way).
#2.) If possible, use surrogate predictors that have fewer missings (rarely available).
#3.) Multiple imputation is often an appropriate method for handling missing data. 

#Standard multiple imputation methods generally assume that data are Missing At Random (MAR). However, multiple imputation can also be applied to Missing Not At Random (MNAR) data if the missingness mechanism is explicitly accounted for. Since this mechanism is usually unknown, sensitivity analyses are recommended to assess the robustness of the results.


#======================================================================

# 3.	Being negligent in data plausibility checks and data cleaning.
set.seed(1)
height_dat <- rnorm(n=100, mean=170, sd=15)
dat <- data.frame(height = height_dat, weight = height_dat -100 + rnorm(n=100, mean=0, sd=10))
dat[c(2, 15, 24, 37, 47, 51, 75),]$height <- dat[c(2, 15, 24, 37, 47, 51, 75),]$height *30
dat
hist(dat$height)
summary(dat)
quantile(dat$height, probs=seq(from=0, to=1, by=0.01))
dat[dat$height>200,] #Not very likely, but possible. Should be verified and NOT automatically set to NA.
dat[dat$height>250,] #Almost certainly a data error. Only one person (alive) is that tall, and only around a dozen people were ever recorded to have a height >250 cm. Check the original data if possible; otherwise set to NA. Document what values you recode and why. Always keep a copy of the original data set!
dat$height_backup <- dat$height
dat[dat$height>250,]$height <- NA
dat[,c("height", "height_backup")]

summary(lm(weight ~ height, data=dat))
summary(lm(weight ~ height_backup, data=dat))


#======================================================================

# 4.	Applying statistical models without checking relevant model assumptions, especially when violations could seriously affect the results.

x <- -100:100
set.seed(1)
y <- x^2 + rnorm(n = length(x), mean = 0, sd = 500)
plot(x, y, main = "Quadratic relationship with constant error variance")
lm1 <- lm(y ~ x)
summary(lm1)
plot(lm1, 1)
#Compare this with a correctly specified model:
lm2 <- lm(y ~ x + I(x^2))
summary(lm2)
plot(lm2, 1)


#======================================================================

# 5.	Ignoring warnings or even errors in the output.
#(To be added.)


#======================================================================

# 6.	Applying functions to objects of the wrong class.
#(To be added.)


#======================================================================

# 7.	Overwriting existing variables or other objects by accident.

lm1 <- lm(mpg ~ hp, data=mtcars)
lm2 <- lm(mpg ~ hp + wt, data=mtcars)
AIC(lm1, lm2)
library(lmtest)
lrtest(lm1, lm2)

lm1 <- lm(mpg ~ hp + wt + cyl, data=mtcars) #oops
AIC(lm1, lm2)
lrtest(lm1, lm2) #The likelihood ratio test still runs without an error or warning, but it now compares different models than originally intended. This is particularly dangerous because the output looks perfectly valid!


#======================================================================

# 8.	Using AI-generated (or elsewhere published) code that you do not understand.
#(Should be evident.)


#======================================================================

# 9.	Relying on AI for code generation and debugging instead of learning how to write code and troubleshoot problems independently.
#(Should be evident.)


#======================================================================

# 10.	Assuming that everyone working with your script has the same packages installed.
#Solution: Posit Public Package Manager (P3M). Formerly known as RStudio Package Manager, this free service hosts daily CRAN snapshots just like MRAN (the now defunct Microsoft R Open Network) did. You can freeze your package versions by setting a dated repository URL in R (e.g., options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2026-11-10"))).


#======================================================================

# 11.	Relying on (too) many niche third-party packages that may become unavailable or incompatible in the future.
#Availability is not the main problem because R packages are archived at https://cran.r-project.org/src/contrib/Archive/ even if the package was removed from CRAN. However, it requires extra knowledge and effort to get a script to run that requires an obscure package. Also, packages may be conflicting with other functions in R (from Base R or other packages), so keep them to a minimum. (I.e., don't simply load all packages you know at the beginning of your script. Load only the packages you need.) If a package overwrites a function from a different package, you will receive a note ("The following object(s) was masked by 'package:xyz':"). If you really need functions from different packages that use exactly the same name, you can assign them to different objects. Example:

#bad practise:
library(Hmisc)
describe(mtcars)
library(psych)
describe(mtcars)

#much better:
descr1 <- Hmisc::describe
descr2 <- psych::describe

descr1(mtcars)
descr2(mtcars)

#If you only need a single function (or two) from a specific package, better use packagename::functionname() instead of loading the package with library() so that you don't risk masking an object (a function) from a previously loaded package.


#======================================================================

# 12.	Over-optimizing code for negligible improvements in speed or code length.
#AND: 13.	Not optimizing code that unnecessarily takes forever to run.

n <- 100000

#bad:
vec1 <- c()
vec1[1] <- 1
for(i in 1:n){
	vec1[i+1] <- vec1[i] + 1
}
vec1

#much better:
vec2 <- numeric(length = n+1)
vec2[1] <- 1
for(i in 1:n){
	vec2[i+1] <- vec2[i] + 1
}
vec2

#even dramatically better in this simple example:
vec3 <- 1:(n+1)

#or more explicit:
vec4 <- as.numeric(seq_len(n+1))

identical(vec1, vec2) #TRUE
identical(vec2, vec3) #FALSE: numeric (double) vs. integer
identical(vec2, vec4) #TRUE


#It really depends on the size of your data set and the complexity of the computations if it is worth to use vectorization or pre-allocation of vectors and/or parallelization.

#Further ways to speed up R code:
#- Take statements that check for conditions (if statements) outside of loops (if possible).
#- In general: Put everything outside of a loop that does NOT change during the iterations.
#- Use ifelse() or logical indexing (subsetting) instead of loops, if possible.
#- Use parallel processing (see example below at point 14.)
#- Use data structures that consume lesser memory and/or are optimized for speed: library(data.table); dt <- data.table(df)


#======================================================================

# 14.	Hard-coding data or other parameters that might change across datasets or computing environments (e.g., absolute file paths, number of available CPU cores in parallelization, intermediate results).

##BAD:
dat <- read.csv2("C:/Users/your.name/Data/yourfile.csv")
number_cpus <- 4

#BETTER:
dat <- read.csv2("C:/Users/your.name/Data/yourfile.csv")
number_cpus <- parallel::detectCores()

#MUCH BETTER:
filepath <- "C:/Users/your.name/Data/yourfile.csv" #Define this at the top of your script. Highlight it with comments to make sure that everyone sees it instantly.
dat <- read.csv2(filepath)
number_cpus <- ceiling(parallel::detectCores()^0.6) #Think about it why this might make some sense by checking how many cores are used if 1, 2, 3, 4, ... CPU cores are available. This is an illustrative heuristic, not a universally optimal rule.
number_cpus <- max(1, parallel::detectCores()-1) #Easier, but typically suboptimal. Good: Always leaves one core unused (unless only one core is available, which should be extremely rare nowadays). Suboptimal: If you have a modern CPU with maybe 12 physical cores and 24 logical cores (hyperthreading), using almost all cores often will not speed up the computations more than using "only" 8 cores. This is because parallelization also creates overhead: R needs to "think" which calculations it sends to which core and how to put the results back together.

#How to parallelize computations using future.apply:
# Example: Parallel processing with future_lapply()
# We want to perform 6,000 bootstrap regressions.
# Each bootstrap sample is independent of the others,
# making this a suitable task for parallel processing.

library(future.apply)

# Determine the number of workers dynamically:
number_cpus <- min(4, ceiling(parallelly::availableCores()^0.6))

# Function performing 500 bootstrap regressions:
bootstrap <- function(i) {
  replicate(500, {
    rows <- sample.int(nrow(mtcars), replace=TRUE)
    model <- lm(mpg ~ hp + wt, data=mtcars[rows, ])
    coef(model)["hp"]
  })
}

# 1. Sequential processing:
plan(sequential)

time1 <- system.time({
  result1 <- future_lapply(1:12, bootstrap, future.seed=123)
})

# 2. Parallel processing:
plan(multisession, workers=number_cpus)

time2 <- system.time({
  result2 <- future_lapply(1:12, bootstrap, future.seed=123)
})

# Reset to sequential processing:
plan(sequential)

# Compare execution times:
time1
time2

# Check whether the results are identical:
identical(result1, result2) # TRUE

# Speedup factor (elapsed time):
time1["elapsed"] / time2["elapsed"]

# LESSON:
# Parallel processing can speed up independent,
# computationally expensive tasks such as bootstrap
# analyses or simulations.
# However, parallelization introduces overhead and
# is therefore not always faster.
# With appropriate random number generation, results
# remain reproducible regardless of the number of workers.

#Run the boostrap example with 1, 2, and 4 cores (and 8, if availabe) and compare the runtime,


#======================================================================

# 15.	Not commenting code or commenting in a suboptimal way (not explaining what the code does and why it is done – unless it is obvious.)
#-> Lecture slides by Gaston Sanchez, which you can find on my Github repo:
#https://github.com/tobiasniedermaier/R-course-2025-26/blob/main/other_stuff/good-habits-in-r-programming.pdf
#https://github.com/tniedermaier/R-course-2025-26/blob/main/other_stuff/good-habits-in-r-programming.pdf


#======================================================================

# 16.	Producing unstructured "spaghetti code".
#- Break down your code into logical blocks, each of which has a specific purpose. (E.g., 1. data cleaning, 2. variable coding, 3. statistical analysis (descriptive), 4. plots, 5. statistical analysis (inductive), 6. sensitivity analyses.


#======================================================================

# 17.	Copy-pasting long sequences of code just to repeat an analysis for a different data set or a different variable in the data set. (Use vectorization [where appropriate], for-loops or functions instead.)

## Bad:
mean(mtcars$cyl)
mean(mtcars$hp)
mean(mtcars$wt)
mean(mtcars$am)
mean(mtcars$mpg)

sd(mtcars$cyl)
sd(mtcars$hp)
sd(mtcars$wt)
sd(mtcars$am)
sd(mtcars$mpg)

## Better:
for(myvars in c("cyl", "hp", "wt", "am", "mpg")){
 print(data.frame(myvars, mean=mean(mtcars[,myvars]), sd=sd(mtcars[,myvars])))
}

## Nerd style:
myvars <- c("cyl", "hp", "wt", "am", "mpg")

i <- 1
while(i <= length(myvars)){
  print(data.frame(
    myvars = myvars[i],
    mean = mean(mtcars[, myvars[i]]),
    sd = sd(mtcars[, myvars[i]])
  ))
  i <- i + 1
}

## Much better:
sapply(mtcars[,c("cyl", "hp", "wt", "am", "mpg")], function(x){data.frame(mean(x), sd(x))}) #or:
t(sapply(mtcars[, myvars], function(x) {c(mean = mean(x), sd = sd(x))}))
#Or:
list_output <- lapply(mtcars[,c("cyl", "hp", "wt", "am", "mpg")], function(x){data.frame(mean(x), sd(x))})
do.call("rbind", list_output)

## Probably the best solution:
#Make sure you have the "psych" package installed: install.packages("psych"); library(psych)
psych::describe(mtcars[,c("cyl", "hp", "wt", "am", "mpg")]) 
psych::describe(mtcars[,c("cyl", "hp", "wt", "am", "mpg")])[,c("mean", "sd")] #If you really only need mean+sd.


#======================================================================

# 18.	Applying advanced concepts that you do not need and maybe do not even understand, just because they are “cool” or recommended by some nerds (e.g. closures / function-generating functions / currying, S4 and R6 classes, active handling of environments, active bindings).

curried_function <- function(x){
	function(y){
		x(y)
		}
}
curried_function(mean)(c(3, 4, 5, 5, 7))

#Looks really cool, right?

#A "curried" function is basically a function-generating function. Broken down for demonstration:
generated_mean_function <- curried_function(x=mean)
generated_mean_function(c(3, 4, 5, 5, 7))

generated_sd_function <- curried_function(x=sd)
generated_sd_function(c(3, 4, 5, 5, 7))

#It actually is pretty cool.

#But then, why not simply use:
mean(c(3, 4, 5, 5, 7))
sd(c(3, 4, 5, 5, 7))
#?


#======================================================================

# 19.	Failing to check whether the results of your analysis are plausible.
#(To be added.)
