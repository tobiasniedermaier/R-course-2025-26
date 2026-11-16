
# Common pitfalls or DON’Ts in R programming and data analysis in general:

# 1.	Forgetting to look at the data before doing any analysis (plots, outlier checks, variable coding, data types etc.).
set.seed(1)
dat <- data.frame(age=runif(1000, min=50, max=75), sex=runif(1000, min=0, max=1), bmi=rnorm(1000, mean=22, sd=5))
summary(lm(bmi ~ age + sex, data=dat)) #This gives the null association that we would expect if we create uncorrelated random variables.
#The problem is that - at a first glance - the results look plausible, although they are actually not.

quantile(dat$bmi)
quantile(dat$bmi, probs=seq(0,1,0.01)) #I would say that the bottom 7% of the BMI distribution are quite impossible or at least highly suspicious.

summary(dat) #Actually, this should always be the first check that you do.
#But also, you should never forget to do scatter plots of the data!

plot(dat$age, dat$bmi)
plot(dat$age, dat$sex) #summary(dat) already told you that the mean is 0.49. This is not suspicious because it should simply reflect the proportion of men or women (depending how the variable is coded.)
#But now you can see that something is really wrong with the variable. (Or at least, the variable does not represent what you probably thought it would represent.)
# We actually have values other than 0  and 1 in the variable, which does not make sense.
# For a simulation where we create data, we should have chosen rbinom(1000, 1, 0.5).

set.seed(1)
dat <- data.frame(age=runif(1000, min=50, max=75), sex=rbinom(1000, 1, 0.5), bmi=rnorm(1000, mean=22, sd=5))
summary(lm(bmi ~ age + sex, data=dat))
#For this simulation, we ASSUME that BMI values <14 are measurement errors. In real data, extreme values must be verified before being recoded.

dat[dat$bmi<14,]$bmi <- NA
summary(lm(bmi ~ age + sex, data=dat))
plot(dat$age, dat$sex)
boxplot(dat$age ~ dat$sex) #This looks better!

#======================================================================

# 2.	Ignoring missing data or handling them incorrectly.
set.seed(123)
dat <- data.frame(weight = rnorm(n=1000, mean=75, sd=10), age=rnorm(1000, mean=65, sd=9))
dat_complete <- dat #Keep the original values for comparison in this simulation.
summary(dat)
#Let's introduce some missings:
# plogis() converts its input into a probability between 0 and 1.
# The probability that weight is missing increases with the (later unobserved) weight.
# This deliberately creates an MNAR mechanism: missingness depends on the missing value itself.
#
p_missing_weight <- plogis((dat$weight - 75))
weight_missing <- runif(n=1000) < p_missing_weight
dat$weight[weight_missing] <- NA

p_missing_age <- plogis((dat$age - 65))
age_missing <- runif(n=1000) < p_missing_age
dat$age[age_missing] <- NA

summary(dat)
#Note that around 50% of the weight and age observations are missing. But not necessarily in the same participants!
#The observed mean weight is lower than the actual mean in the simulated complete dataset:
mean(dat_complete$weight)
mean(dat$weight, na.rm=TRUE)

dat$outcome <- rbinom(1000, 1, 0.2)
glm(outcome ~ age, data=dat, family=binomial) #522 missings
glm(outcome ~ weight, data=dat, family=binomial) #511 missings
glm(outcome ~ age + weight, data=dat, family=binomial) #774 missings

#Lesson: In a real data set, you will have missing values. But different variables may be missing in different individuals. The regression will automatically delete all observations where *any* of the included predictors (or the outcome variable, of course) are missing. Implication: Even if you have only relatively few missings per variable, you might lose a lot of observations if you include all those variables (and if the missing values are hardly correlated between the variables).

#Most common solutions:
#1.) Restrict your analysis to the predictors you actually need (easy way).
#2.) Consider suitable additional/auxiliary information if available. Do not replace important predictors with weaker proxies without considering the consequences.
#3.) Multiple imputation is often an appropriate method for handling missing data. 

#Standard multiple imputation methods generally rely on a Missing At Random (MAR) assumption, conditional on variables in the imputation model. The example above is MNAR by construction. MNAR requires additional assumptions/models; sensitivity analyses are important because the missingness mechanism cannot generally be identified from observed data alone.


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
dat[dat$height>250,] #Extremely implausible and very likely a data error. Check the original data if possible; otherwise consider recoding as NA with explicit justification. Document every recoding and keep the original dataset!
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
#Note: With x symmetric around zero, x and x^2 are uncorrelated. Thus the
#coefficient of x may be near zero even though the model is seriously misspecified.
#Compare this with a correctly specified model:
lm2 <- lm(y ~ x + I(x^2))
summary(lm2)
plot(lm2, 1)


#======================================================================

# 5.	Ignoring warnings or even errors in the output.
#(To be added.)

## Data separation:

dat <- data.frame(smo=rep(c(1,0), each=500), sex=rep(c("men","women"), each=500))
separated_model <- glm(smo ~ sex, data=dat, family="binomial")
summary(separated_model)
#Complete separation: smoking status is perfectly determined by sex in this sample.
#The maximum-likelihood estimate of the corresponding logistic coefficient is not finite
#(the coefficient tends to +/- infinity); the finite numbers printed by glm() are unreliable.
#This is a warning sign, not evidence that the association is absent or unknown.

#Errors are "harder to ignore", because an R function will stop at the point where an error occured.


#======================================================================

# 6.	Applying functions to objects of the wrong class.

mean(c("1","5","7","9")) #Returns NA with a warning (not an error).
mean(c(1,5,7,9)) #OK.
mean(as.numeric(c("1","5","7","9"))) #OK.

#Beware: as.numeric() on a factor returns internal level codes, NOT its labels.
as.numeric(factor(c("10", "20", "30"))) #1, 2, 3, NOT 10, 20, 30.
as.numeric(as.character(factor(c("10", "20", "30")))) #10, 20, 30.

plot.ts(mtcars$mpg) #Works, but probably not what you wanted.

mydata <- list(mtcars[1:15,], mtcars[16:32,])
class(mydata)
mydata + 1 #Won't work.

mydata[[1]] + 1 #Works.
mydata[[2]] + 1 #Works.

lapply(mydata, "+", 1)

hist(LETTERS) #Won't work.
plot(LETTERS) #Won't work.

hist(as.numeric(factor(LETTERS))) #Works.
plot(factor(LETTERS)) #Works.
#Think about it: Why does this work? And does it make sense? The numbers from factor(LETTERS) are arbitrary level codes.


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
#Check which packages your script requires and whether they are installed, e.g.:
requireNamespace("psych", quietly=TRUE) #If FALSE: install.packages("psych") (internet required).
#A dated repository snapshot (e.g. Posit Public Package Manager) helps with version
#reproducibility for NEW installations; it does NOT install missing packages automatically
#or change packages already installed. For project-specific version locking, consider renv.
#Example dated repository: options(repos=c(CRAN="https://packagemanager.posit.co/cran/2026-11-10"))


#======================================================================

# 11.	Relying on (too) many niche third-party packages that may become unavailable or incompatible in the future.
#Many older package versions remain available from https://cran.r-project.org/src/contrib/Archive/, but installing archived packages can be difficult (e.g., incompatible dependencies). Avoid unnecessary dependencies. Packages may also MASK functions with the same name from previously loaded packages (they do not overwrite the original function). Load only what you need. If both functions are useful, call them explicitly by package name. Example:

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

## Convenient when you also want further descriptive statistics (but requires an extra package):
#Make sure you have the "psych" package installed: install.packages("psych"); library(psych)
psych::describe(mtcars[,c("cyl", "hp", "wt", "am", "mpg")]) 
psych::describe(mtcars[,c("cyl", "hp", "wt", "am", "mpg")])[,c("mean", "sd")] #If you really only need mean+sd.


#======================================================================

# 18.	Applying advanced concepts that you do not need and maybe do not even understand, just because they are “cool” or recommended by some nerds (e.g. closures / function-generating functions, S4 and R6 classes, active handling of environments, active bindings).

function_generator <- function(x){
	function(y){
		x(y)
		}
}
function_generator(mean)(c(3, 4, 5, 5, 7))

#Looks really cool, right?

#This is a function-generating function (and an example of a closure), NOT currying in the strict sense. Broken down for demonstration:
generated_mean_function <- function_generator(x=mean)
generated_mean_function(c(3, 4, 5, 5, 7))

generated_sd_function <- function_generator(x=sd)
generated_sd_function(c(3, 4, 5, 5, 7))

#It actually is pretty cool.

#But then, why not simply use:
mean(c(3, 4, 5, 5, 7))
sd(c(3, 4, 5, 5, 7))
#?


#======================================================================

# 19.	Failing to check whether the results of your analysis are plausible.

#This is not so much a programming question but rather an epidemiologic question. With your epidemiological thinking and skills, you should be able to figure out if a result makes sense or if it is completely implausible.

#For example, suppose a Cox regression suggests that aspirin users have a higher CVD hazard.
#Possible explanations include:

#- Incorrect event coding: For example, the event indicator was coded as 0=event and 1=no event,
#  but the analysis treats 1 as an event.

#- Confounding: Aspirin users may be older or have more cardiovascular risk factors than non-users;
#  omitting or inadequately controlling for these variables can distort the association.

#- Confounding by indication: People at higher baseline cardiovascular risk are more likely to be
#  prescribed aspirin. They might remain at higher risk even if aspirin has a protective effect.

#- Selection bias / informative censoring: People at higher CVD risk may be disproportionately lost
#  to follow-up, violating an assumption needed for standard survival analysis.

#- Exposure misclassification or non-adherence: Prescribed aspirin use may differ from actual use.
#  A binary yes/no variable also cannot capture different doses or exposure histories.

#- Functional-form misspecification: The dose-response relationship may be nonlinear, so including
#  aspirin dose as a single linear term may be inadequate. Omitted nonlinear terms can bias the
#  estimated coefficient; this is not automatically residual confounding.

#- Residual confounding: For example, socioeconomic status could affect both aspirin use and CVD
#  risk. If SES is omitted, measured too crudely, or inadequately modeled, confounding may remain.

#- Competing risks: If aspirin users have a lower risk of death from other causes, more of them may
#  survive long enough to develop CVD. This can increase the cumulative incidence of CVD even if the
#  cause-specific CVD hazard is unchanged. This is not necessarily bias: interpret the estimand.

#- And many more!


#======================================================================

# 20. Merging datasets without checking the join keys and the resulting observations.

patients <- data.frame(id=1:4, age=c(50, 60, 70, 80))
lab <- data.frame(id=c(1, 2, 2, 4), crp=c(2, 5, 9, 1))

merged_wrong <- merge(patients, lab, by="id") #Default: inner join; matches all rows with the same id.
merged_wrong
nrow(patients)
nrow(merged_wrong) #Both are 4! The row count alone does not reveal the problem.

#But patient 3 has disappeared, and patient 2 appears twice:
setdiff(patients$id, merged_wrong$id)
merged_wrong$id[duplicated(merged_wrong$id)]

#Always check the expected relationship between keys BEFORE merging:
any(duplicated(patients$id)) #FALSE
any(duplicated(lab$id))      #TRUE: two lab rows for patient 2.

#If the intended structure is one row per patient, this merge is wrong.
#Repeated lab measurements might be valid, but then the one-to-many merge must be intentional.
#Do not simply discard duplicate rows or fabricate values to force unique IDs.
#Also check which observations are lost in an inner join and whether row order changes.
#Using all.x=TRUE preserves unmatched patients but DOES NOT solve duplicated keys.


#======================================================================

# 21. Adjusting for inappropriate variables (overadjustment and collider bias).

## Example A: Adjusting for a mediator, although the TOTAL effect is of interest.
#Causal structure: exposure -> mediator -> outcome, plus exposure -> outcome.
set.seed(123)
n <- 3000
exposure <- rbinom(n, 1, 0.5)
mediator <- exposure + rnorm(n)
outcome <- 0.5*exposure + 2*mediator + rnorm(n)
dat <- data.frame(exposure, mediator, outcome)

coef(lm(outcome ~ exposure, data=dat))["exposure"]             #Approx. 2.5: total effect.
coef(lm(outcome ~ exposure + mediator, data=dat))["exposure"]  #Approx. 0.5: direct effect.

#Adjusting for a mediator is not always wrong! It is wrong if your target is the total effect.
#The adjusted model answers a different question, even though both regressions run normally.

## Example B: Conditioning on a collider can CREATE a spurious association.
#Causal structure: exposure -> collider <- U -> outcome.
set.seed(456)
n <- 3000
exposure <- rnorm(n)
u <- rnorm(n) #Unobserved cause of the outcome.
collider <- exposure + u + rnorm(n)
outcome <- u + rnorm(n)
dat <- data.frame(exposure, collider, outcome)

coef(lm(outcome ~ exposure, data=dat))["exposure"]              #Approx. 0: no effect.
coef(lm(outcome ~ exposure + collider, data=dat))["exposure"]   #Spurious association!

#LESSON: Adding more covariates does not automatically improve a regression model.
#Covariate selection should depend on the causal question, not just statistical significance.
