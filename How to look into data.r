
###########################
# "How to look into data" #
###########################

dat <- survival::pbc

# 1. GET TO KNOW THE DATASET

# How large is the dataset?

nrow(dat) #Number of rows
ncol(dat) #Number of columns
dim(dat) #Rows and columns (dimensions)

# Inspect the first and last observations
head(dat) #first 6 observations
head(dat, 10) #first 10 observations
tail(dat) #last 6 observations
View(dat) #take a look at the whole data set

# 2. CHECK VARIABLE TYPES

str(dat)
class(dat) #object class
class(dat$time) #object class
class(dat$age) #object class
sapply(dat, class) #classes for all variables in the data set. "Apply the function class() to all variables in dat."

# 3. INVESTIGATE MISSING VALUES; SUMMARY STATISTICS

summary(dat)
# Number of missing values per variable
colSums(is.na(dat))

# How many participants have at least one missing value?
sum(!complete.cases(dat))

# How many cholesterol values are missing?
sum(is.na(dat$chol))

mean(dat$time)
median(dat$time)
mean(dat$trt)
mean(dat$trt, na.rm=TRUE) #useful??

# Check possible values for categorical variables
table(dat$status, useNA = "always")
table(dat$trt)
table(dat$trt, useNA = "always")
table(dat$trt, exclude=NULL)

# Why should we not simply calculate the mean of trt?
mean(dat$trt, na.rm = TRUE)

# 5. VISUALIZE THE DATA
# Numerical variables
hist(dat$age)
hist(dat$bili)
boxplot(dat$protime)

# Categorical variables
barplot(table(dat$sex))
barplot(table(dat$stage, useNA = "always"))

# Relationships between numerical variables
plot(dat$age, dat$bili)

# Add meaningful labels
plot(dat$age, dat$bili,
     xlab = "Age (years)",
     ylab = "Serum bilirubin (mg/dl)",
     main = "Age and bilirubin")


# 6. INVESTIGATE SUSPICIOUS OBSERVATIONS

# Examine potentially unusual blood clotting times
boxplot(dat$protime)
summary(dat$protime)

# Define which observations should be inspected
myrows <- !is.na(dat$protime) & dat$protime > 20

# How many observations are affected?
sum(myrows)

# Inspect those observations
dat[myrows, c("id", "protime", "age", "bili")]

# Important:
# An unusual value is not necessarily an incorrect value!
# Do not automatically replace outliers with NA.
