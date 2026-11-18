
#This script is supposed to demonstrate how even relatively few missing variables per variable can lead to a large number of "deleted" observations in regressions.

dat <- data.frame(age=c(54,35,46,27,NA,9,16, 1, 5, 3, 46,30,41,99,NA,7,12, 0, 6,-1),
				  sex=c( 1, 0, 0, 1, 0,NA,1, 1, 0, 0,  1, 0, 0, 1, 0,NA,1, 1, 0, 3),
				  bmi=c(23,22,25,27,19,30,NA,17,24,22,21,20, 5,-4,21,80,NA,18,26,27),
				  smo=c( 0, 0, 1, 1, 0, 1, 0,NA,1, 0,  0, 1, 0, 0, 0, 1, 0,NA,0, 1),
				  fah=c( 1, 0, 1, 0, 1, 0, 1, 0,NA,1,  0, 1, 0, 0, 1, 1, 0, 0,NA,1), #family history
				  pyr=c( 0, 5, 10,8, 0, 0, 7, 9,0,NA,  2, 5, 7, 9, 0, 0, 1, 4,0,NA), #smoking pack years
				  crc=c( 0, 1, 0, 1, 0, 1, 0, 1,0, 1, NA, 0, 1, 0, 1, 0, 1, 0,1, 0)) #colorectal cancer


#An dem Beispiel auch demonstrieren: 9 year-old smoker + smoker mit 0 pack-years sind unplausibel!
#row-wise deletion of missing values.

#Check implausible data:
summary(dat)
# Negative age??
# 3rd sex??
# Negative BMI or BMI of 5??
# BMI of 80??
# smoker but 0 pack-years??

#But let's continue for the moment, assuming that all data entries are plausible.

glm(crc ~ age, data=dat, family="binomial")
glm(crc ~ sex, data=dat, family="binomial")
glm(crc ~ bmi, data=dat, family="binomial")
glm(crc ~ smo, data=dat, family="binomial")
glm(crc ~ fah, data=dat, family="binomial")
glm(crc ~ pyr, data=dat, family="binomial")
# Each of them has 3 deleted observations: 1 because of missing outcome, 2 because of missing predictors.


A1 <- glm(crc ~ age + sex, data=dat, family="binomial") #5 removed obs.
B1 <- glm(crc ~ age + sex + bmi, data=dat, family="binomial") #7 removed obs.
C1 <- glm(crc ~ age + sex + bmi + smo, data=dat, family="binomial") #9 removed obs.
D1 <- glm(crc ~ age + sex + bmi + smo + fah, data=dat, family="binomial") #11 removed obs.
E1 <- glm(crc ~ age + sex + bmi + smo + fah + pyr, data=dat, family="binomial") #13 removed obs.

lapply(list(A1, B1, C1, D1, E1), summary)

################################
# We should also handle the implausible values:
# Ideally, you would figure out the true value for those implausible data entries.
# Second-best: Set them to NA.

dat[!is.na(dat$age) & dat$age <0,]$age <- NA
dat[!is.na(dat$sex) & !dat$sex %in% c(0,1),]$sex <- NA
dat[!is.na(dat$bmi) & (dat$bmi <0 | dat$bmi>50),]$bmi <- NA #Actually, we should also delete the BMI of 5.
dat[!is.na(dat$smo) & !is.na(dat$pyr) & dat$smo==1 & dat$pyr==0,c("smo","pyr")] <- NA

A2 <- glm(crc ~ age + sex, data=dat, family="binomial") #6 removed obs.
B2 <- glm(crc ~ age + sex + bmi, data=dat, family="binomial") #9 removed obs.
C2 <- glm(crc ~ age + sex + bmi + smo, data=dat, family="binomial") #12 removed obs.
D2 <- glm(crc ~ age + sex + bmi + smo + fah, data=dat, family="binomial") #13 removed obs.
E2 <- glm(crc ~ age + sex + bmi + smo + fah + pyr, data=dat, family="binomial") #14 removed obs.

lapply(list(A2, B2, C2, D2, E2), summary)

# Important: Adding predictors may change the regression coefficients not only because of statistical adjustment, but also because different observations are included in the analysis.
# Thus, changes in coefficients between models may partly reflect differences in the analyzed samples rather than adjustment effects.


## Note also that 3 models produced warnings in this example: D1, C2, D2.

# Some models produce warnings due to complete separation.
# With very few observations remaining, combinations of predictors can perfectly distinguish between observations with crc=0 and crc=1.
# As a result, regression coefficients may become extremely large, and standard errors and p-values are unreliable.
# Even models without warnings may be severely overfitted. Thus, missing data not only reduce statistical power but can also make regression models unstable or impossible to estimate reliably.


# Compare the number of observations used in each regression:

models <- list(A1=A1, B1=B1, C1=C1, D1=D1, E1=E1,
               A2=A2, B2=B2, C2=C2, D2=D2, E2=E2)

data.frame(
  model = names(models),
  included = sapply(models, nobs),
  excluded = nrow(dat) - sapply(models, nobs)
)
