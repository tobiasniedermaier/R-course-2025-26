
######################################
# Different types of regression in R #
######################################

## Linear regression:

mylm <- lm(mpg ~ hp + cyl + wt, data=mtcars)
mylm
summary(mylm)
coef(mylm)
confint(mylm)

glm(mpg ~ hp + cyl + wt, data=mtcars) #The same but with glm() instead of lm().
glm(mpg ~ hp + cyl + wt, data=mtcars, family="gaussian") #Now with explicitely stated regression type. (It's the same because gaussian is the default option for family.)


## Logistic regression:

data(Titanic)
df <- as.data.frame(Titanic)
titanic_full <- df[rep(seq_len(nrow(df)), df$Freq),] #Disaggregates the data set.
titanic_full$Freq <- NULL
#Or, more intuitive:
titanic_full2 <- tidyr::uncount(data=df, weights=Freq)
identical(titanic_full, titanic_full2) #TRUE

head(titanic_full)
rownames(titanic_full) <- NULL
head(titanic_full)

mylogreg <- glm(Survived ~ Class + Sex + Age, data=titanic_full, family="binomial")
exp(coef(mylogreg))
exp(confint(mylogreg))
glm(Survived ~ Class + Sex + Age, data=titanic_full, family=binomial(link="logit")) #The same as above, because link="logit" is the default option.


## Cox regression:

library(survival)
data(lung)

cox_full <- coxph(Surv(time, status) ~ pat.karno + wt.loss + age + sex, data=lung)
summary(cox_full)
round(summary(cox_full)$coef,3)

ph_test <- cox.zph(cox_full)
ph_test
plot(ph_test)


## Probit regression: (Quite similar to logistic regression in the results and interpretation, but slightly more computationally intense.)

myprobreg <- glm(Survived ~ Class + Sex + Age, data=titanic_full, family=binomial(link="probit"))
summary(myprobreg)
#To get predicted probabilities:
newdata1 <- data.frame(Class = "3rd", Sex = "Male", Age = "Adult")
predict(myprobreg, newdata1, type = "response")  # gives P(manual)

newdata2 <- data.frame(Class = "1st", Sex = "Female", Age = "Child")
predict(myprobreg, newdata2, type = "response")

## Poisson regression:
#Example: number of breaks in yarn from warpbreaks.

data(warpbreaks)

# Count outcome: breaks
table(warpbreaks$breaks)[1:5]  # see some counts

fit_pois <- glm(breaks ~ wool + tension, data = warpbreaks, family = poisson(link = "log"))
summary(fit_pois)
#Interpretation:
#Coefficients are on the log count scale.
exp(coef(fit_pois)) #gives multiplicative effects on expected counts.
#For example, exp(coef) for woolB is the rate ratio of breaks for wool B vs A.

## Negative binomial model for overdispersed count data:

library(MASS)
data("quine")

# Quick check: Days is a count
summary(quine$Days)

# Poisson model first
fit_pois_q <- glm(Days ~ Eth + Sex + Age + Lrn, data   = quine, family = poisson)

# Check overdispersion
dispersion <- sum(residuals(fit_pois_q, type = "pearson")^2) / fit_pois_q$df.residual
dispersion   # > 1 suggests overdispersion

# Negative binomial model
fit_nb <- glm.nb(Days ~ Eth + Sex + Age + Lrn, data = quine)
summary(fit_nb)

## Alternative to the negative binomial model in case of overdispersion: Quasi-poisson model.
fit_quasi_pois <- glm(Days ~ Eth + Sex + Age + Lrn, data   = quine, family = quasipoisson)
summary(fit_quasi_pois)

# | Feature                    | **Quasi-Poisson**                                                      | **Negative binomial**                              |
# | -------------------------- | ---------------------------------------------------------------------- | -------------------------------------------------- |
# | Likelihood                 | **No true likelihood** (quasi-likelihood only)                         | **Full likelihood model**                          |
# | Variance                   | Var(Y) = φ μ                                                           | Var(Y) = μ + μ²/θ                                  |
# | Dispersion                 | Estimated as φ (free parameter)                                        | θ = NB dispersion (estimated through ML)           |
# | Coefficient interpretation | Same as Poisson (log-rate ratios)                                      | Same as Poisson (log-rate ratios)                  |
# | Inference                  | Based on quasi-likelihood → no AIC                                     | Full likelihood → AIC, LR tests available          |
# | Good for                   | Overdispersion of *any form*                                           | Overdispersion that increases quadratically with μ |
# | Prediction                 | No true distribution → can’t get probabilities/CI intervals for counts | Can generate predicted count distributions         |

# Use quasi-Poisson when:
# - You only care about regression coefficients and robust standard errors.
# - You don’t need AIC, likelihood ratio tests, or comparing nested models.
# - Overdispersion exists but you don’t need a parametric count model.

# Use negative binomial when:
# - You want a true probability model for counts.
# - You want to compare models using AIC or likelihood ratio tests.
# - Overdispersion follows a quadratic mean–variance relationship, typical in biological/ecological data.
# - You need simulated/predicted count distributions.

# If you want valid inference only → quasi-Poisson is perfectly fine.
# If you want model selection, likelihood-based inference, or distribution-based prediction → negative binomial is better.
# If the overdispersion is very strong, NB is usually more appropriate.
# If overdispersion is mild (~1.5–2×) → quasi-Poisson is often good enough.


## Gamma regression:
data(cars)
summary(cars)  # dist > 0, speed > 0
fit_gamma <- glm(dist ~ speed, data   = cars, family = Gamma(link = "log"))
summary(fit_gamma)
exp(coef(fit_gamma))
#exp(coef(fit_gamma)["speed"]) is approximately the factor by which expected stopping distance changes for a 1-unit increase in speed.



##### Other types of regressions #####

# Not covered here:
# - ordinal regression
# - multinomial regression
# - zero-inflated models (poisson, negative binomial)
# - Tobit regression
# - quantile regression
# - Lasso regression
# - mixed effects models
# - generalized additive models
# - parametric survival models (exponential, Weibull, log-nomal etc.)
# - competing risks regrssion
# - dirichlet regression
# - robust regression

