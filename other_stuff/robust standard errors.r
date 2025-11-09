
#######################################################################
# linear regression with robust standard errors because of heteroscedasticity


#from:
#http://thestatsgeek.com/2014/02/14/the-robust-sandwich-variance-estimator-for-linear-regression-using-r/

require(sandwich) || install.packages("sandwich", dep=T)

set.seed(194812)
n <- 100
x <- rnorm(n)
residual_sd <- exp(x)
y <- 2*x + residual_sd*rnorm(n)

plot(x,y) #heteroscedastic data

#"Play dumb": Ignore heteroscedasticity and fit a normal linear model:
mod <- lm(y~x)
summary(mod)
par(mfrow=c(2,2))
plot(mod) #In the first plot you can see nicely that the data are heteroscedastic.

summary(mod)["coefficients"] #SE of x: 0.311, p value: 0.00025

library(sandwich)

#Create robust standard errors:
vcovHC(mod, type="HC")
(sandwich_se <- diag(vcovHC(mod, type="HC"))^0.5) #sandwich_se v. x: 0.584
coef(mod) - 1.96*sandwich_se #lower 95% CI
coef(mod) + 1.96*sandwich_se #upper 95% CI

z_stat <- coef(mod)/sandwich_se
(p_values <- pchisq(z_stat^2, 1, lower.tail=FALSE))
#p value: 0.043, compared to 0.00025 with normal standard errors

