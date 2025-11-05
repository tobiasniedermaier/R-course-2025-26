
#######################################################################
# lineare Regression mit robusten Standardfehlern bei Heteroskedastie # #######################################################################


#von:
#http://thestatsgeek.com/2014/02/14/the-robust-sandwich-variance-estimator-for-linear-regression-using-r/

require(sandwich) || install.packages("sandwich", dep=T)

set.seed(194812)
n <- 100
x <- rnorm(n)
residual_sd <- exp(x)
y <- 2*x + residual_sd*rnorm(n)

plot(x,y) #heteroskedastische Daten

#"dumm stellen": Heteroskedastie ignorieren und normales lineares Modell fitten:
mod <- lm(y~x)
summary(mod)
par(mfrow=c(2,2))
plot(mod) #im ersten Plot sieht man schön, dass die Daten stark heteroskedastisch sind.

summary(mod)["coefficients"] #SE v. x: 0.311, p-Wert: 0.00025

library(sandwich)

#erstelle robuste Standardfehler:
vcovHC(mod, type="HC")
(sandwich_se <- diag(vcovHC(mod, type="HC"))^0.5) #sandwich_se v. x: 0.584
coef(mod) - 1.96*sandwich_se #unteres KI
coef(mod) + 1.96*sandwich_se #oberes KI

z_stat <- coef(mod)/sandwich_se
(p_values <- pchisq(z_stat^2, 1, lower.tail=FALSE))
#p-Wert: 0.043, vgl. mit 0.00025 mit normalen Standardfehlern
