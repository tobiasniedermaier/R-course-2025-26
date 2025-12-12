
##############################################################################
# Linear regression with robust standard errors because of heteroscedasticity


#from:
#http://thestatsgeek.com/2014/02/14/the-robust-sandwich-variance-estimator-for-linear-regression-using-r/

# Aim of this example
# -------------------
# 1. Simulate data from a *correctly specified* linear mean model:
#        E[Y | X] = β0 + β1 X
#    but with error variance that changes with X (heteroscedasticity).
#
# 2. Show that:
#    - OLS coefficients are still fine (they estimate the right βs),
#    - but the *usual* standard errors from lm() rely on the assumption
#      of *homoscedasticity* (constant error variance),
#      so with heteroscedasticity they become wrong → misleading p-values.
#
# 3. Compute heteroscedasticity-consistent (robust / sandwich) standard
#    errors, which remain valid even when Var(ε_i | X) is not constant.
#
# Background: where the usual SE formula comes from
# -------------------------------------------------
# In the classical linear model we assume, among other things:
#
#   (1) E[ε_i | X] = 0                      (errors have mean 0)
#   (2) Var(ε_i | X) = σ^2 for all i       (homoscedasticity)
#   (3) Cov(ε_i, ε_j | X) = 0, i ≠ j       (no correlation)
#
# Under these assumptions,
#
#   Var(β̂ | X) = σ^2 (X'X)^(-1).
#
# lm() uses this formula (with σ^2 estimated from residuals) to compute
# the "usual" standard errors. This is only correct if (2) holds.
#
# With heteroscedasticity we instead have
#
#   Var(β̂ | X) = (X'X)^(-1) X' Ω X (X'X)^(-1),
#
# where Ω is a diagonal matrix with Var(ε_i | X) on the diagonal, and
# these variances can differ across observations.
# If lm() still pretends Ω = σ^2 I, its standard errors are inconsistent:
# typically too small where the variance is large → overconfident
# p-values and too-narrow confidence intervals.
#
# The robust / sandwich estimator (vcovHC) estimates X'ΩX directly from
# the residuals, leading to consistent standard errors even when
# Var(ε_i | X) is unknown and non-constant.
#
# When is this relevant in practice?
# ----------------------------------
# Use robust SEs when you suspect heteroscedasticity, for example:
# - Residual-vs-fitted plots show a "funnel" shape (variance grows/shrinks).
# - The outcome is measured with different precision in different ranges
#   of a regressor (e.g. richer vs poorer households, large vs small firms).
# - Pooled cross-sectional data combining very different groups.
#
# In such cases, OLS coefficients themselves are usually still OK
# *if the mean model is correctly specified*, but you should not trust
# the naive standard errors and p-values.

#######################################################################
# Packages:

require(sandwich) || (install.packages("sandwich", dep=T) && require(sandwich))

#######################################################################
# Simulate data with heteroscedastic errors

set.seed(194812)
n <- 100

# Regressor X ~ N(0,1)
x <- rnorm(n)

# Error standard deviation that *depends on X*:
#   residual_sd = exp(X)  → variance increases with X.
# This means Var(ε_i | X = x_i) = exp(2 x_i), so errors are heteroscedastic.
residual_sd <- exp(x)

# True mean model: E[Y | X] = 2 * X
# Data generating process: Y = 2X + ε,  ε ~ N(0, residual_sd^2)
y <- 2*x + residual_sd*rnorm(n)

# Visual check: scatterplot shows variance increasing with X.
plot(x, y, main = "Simulated data with heteroscedastic errors") #heteroscedastic data

#######################################################################
# Play dumb": Ignore heteroscedasticity and fit a normal linear model:

mod <- lm(y~x)
summary(mod) # Summary uses the *homoscedasticity-based* covariance formula.
par(mfrow=c(2,2))
plot(mod) #In the first plot (Residuals vs Fitted) you can see nicely that the data are heteroscedastic: Residual variance increases with the fitted values.
# Consequence: the model-based SEs shown in summary(mod) are no longer reliable

# Look at the usual (naive) standard errors and p-values:
summary(mod)["coefficients"] #SE of x: 0.311, p value: 0.00025

#######################################################################
# Robust (heteroscedasticity-consistent) standard errors:

# The function vcovHC() computes a robust estimator of Var(β̂ | X).
# type = "HC" uses a default HC estimator (often HC3/HC0 depending on version).
# The key idea: instead of assuming Ω = σ^2 I, vcovHC uses the squared
# residuals to estimate the heteroscedastic pattern.

library(sandwich)

#Create robust standard errors:
vcovHC(mod, type="HC") # Robust variance–covariance matrix of the coefficients
(sandwich_se <- sqrt(diag(vcovHC(mod, type="HC"))) #sandwich_se v. x: 0.584
# Compare: robust SE for x is typically *larger* than the naive one.
# E.g. robust SE(x) ~ 0.58 instead of ~ 0.31.

# Note: OLS coefficient estimates themselves do not change:

# Build robust 95% confidence intervals using the robust SEs.
# Here we use 1.96 as the normal critical value (large-sample approximation).
coef(mod) - 1.96*sandwich_se #lower 95% CI
coef(mod) + 1.96*sandwich_se #upper 95% CI

# Interpretation:
# The CI for x is now wider because we acknowledge the extra uncertainty from heteroscedasticity.
# With naive SEs we would be overly confident.

# Robust test statistics and p-values:
z_stat <- coef(mod)/sandwich_se
(p_values <- pchisq(z_stat^2, 1, lower.tail=FALSE))
#p value: 0.043, compared to 0.00025 with normal standard errors

# For the slope x, the robust p-value is typically much larger than the naive p-value from summary(mod). In one run you might see:
# - naive p-value ≈ 0.00025  (strong evidence if we *ignore* heteroscedasticity)
# - robust p-value ≈ 0.04    (still significant at 5%, but much less dramatic)

# This illustrates: Heteroscedasticity does not bias the OLS *coefficients*, but it can severely distort the *standard errors* and, therefore, your conclusions about significance if you ignore it.
