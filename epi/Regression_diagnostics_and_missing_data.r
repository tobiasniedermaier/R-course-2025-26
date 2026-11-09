
################################
#### What you will learn here:

#- (Wrong) model specifications, their consequences, how to detect and fix them.
#- The impact of heteroscedasticity and how to deal with it.
#- The impact of systematically missing data and how to deal with it.


# ============================================================
# Required packages
# ============================================================

# Run this once if the packages are not installed yet:
# install.packages(c("lmtest", "sandwich", "mice"))


# ============================================================
# PART 1: A misspecified mean relationship
# ============================================================

# Learning objective:
# Distinguish an incorrectly specified mean relationship from
# heteroscedasticity. Robust standard errors cannot repair an
# incorrect functional form.

x <- -100:100

# Set the random seed to make the simulated data reproducible.
set.seed(1)

# Generate a quadratic relationship with normally distributed errors.
# The error standard deviation is 500 for every value of x.
#
# Therefore:
# E(Y | x)   = x^2
# Var(Y | x) = 500^2
#
# The conditional mean changes with x, but the conditional variance
# is constant: the errors are homoscedastic.
y <- x^2 + rnorm(n = length(x), mean = 0, sd = 500)

plot(x, y, main = "Quadratic relationship with constant error variance")


# --- Fit an incorrectly specified model ---

# This model assumes a straight-line relationship between x and y.
# It cannot describe the U-shaped relationship in the data.
lm1 <- lm(y ~ x)

summary(lm1)

# There is no evidence of a linear association (p approximately 0.938).
# However, this does NOT mean that x is unrelated to y.
#
# Across this symmetric range of x, the decreasing and increasing
# parts of the parabola cancel out when estimating a single slope.
# A strong nonlinear relationship can therefore coexist with a
# linear slope close to zero.


# --- Try robust standard errors for the incorrect model ---

# HC3 standard errors adjust the estimated uncertainty of the
# regression coefficients.
#
# They do not change the fitted coefficients or turn the straight
# line into a curve. The functional form is still inappropriate.
lmtest::coeftest(
  lm1,
  vcov. = sandwich::vcovHC(lm1, type = "HC3")
)


# --- Inspect diagnostic plots ---

# In particular, inspect the Residuals vs Fitted plot.
# A systematic curved pattern indicates that the model has not
# captured the mean relationship.
#
# Here, this pattern is caused by an omitted quadratic term,
# even though the data-generating errors have constant variance.
plot(lm1)


# --- Fit a correctly specified model ---

# I(x^2) tells R to include the numerical square of x as a predictor.
# This matches the mean relationship used to generate the data.
#
# A separate linear x term is not needed in this simulation because
# we know that its true coefficient is zero. In real applications,
# a quadratic model would commonly include both x and I(x^2).
lm2 <- lm(y ~ I(x^2))

summary(lm2)

# After correcting the mean relationship, the residuals should no
# longer show the systematic pattern caused by the missing curve.
plot(lm2)


# --- Test for heteroscedasticity ---

# The Breusch-Pagan test looks for evidence against constant error
# variance, using the model predictors by default.
#
# We know from the simulation that the errors are homoscedastic.
# A non-significant test result does not itself prove homoscedasticity.
# Even with homoscedastic errors, a test can reject by chance.
lmtest::bptest(lm2)

# Main lesson:
# First check whether the model describes the mean relationship.
# Robust standard errors cannot substitute for an appropriate
# specification of that relationship.


# ============================================================
# PART 2: Missing values and changing analysis samples
# ============================================================

# Learning objective:
# Understand why models fitted to the same dataset can use
# different observations when their variables contain missing values.

set.seed(42)
dat <- iris
p_missing <- 0.2

# Each measurement has a 20% probability of being missing.
# Missingness is generated independently of the measured values
# and independently across the four measurement variables.
# This is a missing completely at random (MCAR) mechanism.

dat$Sepal.Length[runif(nrow(dat)) < p_missing] <- NA
dat$Sepal.Width[runif(nrow(dat)) < p_missing] <- NA
dat$Petal.Length[runif(nrow(dat)) < p_missing] <- NA
dat$Petal.Width[runif(nrow(dat)) < p_missing] <- NA

# Count missing values separately for each variable.
colSums(is.na(dat))

# Different models require different combinations of variables.
# By default, lm() excludes rows with missing values in either
# the outcome or any predictor included in that model.

model1 <- lm(Sepal.Length ~ Sepal.Width, data = dat)
model2 <- lm(Sepal.Length ~ Petal.Length, data = dat)
model3 <- lm(Sepal.Length ~ Petal.Width, data = dat)

model4 <- lm(
  Sepal.Length ~ Sepal.Width + Petal.Length,
  data = dat
)

model5 <- lm(
  Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
  data = dat
)

summary(model1)
summary(model2)
summary(model3)
summary(model4)
summary(model5)

# Compare the actual sample sizes used by the models.
# Equal sample sizes would not necessarily mean identical rows.
sapply(
  list(model1 = model1, model2 = model2, model3 = model3,
       model4 = model4, model5 = model5),
  nobs
)

# As more variables are required, fewer complete cases remain.
# Changes in coefficients between models may reflect both:
# 1. changes in the predictors included;
# 2. changes in the observations included.

# To isolate the effect of adding predictors, refit the nested
# models using the same set of complete observations.
common_sample <- dat[complete.cases(dat[, 1:4]), ]

summary(lm(
  Sepal.Length ~ Sepal.Width,
  data = common_sample
))

summary(lm(
  Sepal.Length ~ Sepal.Width + Petal.Length,
  data = common_sample
))

summary(lm(
  Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
  data = common_sample
))

# Using a common sample makes these comparisons easier to interpret,
# but discards observations that simpler models could otherwise use.
# It does not generally solve bias from selective missingness.
#
# In this example, missingness is MCAR. The later BMI example
# demonstrates a mechanism that biases complete-case regression.


# ============================================================
# PART 3: Heteroscedasticity with a correctly specified mean
# ============================================================

# Learning objective:
# Recognize changing error variance and understand what robust
# standard errors do, and what they do not do.

set.seed(2)

# Use positive x values so that 0.2*x is a valid standard deviation.
x <- runif(n = 1000, min = 1, max = 100)

# The conditional mean is linear, but the error standard deviation
# increases with x.
#
# E(Y | x)   = x
# Var(Y | x) = (0.2*x)^2
#
# Observations at larger x values therefore scatter more widely
# around the true regression line.
y <- rnorm(n = length(x), mean = x, sd = 0.2*x)

plot(x, y, main = "Linear relationship with increasing error variance")

# Add the true mean relationship used to generate the data.
abline(a = 0, b = 1, col = "red", lwd = 2)


# --- Fit the mean model ---

# The linear mean relationship is correctly specified.
# Heteroscedasticity does not by itself make the OLS coefficients
# biased here, but the usual standard errors can be incorrect.
lm3 <- lm(y ~ x)


# --- Inspect diagnostic plots ---

# Residuals vs Fitted:
# Look for a funnel-shaped pattern. The residual spread increases
# as the fitted values increase.
plot(lm3, which = 1)

# Scale-Location:
# This plot emphasizes changes in residual spread.
# An upward trend indicates increasing variability.
plot(lm3, which = 3)


# --- Test for heteroscedasticity ---

# A small p-value provides evidence against constant error variance.
# Interpret the test together with the plots and the model structure.
lmtest::bptest(lm3)


# --- Compare conventional and robust inference ---

# summary() reports the conventional standard errors, which assume
# constant error variance for this ordinary least-squares model.
summary(lm3)

# Estimate a heteroscedasticity-robust covariance matrix.
# HC3 includes a leverage-based correction.
V_robust <- sandwich::vcovHC(lm3, type = "HC3")

# Report the same regression coefficients, now with robust standard
# errors, test statistics, and p-values.
lmtest::coeftest(lm3, vcov. = V_robust)

# Calculate confidence intervals using the same robust covariance
# matrix, so that the intervals match the robust coefficient tests.
lmtest::coefci(lm3, vcov. = V_robust)

# Compare the standard errors directly.
# Robust standard errors are not necessarily larger than conventional
# ones; the direction of the difference depends on the data.
cbind(
  SE_conventional = sqrt(diag(vcov(lm3))),
  SE_robust_HC3 = sqrt(diag(V_robust))
)

# Main lesson:
# Robust standard errors change inference about the coefficients.
# They do not change the fitted coefficients, fitted values, or
# residuals, and they do not remove the funnel-shaped pattern.
#
# The association is strong in this example, so both approaches
# may give very small p-values. Focus on the standard errors and
# confidence intervals rather than only on statistical significance.


# ============================================================
# PART 4: Missing BMI values and multiple imputation
# ============================================================

# Learning objective:
# Show how complete-case analysis can distort an association when
# BMI is more likely to be missing among people with high blood
# pressure, and how correctly specified multiple imputation can
# address this selection bias under a MAR mechanism.

set.seed(123)
n <- 2000


# --- Generate complete data ---

full <- data.frame(
  bmi = rnorm(n, mean = 27, sd = 4)
)

# Generate systolic blood pressure with a positive association
# with BMI.
#
# The true slope is 2 mmHg per additional BMI unit.
# At BMI = 25, expected systolic blood pressure is 120 mmHg.
full$sbp <- 120 + 2 * (full$bmi - 25) +
  rnorm(n, mean = 0, sd = 12)

# This model provides a benchmark using all original observations.
# Its estimated slope will differ slightly from 2 due to sampling
# variation, even though the model is correctly specified.
mod_full <- lm(sbp ~ bmi, data = full)


# --- Introduce missing BMI values ---

# plogis() converts its input into a probability between 0 and 1.
# The probability that BMI is missing increases with blood pressure.
#
# At blood pressure = 130 mmHg, the missingness probability is 50%.
# Lower blood pressure gives a lower probability; higher blood
# pressure gives a higher probability.
p_missing <- plogis((full$sbp - 130) / 6)

# Randomly determine which BMI values are missing, using each
# person's individual missingness probability.
#
# Missingness is still stochastic, but it is not equally likely
# for everyone.
bmi_missing <- runif(n) < p_missing

# Preserve the original data for comparison and create a version
# in which the selected BMI values are replaced by NA.
dat <- full
dat$bmi[bmi_missing] <- NA

# Calculate the proportion of participants with missing BMI.
mean(bmi_missing)

# This is a MAR mechanism:
# Missingness depends on blood pressure, which is fully observed.
# Conditional on blood pressure, it does not additionally depend
# on the unobserved BMI.
#
# MAR does NOT imply that complete cases are a representative
# sample for every analysis.


# --- Analyze complete cases only ---

# By default, lm() excludes observations with missing values in
# variables required by the model.
#
# Participants with high blood pressure are less likely to remain
# in this analysis because their BMI is more often missing.
mod_cc <- lm(sbp ~ bmi, data = dat)

# This selection can flatten the estimated relationship:
# among participants with high BMI, those with comparatively low
# blood pressure are more likely to remain observed.
#
# In this example, complete-case analysis therefore tends to
# underestimate the slope of blood pressure on BMI.


# --- Impute missing BMI values using observed blood pressure ---

# The imputation model predicts BMI from blood pressure.
# This is deliberately the reverse direction of the final analysis:
# imputation models the variable that needs to be filled in.
#
# BMI and blood pressure are jointly normally distributed in this
# simulation. A normal linear model for BMI conditional on blood
# pressure is therefore correctly specified.
#
# The "norm" method draws model parameters and residual variation,
# rather than simply replacing missing values with fitted means.

methods <- mice::make.method(dat)

# Fully observed variables do not need an imputation method.
methods[] <- ""
methods["bmi"] <- "norm"

# Explicitly specify blood pressure as the predictor of missing BMI.
predictors <- mice::make.predictorMatrix(dat)
predictors[,] <- 0
predictors["bmi", "sbp"] <- 1

# Create 50 completed datasets with different plausible BMI values.
#
# One iteration is sufficient in this particular example because
# only BMI is incomplete and its predictor is fully observed.
# More complex patterns involving several incomplete variables
# generally require multiple iterations and convergence checks.
#
# Only dat is supplied: the original, hidden BMI values in full
# are NOT used by the imputation procedure.
imp <- mice::mice(
  dat,
  m = 50,
  maxit = 1,
  method = methods,
  predictorMatrix = predictors,
  seed = 456,
  printFlag = FALSE
)


# --- Fit the analysis model and pool the results ---

# Fit the same regression separately in each completed dataset.
models_mi <- with(imp, lm(sbp ~ bmi))

# Combine estimates and uncertainty using Rubin's rules.
# Pooling accounts for both within-imputation uncertainty and
# differences between the imputed datasets.
#
# Do not stack the datasets and treat them as independent observations.
mi_result <- summary(
  mice::pool(models_mi),
  conf.int = TRUE
)

mi_result


# --- Compare estimated slopes ---

comparison <- data.frame(
  Analysis = c(
    "True value",
    "Complete data",
    "Complete cases",
    "Multiple imputation"
  ),
  Slope = c(
    2,
    unname(coef(mod_full)["bmi"]),
    unname(coef(mod_cc)["bmi"]),
    mi_result$estimate[mi_result$term == "bmi"]
  )
)

print(comparison)

# Expected pattern:
# The complete-case slope is too shallow, whereas the MI estimate
# is closer to the complete-data estimate and the true slope.
#
# This is not guaranteed in every individual simulation.
# Bias is a property of an estimator across repeated samples;
# one simulated dataset illustrates the mechanism but does not
# establish unbiasedness by itself.


# ============================================================
# Visualize the effect of missingness and imputation
# ============================================================

# Extract one completed dataset to illustrate the imputed values.
# The pooled analysis above still uses all 50 completed datasets.
dat_imp <- mice::complete(imp, action = 1)

# Extract pooled coefficients for the MI regression line.
mi_intercept <- mi_result$estimate[
  mi_result$term == "(Intercept)"
]

mi_slope <- mi_result$estimate[
  mi_result$term == "bmi"
]

# Use identical axis limits so the four panels can be compared.
x_limits <- range(c(full$bmi, dat_imp$bmi))
y_limits <- range(full$sbp)

# Save the current layout settings and create a 2-by-2 layout.
oldpar <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# Helper function for consistently formatted scatterplots.
mydotplot <- function(x, y, title, color = "grey60") {
  plot(
    x, y,
    xlim = x_limits,
    ylim = y_limits,
    pch = 16,
    cex = 0.45,
    col = color,
    xlab = "BMI",
    ylab = "Systolic blood pressure (mmHg)",
    main = title
  )
}

# The true mean relationship is:
# 120 + 2*(BMI - 25) = 70 + 2*BMI.
#
# Add it as a dashed black reference line to every panel.
true_slope <- function() {
  abline(a = 70, b = 2, col = "black", lty = 2, lwd = 2)
}


# --- Panel A: Original complete data ---

# The green line is estimated from the complete data.
# It should be close to the dashed true relationship.
mydotplot(
  full$bmi, full$sbp,
  "A: Complete data"
)

true_slope()
abline(mod_full, col = "darkgreen", lwd = 2)


# --- Panel B: Reveal the selective missingness mechanism ---

# Red points identify participants whose BMI becomes missing.
# They occur more frequently at higher blood pressure values.
#
# We can display their true BMI only because this is a simulation.
# In a real dataset, these missing BMI values would be unknown.
mydotplot(
  full$bmi, full$sbp,
  "B: BMI more often missing\nat high blood pressure",
  color = ifelse(bmi_missing, "tomato", "grey60")
)

true_slope()

legend(
  "bottomright",
  legend = c("BMI observed", "BMI missing"),
  col = c("grey60", "tomato"),
  pch = 16,
  bty = "n"
)


# --- Panel C: Complete cases only ---

# Points with missing BMI cannot be plotted and are omitted.
# The blue line is estimated using only the remaining observations.
#
# Compare its slope with the dashed true relationship.
# This demonstrates the distortion caused by selective inclusion.
mydotplot(
  dat$bmi, dat$sbp,
  "C: Complete cases"
)

true_slope()
abline(mod_cc, col = "blue", lwd = 2)


# --- Panel D: Observed and imputed BMI values ---

# Orange points contain imputed BMI values; their blood pressure
# values were observed throughout.
#
# Imputation supplies plausible BMI values, not necessarily the
# original BMI of each individual participant.
mydotplot(
  dat_imp$bmi, dat_imp$sbp,
  "D: Orange = imputed BMI",
  color = ifelse(bmi_missing, "darkorange", "grey60")
)

true_slope()

# The purple line uses the pooled coefficients from all 50 analyses.
# The plotted points illustrate only the first completed dataset.
abline(
  a = mi_intercept,
  b = mi_slope,
  col = "purple",
  lwd = 2
)

# Restore the previous graphics settings.
par(oldpar)


# Final lessons:
#
# 1. Selective missingness can bias a complete-case regression.
#
# 2. Under this MAR mechanism, blood pressure contains the information
#    needed to model missingness-related selection when imputing BMI.
#
# 3. Correctly specified MI can address the resulting bias while
#    accounting for uncertainty about the missing values.
#
# 4. MI is not a universal correction for missing data: its validity
#    depends on the missingness assumptions and imputation model.
#
# 5. A scatterplot of complete cases alone cannot establish whether
#    selection bias is present. Here, the complete-data benchmark is
#    available only because we generated the data ourselves.
