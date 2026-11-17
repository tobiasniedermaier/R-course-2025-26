
# ================================================================
# RANDOM WALKS, SPURIOUS TRENDS, AND MULTIPLE TESTING
# ================================================================

# Objective:
# This simulation demonstrates that statistically significant
# results can occur even when there is no true systematic effect.
#
# We will investigate two related statistical problems:
#
# 1. Multiple testing:
#    When many statistical tests are performed, some will produce
#    significant results purely by chance, even if all null
#    hypotheses are true.
#
# 2. Violation of model assumptions:
#    Applying ordinary linear regression to random walks can
#    produce a very large proportion of significant trends,
#    even though the data-generating process has no systematic
#    upward or downward drift.
#
# We will compare two methods:
# - Linear regression on the original random-walk observations
# - A t-test on the differences between consecutive observations


# ================================================================
# 1. GENERATE RANDOM-WALK DATA
# ================================================================

set.seed(123)  # Ensures reproducibility

# Simulate 10,000 individuals, each observed over 50 time steps.
# Including the baseline (t = 0), each individual has 51 observations.

n_people <- 10000
n_steps <- 50

# Generate baseline values.
# Individuals start at different values, with a population
# mean of 50 and a standard deviation of 5.

dat <- data.frame(
  t0 = rnorm(n = n_people, mean = 50, sd = 5)
)

# Generate a random walk for each individual.
#
# At every time step, a random change is added to the
# previous observation:
#
#   X(t) = X(t-1) + e(t)
#
# where e(t) follows a normal distribution with
# mean = 0 and standard deviation = 1.
#
# Importantly, there is NO systematic upward or downward trend:
# The expected change at each time step is exactly zero.
#
# However, individual trajectories may appear to increase,
# decrease, or fluctuate over time.
#
# Random changes also accumulate: a positive change at one
# time step affects all subsequent observations.
#
# Consequently, the variance of the observations increases
# over time, even though their expected value remains constant.

for (i in 1:n_steps) {
  dat[, i + 1] <- dat[, i] +
    rnorm(n = n_people, mean = 0, sd = 1)
}

time <- 0:n_steps


# ================================================================
# 2. VISUALIZE SOME RANDOM WALKS
# ================================================================

# Plot the trajectories of the first 10 individuals.
#
# Although all individuals follow the same random process
# without systematic drift, some trajectories may look
# as if they exhibit a clear positive or negative trend.

matplot(
  time,
  t(as.matrix(dat[1:10, ])),
  type = "l",
  lty = 1,
  xlab = "Time",
  ylab = "Observed value",
  main = "Ten random walks without systematic drift"
)


# ================================================================
# 3. TEST FOR LINEAR TRENDS USING ORDINARY LINEAR REGRESSION
# ================================================================

# Fit a separate linear regression for each individual:
#
#   Y(t) = beta0 + beta1 * time + error
#
# Null hypothesis:
#   H0: beta1 = 0 (no linear trend)
#
# Alternative hypothesis:
#   H1: beta1 != 0 (positive or negative linear trend)
#
# Extract the p-value for the regression coefficient of time.

p_values <- apply(dat, 1, function(y) {
  summary(lm(y ~ time))$coefficients["time", "Pr(>|t|)"]
})

# Count individuals with a statistically significant trend.

sum(p_values < 0.05)

# Calculate the proportion of individuals with a
# statistically significant trend.

mean(p_values < 0.05)


# INTERPRETATION:
#
# Surprisingly, a very large proportion of individuals
# (often around 80%) will have a statistically significant
# regression coefficient for time.
#
# How is this possible when the random walks were generated
# without any systematic drift?
#
# The problem is that the ordinary regression t-test assumes
# an error structure that is incompatible with a random walk.
#
# Consecutive random-walk observations are strongly dependent:
# Each observation contains all previous random changes.
#
# A series of positive random changes can therefore generate
# a persistent upward movement, even though each individual
# change is purely random.
#
# Ordinary linear regression may incorrectly interpret
# this accumulated random movement as evidence of a
# systematic trend.
#
# The conventional regression standard errors and p-values
# are not valid for this random-walk data-generating process.
#
# This is an example of spurious statistical significance
# arising from nonstationary time-series data.


# ================================================================
# 4. CORRECT FOR MULTIPLE TESTING
# ================================================================

# We have performed 10,000 statistical tests.
#
# Even if the individual tests were valid, performing so many
# tests would produce some significant results by chance.
#
# Under 10,000 true null hypotheses and correctly calibrated
# tests, approximately 5% (500 tests) would be expected to
# yield p < 0.05.
#
# The Benjamini-Hochberg (BH) procedure addresses multiple
# testing by controlling the false discovery rate (FDR)
# under suitable assumptions.

p_adjusted <- p.adjust(p_values, method = "BH")

sum(p_adjusted < 0.05)

# INTERPRETATION:
#
# Many random walks may still have significant results,
# even after correction for multiple testing.
#
# This is because multiple testing is NOT the main problem.
#
# The original regression p-values are invalid due to
# the stochastic dependence and nonstationarity of the
# random-walk observations.
#
# Multiple-testing corrections cannot fix a violation
# of the underlying statistical model assumptions.


# ================================================================
# 5. A MORE APPROPRIATE TEST: ANALYZE FIRST DIFFERENCES
# ================================================================

# Instead of analyzing the original observations, calculate
# the differences between consecutive time points:
#
#   D(t) = X(t) - X(t-1)
#
# For our simulated random walks, these differences are
# exactly the independent random changes used to generate
# the observations.
#
# They follow a normal distribution with mean = 0 and
# standard deviation = 1.
#
# We can therefore use a one-sample t-test to investigate
# whether the mean change differs significantly from zero.
#
# Null hypothesis:
#   H0: mean(D) = 0 (no systematic drift)
#
# Alternative hypothesis:
#   H1: mean(D) != 0 (systematic positive or negative drift)
#
# Unlike the original random-walk observations, the
# first differences satisfy the independence assumption
# in this particular simulation.

p_values_rw <- apply(dat, 1, function(y) {
  t.test(diff(y), mu = 0)$p.value
})

# Count individuals with a significant mean change.

sum(p_values_rw < 0.05)

# Calculate the proportion with a significant mean change.

mean(p_values_rw < 0.05)

# INTERPRETATION:
#
# We now expect approximately 5% significant results.
#
# This is exactly what we would expect from valid
# statistical tests when all null hypotheses are true
# and the significance level is alpha = 0.05.
#
# The significant results are false positives resulting
# from random sampling variation.
#
# Unlike the regression on the original observations,
# the t-tests on first differences have the intended
# Type I error rate in this simulation.


# ================================================================
# 6. CORRECT THE NEW P-VALUES FOR MULTIPLE TESTING
# ================================================================

p_adjusted_rw <- p.adjust(p_values_rw, method = "BH")

sum(p_adjusted_rw < 0.05)

# INTERPRETATION:
#
# After FDR correction, we will usually find no
# significant results, although a small number of
# significant results is still possible by chance.
#
# In our simulation, all null hypotheses are true.
# Therefore, any significant result is a false positive.
#
# The BH procedure is designed to control the expected
# proportion of false discoveries among rejected hypotheses,
# rather than guarantee that no false positives occur.


# ================================================================
# 7. COMPARISON: INDEPENDENT RANDOM OBSERVATIONS
# ================================================================

# Finally, consider a different simulation.
#
# Instead of random walks, generate 51 independent random
# observations for each individual.
#
# In this case, observations do not accumulate previous
# random changes. There is no temporal dependence or
# systematic trend.
#
# Ordinary linear regression is appropriate for these data,
# because the observations satisfy its error assumptions.

dat_independent <- matrix(
  rnorm(n = n_people * (n_steps + 1), mean = 50, sd = 1),
  nrow = n_people
)

# Test for linear trends as before.

p_values_independent <- apply(dat_independent, 1, function(y) {
  summary(lm(y ~ time))$coefficients["time", "Pr(>|t|)"]
})

mean(p_values_independent < 0.05)

# INTERPRETATION:
#
# Approximately 5% of the regressions should be significant,
# even though the observations are entirely random.
#
# This illustrates the ordinary multiple-testing problem:
# With sufficiently many tests, some significant results
# will occur purely by chance.
#
# Compare this result with the random-walk regression:
#
# Independent random observations:
#   Approximately 5% significant linear trends
#
# Random walks:
#   Often around 80% significant linear trends
#
# Random-walk first differences:
#   Approximately 5% significant mean changes
#
# The comparison shows that a large number of significant
# findings may result either from multiple testing or
# from inappropriate statistical models.


# ================================================================
# TAKE-HOME MESSAGES
# ================================================================

# 1. Statistical significance does not necessarily imply
#    the existence of a genuine systematic effect.
#
# 2. Multiple testing produces false positives even when
#    every individual statistical test is valid.
#
# 3. Violations of model assumptions can dramatically
#    increase the frequency of apparently significant results.
#
# 4. Time-series observations are often dependent, and
#    standard regression methods may be inappropriate.
#
# 5. First differencing can help when the data follow a
#    random-walk process. However, differencing is not
#    a universal solution for all time-series problems.
#
# 6. Multiple-testing corrections address the accumulation
#    of false discoveries across tests, but they cannot
#    compensate for invalid individual statistical tests.
