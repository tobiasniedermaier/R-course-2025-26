

###########################
# Calculate unadjusted and adjusted relative risks from a cohort study:

# Install packages (if not installed yet) and load them:

library(parameters)

dat <- read.csv("confounding_example.csv")

# Unadjusted relative risk
model1 <- glm(disease ~ inactive, data = dat,
              family = poisson(link = "log"))

#Exercise: Calculate the unadjusted relative risks "manually" (2x2 table, without regression).

# Adjusted relative risk
model2 <- glm(disease ~ inactive + older, data = dat,
              family = poisson(link = "log"))

# Relative risks and 95% confidence intervals
model_parameters(model1, exponentiate = TRUE, vcov = "HC0")
model_parameters(model2, exponentiate = TRUE, vcov = "HC0")


########################

