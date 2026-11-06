

###########################
# Calculate unadjusted and adjusted relative risks from a cohort study:

# Install packages (if not installed yet) and load them:

library(parameters)

dat <- read.csv("confounding_example_rare.csv")

# Unadjusted relative risk
model1 <- glm(disease ~ inactive, data = dat,
              family = poisson(link = "log"))

# Adjusted relative risk
model2 <- glm(disease ~ inactive + older, data = dat,
              family = poisson(link = "log"))

# Relative risks and 95% confidence intervals
model_parameters(model1, exponentiate = TRUE, vcov = "HC0")
model_parameters(model2, exponentiate = TRUE, vcov = "HC0")

##Exercise: Calculate the unadjusted relative risks "manually" (2x2 table, without regression).

mytab <- table(dat[,c("disease","inactive")])

e_s <- mytab[2,2]
e_ns <- mytab[1,2]
ne_s <- mytab[2,1]
ne_ns <- mytab[1,1]

(e_s / (e_s + e_ns)) / (ne_s / (ne_s + ne_ns)) #Tada!

##Exercise: Calculate the adjusted and unadjusted odds ratio (the latter by regression and "manually").
