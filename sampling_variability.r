
########################
# Sampling variability #
########################

#Die Kursleiterin/-Organisatorin hat mir noch folgenden "Auftrag" gegeben: 

#"Please do discuss sampling variability in class. this is one of the most important things students need to understand: Random sample do vary and this leads to uncertainty in the estimates which can be quantified by SE."

set.seed(123)

age <- 0:100

# Approximate age distribution
weights <- (0.55 + 0.45 * exp(-0.5 * ((age - 60) / 16)^2)) *
           plogis((86 - age) / 3)

# Parameter	Bedeutung
# 0.55	Grundhäufigkeit bei jungen Altersjahrgängen, relativ zum Maximum
# 60	Ungefähres Maximum der Altersverteilung
# 16	Breite des zusätzlichen Glockenkurvenanteils
# 86	Ungefährer Mittelpunkt des starken Abfalls im hohen Alter
# 3	Steilheit dieses Abfalls

#############################
# 1. Generate population

# Generate 84,000,000 individuals
dat <- data.frame(
  age = sample(age, size=84000000, replace=TRUE, prob=weights)
)

# Plot
hist(dat$age, breaks=seq(-0.5, 100.5, 1),
     xlab="Age (years)", main="Simulated age distribution",
     col="lightblue", border=NA)

dev.off()

#We assume that this is the real age distribution of the entire population in Germany. In epidemiological studies, we typically only have a random sample of this population, maybe a few hundred or thousand participants.

#############################
# 2. Draw ONE sample from that population and calculate the mean age


set.seed(1)
x <- sample(x=dat$age, size=1645, replace=FALSE)

#This would be the age distribution of ONE study that samples from the German population. The study has 1645 participants. (Arbitrarily selected. Should reflect a relatively typical size of epidemiological studies.)
mean(x) #45.7 years

#Compare this to the mean age of a DIFFERENT randomly drawn study population of the same size (1645 participants):
set.seed(234)
y <- sample(x=dat$age, size=1645, replace=FALSE)
mean(y) #47.2 years


#############################
# 3. Now calculate the mean age for 100 study samples:

set.seed(1234)

all_samples <- list()

for(i in 1:100){
 all_samples[[i]] <- sample(dat$age, size=1645, replace=FALSE)
}

t(sapply(all_samples, summary))
#You can see that in every sample (of size n=1645 each), we have a slightly different median and mean age. The minimum possible age is 0 years (although some samples might not contain participants of age 0 by chance); the maximum is (by assumption) capped at 100 years (although this might not be reached in all samples).


# Standard error of the mean (SEM):
# SEM = SD / sqrt(n)
#
# SD describes the variability of individual observations.
# SEM describes the sampling variability of the estimated mean.
#
# In real studies, the population SD is usually unknown.
# We therefore estimate the SEM using the sample SD:
#
# Estimated SEM = sample SD / sqrt(n)


# True population mean and SD
population_mean <- mean(dat$age)
population_sd   <- sd(dat$age)

population_mean
population_sd

#In real epidemiological studies, we typically do not know the true population parameters of interest. We therefore use random samples to estimate these parameters and quantify the resulting sampling uncertainty.


#############################
# 4. Sampling Distribution

# Mean age in each of the 100 samples
sample_means <- sapply(all_samples, mean)

# Examine the results
summary(sample_means)

# Plot distribution of sample means
hist(sample_means,
     main="Sampling distribution of the mean",
     xlab="Mean age (years)",
     col="lightblue",
     border="white")

# True population mean
abline(v=population_mean, col="red", lwd=2)


#############################
# 5. Empirical standard error
sd(sample_means)

# Theoretical standard error (using population SD)
population_sd / sqrt(1645)


# Calculate SD for each sample
sample_sds <- sapply(all_samples, sd)

# Calculate estimated SE for each sample
sample_SEs <- sample_sds / sqrt(1645)

# Compare three ways of obtaining SE

# 1. Empirical SE from 100 repeated samples
sd(sample_means)

# 2. Mean of the 100 individually estimated SEs
mean(sample_SEs)

# 3. SE calculated from the population SD
population_sd / sqrt(1645)



# Compare the variability of individual ages
# with the variability of sample means

sd(all_samples[[1]])  # SD of individual ages in one study

sd(sample_means)       # Empirical SE across 100 studies

sample_SEs[1]          # Estimated SE based on just one study


#############################
# 6. Point estimate and 95% confidence intervals for each of the 100 samples:

# Lower and upper confidence limits
lower <- sample_means - 1.96 * sample_SEs
upper <- sample_means + 1.96 * sample_SEs

# Display results
data.frame(
  Mean = round(sample_means, 1),
  SE = round(sample_SEs, 2),
  Lower_CI = round(lower, 1),
  Upper_CI = round(upper, 1)
)


# Does each 95% CI contain the true population mean?

coverage <- lower <= population_mean &
            upper >= population_mean

# Number of CIs containing the true population mean
sum(coverage)

# Percentage of CIs containing the true population mean
mean(coverage) * 100


# Plot the first 100 confidence intervals

plot(1:100, sample_means[1:100],
     ylim=range(c(lower, upper, population_mean)),
     xlab="Study sample",
     ylab="Mean age (years)",
     main="95% confidence intervals",
     pch=19)

segments(
  x0=1:100,
  y0=lower[1:100],
  x1=1:100,
  y1=upper[1:100],
  col=ifelse(coverage[1:100], "orange", "blue"),
  lwd=2
)

abline(h=population_mean, col="darkred", lwd=2)

legend("topright",
       legend=c("CI contains true mean",
                "CI misses true mean",
                "True population mean"),
       col=c("orange", "blue", "darkred"),
       lty=1,
       lwd=2,
       bty="n")

#8 of the 100 confidence intervals did not cover the true population mean. Which is a bit more than we would expect from a 95% confidence interval (-> 5 of 100).


#############################
# 7. Repeat the simulation with another 100 random samples

set.seed(4321)

all_samples2 <- list()

for(i in 1:100){
 all_samples2[[i]] <- sample(dat$age, size=1645, replace=FALSE)
}

# Calculate mean, SD and SE for each sample
sample_means2 <- sapply(all_samples2, mean)
sample_sds2 <- sapply(all_samples2, sd)
sample_SEs2 <- sample_sds2 / sqrt(1645)

# Calculate 95% confidence intervals
lower2 <- sample_means2 - 1.96 * sample_SEs2
upper2 <- sample_means2 + 1.96 * sample_SEs2

# Does each CI contain the true population mean?
coverage2 <- lower2 <= population_mean &
             upper2 >= population_mean

# Number of CIs NOT containing the true population mean
sum(!coverage2)

# Percentage of CIs containing the true population mean
mean(coverage2) * 100


#############################
# Plot the second set of 100 confidence intervals

plot(1:100, sample_means2,
     ylim=range(c(lower2, upper2, population_mean)),
     xlab="Study sample",
     ylab="Mean age (years)",
     main="95% confidence intervals (second simulation)",
     pch=19)

segments(
  x0=1:100,
  y0=lower2,
  x1=1:100,
  y1=upper2,
  col=ifelse(coverage2, "orange", "blue"),
  lwd=2
)

abline(h=population_mean, col="darkred", lwd=2)

legend("topright",
       legend=c("CI contains true mean",
                "CI misses true mean",
                "True population mean"),
       col=c("orange", "blue", "darkred"),
       lty=1,
       lwd=2,
       bty="n")

#5 of the 100 confidence intervals did not cover the true population mean. This corresponds to the expected number of 5 misses. However, the observed number varies randomly between simulation runs, even when the statistical method remains exactly the same.

#If you keep repeating this, you will sometines get 2 or 3 and sometimes 6 or 7 confidence intervals (or in rare cases even more or fewer) that do not cover the true population mean.


# Number of CIs missing the true population mean
# in the first simulation
sum(!coverage)

# Number of CIs missing the true population mean
# in the second simulation
sum(!coverage2)

# Both simulations combined (200 confidence intervals)
sum(!c(coverage, coverage2))

##############

# Distinguish between:

# a) Sampling variability of point estimates:
#    Different random samples produce different estimates of the population mean.

# b) Variability in the observed CI coverage rate:
#    The theoretical coverage probability is approximately 95%. However, when calculating 100 confidence intervals, the observed proportion containing the true population mean varies between simulation runs.

