
#Common statistical tests:
#see also: https://stats.idre.ucla.edu/r/whatstat/what-statistical-analysis-should-i-usestatistical-analyses-using-r/

##Chi-squared:
library(vcd)
(mytable <- xtabs(~Treatment+Improved, data=Arthritis))
#         Improved
#Treatment None Some Marked
#  Placebo   29    7      7
#  Treated   13    7     21
chisq.test(mytable)
chisq.test(mytable, correct=FALSE)

##Fisher's exact test:
fisher.test(mytable)
fisher.test(mytable, simulate.p.value=TRUE, B=5000)

##LR, Pearson, Phi, contingency coeff., Cramer's V:
assocstats(mytable) #from the vcd package (visualizing categorical data)
summary(assocstats(mytable)) #etwas ausführlicher

##One-sample t test: (example taken from http://r-statistics.co/Statistical-Tests-in-R.html)
set.seed(100)
x <- rnorm(50, mean = 10, sd = 0.5)
t.test(x, mu=10)

##Two-sample t-Test (Welch test):
t.test(1:10, y = c(7:20))

#Note that you can also use the "formula syntax" and that you can specify if you want to test one- or two-sided and the alpha level:
with(mtcars, t.test(mpg[am == 0], mpg[am == 1]))
t.test(mtcars$mpg ~ mtcars$am) #same
t.test(mpg ~ am, data=mtcars) #same
t.test(mpg ~ am, data=mtcars, alternative="two.sided") #Same, because two.sided is the default option.
t.test(mpg ~ am, data=mtcars, alternative="greater") #One-sided
t.test(mpg ~ am, data=mtcars, alternative="less") #One-sided
t.test(mpg ~ am, data=mtcars, conf.level=0.90) #For 90% confidence intervals.

##Paired t-Test:
#Time in seconds before training (a) and after training (b) for each athlete:
a = c(12.9, 13.5, 12.8, 15.6, 17.2, 19.2, 12.6, 15.3, 14.4, 11.3)
b = c(12.7, 13.6, 12.0, 15.2, 16.8, 20.0, 12.0, 15.9, 16.0, 11.1)

t.test(a,b, paired=TRUE)

#A practical example for the paired t-test:
# BMI before and after a diet for 10 people
bmi_before <- c(30.1, 28.4, 26.7, 32.0, 29.5, 27.8, 31.2, 33.0, 25.9, 29.0)
bmi_after  <- c(28.9, 27.5, 25.9, 30.1, 28.8, 27.0, 30.3, 31.4, 25.1, 28.4)

# Put in a data.frame with an ID
diet_bmi <- data.frame(
  id         = 1:10,
  bmi_before = bmi_before,
  bmi_after  = bmi_after
)
diet_bmi

t.test(diet_bmi$bmi_before, diet_bmi$bmi_after, paired = TRUE)
#On average, BMI decreased by about 1.02 units (because before − after is positive).

#For comparison the non-paired t-test:
t.test(diet_bmi$bmi_before, diet_bmi$bmi_after, paired = FALSE)
#Same mean difference (as one would expect), but much higher p value because it "throws away" the information on the individual BMI change and only uses the mean differences across all participants instead.

#z test:
library(BSDA)
x <- c(7.8, 6.6, 6.5, 7.4, 7.3, 7., 6.4, 7.1, 6.7, 7.6, 6.8)
y <- c(4.5, 5.4, 6.1, 6.1, 5.4, 5., 4.1, 5.5)
z.test(x, sigma.x=0.5, y, sigma.y=0.5, conf.level=0.95)
t.test(x, y) #For comparison.
 # Two-sided standard two-sample z-test where both sigma.x
 # and sigma.y are both assumed to equal 0.5. The null hypothesis
 # is that the population mean for 'x' less that for 'y' is 2.
 # The alternative hypothesis is that this difference is not 2.
 # A confidence interval for the true difference will be computed.

#F-Test for equality of variances:
var.test(a, b) #Tests: H0: σ1² = σ2² vs H1: σ1² ≠ σ2²

#F test in linear regression to check if the model is better than the null model:
mylm <- lm(mpg ~ wt + hp + cyl, data=mtcars)
summary(mylm) #F-statistic: 50.17 on 3 and 28 DF,  p-value: 2.184e-11 -> Yes, the model is significantly better than the null model. Tests if all coefficients (for wt, hp, cyl) are 0 (H0) vs. if at least one of them is non-zero.
#Alternatively:
lm1 <- lm(mpg ~ wt + hp + cyl, data=mtcars)
lm2 <- lm(mpg ~ 1, data=mtcars) #intercept only
anova(lm1, lm2) #same p value

#Wilcoxon rank sum test:
numeric_vector <- c(20, 29, 24, 19, 20, 22, 28, 23, 19, 19)
wilcox.test(numeric_vector, mu=20, conf.int = TRUE)

x <- c(0.80, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46)
y <- c(1.15, 0.88, 0.90, 0.74, 1.21)
wilcox.test(x, y, alternative = c("two.sided"))
wilcox.test(x, y, alternative = c("less"))
wilcox.test(x, y, alternative = c("greater"))

#Another example:
wilcox.test(mpg ~ am, data=mtcars)
wilcox.test(mpg ~ am, data=mtcars, correct=FALSE)
wilcox.test(mpg ~ am, data=mtcars, conf.int=TRUE)
#Note: If there are any ties (identical values), no exact (only approximated) p values and CIs can be computed.
tmp <- mtcars[!duplicated(mtcars$mpg),] #Remove ties
wilcox.test(mpg ~ am, data=tmp)
wilcox.test(mpg ~ am, data=tmp, correct=FALSE)
wilcox.test(mpg ~ am, data=tmp, conf.int=TRUE)

#Wilcoxon signed-rank test (for PAIRED samples!):
wilcox.test(diet_bmi$bmi_before, diet_bmi$bmi_after, paired = TRUE) #Note how much smaller the p value is if you use the information that you have paired (before-after) data. Just like above for the t-test.

#Correlation test:
cor.test(mtcars$mpg, mtcars$hp)

#non-parametric (rank-)correlation test:
cor.test(mtcars$mpg, mtcars$hp, method = "spearman")

#Shapiro-Test for normal distribution:
normaly_disb <- rnorm(100, mean=5, sd=1)
shapiro.test(normaly_disb) #if p<0.05 -> not normally distributed

#Kolmogorov-Smirnov test is used to check whether 2 samples follow the same distribution
x <- rnorm(50)
y <- runif(50)
ks.test(x, y)

#Binomial test:
successes <- 5
trials <- 12
binom.test(successes, trials) #Tests if p is significantly different from 0.5 by default)
binom.test(successes, trials, p=0.6) #Tests if p is signif. different from 0.6 -> larger observed difference -> smaller p value.
binom.test(successes, trials, alternative="greater")
binom.test(successes, trials, alternative="less")
binom.test(successes, trials, conf.level=0.90)

#Median test:
require(coin) || (install.packages("coin") && (require(coin)))
median_test(mpg ~ factor(am), data = mtcars) #Note that the grouping variable needs to be a factor.

get_package <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    require(pkg, character.only = TRUE)
  }
}

get_package("RVAideMemoire")
mood.medtest(mpg ~ factor(am), data = mtcars)

#Kolmogorov-Smirnov test:
x <- rnorm(50)
y <- runif(30)
# Do x and y come from the same distribution?
ks.test(x, y)

#Levene test (for equality of variances):
get_package("car")

set.seed(123)

group <- factor(rep(c("A", "B", "C"), each = 30))

value <- c(
  rnorm(30, mean = 10, sd = 1),   # Group A, small variance
  rnorm(30, mean = 10, sd = 3),   # Group B, larger variance
  rnorm(30, mean = 10, sd = 5)    # Group C, even larger variance
)

df <- data.frame(group, value)
leveneTest(value ~ group, data = df)


#Further statistical tests are available in the package "lawstat".

#Required case number for a t-test (difference in means) at a pre-defined statistical power and significance and mean difference (effect size):
pwr.t.test(d=0.8, sig.level=0.05, power=0.9, type="two.sample", alternative="two.sided") #where d=(mean group 1 - mean group 2) / sqrt(pooled sampling variance)

#Required case number for a test of differences in proportions:
p1 <- 0.5 #z.B. Se. v.gFOBT f. CRC
p2 <- 0.7 #z.B. Se. v.FIT f. CRC
pwr.2p.test(h=ES.h(p1,p2), sig.level=0.05, power=0.8, alternative="two.sided")
