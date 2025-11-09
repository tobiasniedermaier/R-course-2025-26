
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

##Fisher's exact test:
fisher.test(mytable)

##LR, Pearson, Phi, contingency coeff., Cramer's V:
assocstats(mytable) #from the vcd package (visualizing categorical data)
summary(assocstats(mytable)) #etwas ausführlicher

##One-sample t test: (example taken from http://r-statistics.co/Statistical-Tests-in-R.html)
set.seed(100)
x <- rnorm(50, mean = 10, sd = 0.5)
t.test(x, mu=10)

##Two-sample t-Test (Welch test):
t.test(1:10, y = c(7:20))

##Paired t-Test:
#Time in seconds before training (a) and after training (b) for each athlete:
a = c(12.9, 13.5, 12.8, 15.6, 17.2, 19.2, 12.6, 15.3, 14.4, 11.3)
b = c(12.7, 13.6, 12.0, 15.2, 16.8, 20.0, 12.0, 15.9, 16.0, 11.1)

t.test(a,b, paired=TRUE)

#F-Test for equality of variances:
var.test(a,b)

#Wilcoxon rank sum test:
numeric_vector <- c(20, 29, 24, 19, 20, 22, 28, 23, 19, 19)
wilcox.test(numeric_vector, mu=20, conf.int = TRUE)

x <- c(0.80, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46)
y <- c(1.15, 0.88, 0.90, 0.74, 1.21)
wilcox.test(x, y, alternative = c("two.sided"))
wilcox.test(x, y, alternative = c("less"))
wilcox.test(x, y, alternative = c("greater"))

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

#Further statistical tests are available in the package "lawstat".


#Required case number for a t-test (difference in means) at a pre-defined statistical power and significance and mean difference (effect size):
pwr.t.test(d=0.8, sig.level=0.05, power=0.9, type="two.sample", alternative="two.sided") #where d=(mean group 1 - mean group 2) / sqrt(pooled sampling variance)

#Required case number for a test of differences in proportions:
p1 <- 0.5 #z.B. Se. v.gFOBT f. CRC
p2 <- 0.7 #z.B. Se. v.FIT f. CRC
pwr.2p.test(h=ES.h(p1,p2), sig.level=0.05, power=0.8, alternative="two.sided")



