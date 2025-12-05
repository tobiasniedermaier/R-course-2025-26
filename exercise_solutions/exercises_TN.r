
rm(list=ls(all=TRUE))

# Exercise 1:

## Task 1

#1. Assign the number 2 to an object a.
a <- 2 #Or:
a = 2 #Or:
2 -> a

#2. Take the natural logarithm of a.
log(a)

#3. Assign a new object using sin <- a.
sin <- a

#4. What will be the output of sin(sin)? Try to predict the output before executing the code!
sin(sin)

#Just for fun:
sin(1)^2 + cos(1)^2 #Or: sin(1)**2 + cos(1)**2

#5. Assign a new variable b with the value TRUE.
b <- TRUE

#6. What will be the result of 1 + b? Why?
1 + b

#7. What will be the result of sqrt(b)?
sqrt(b)

#8. What is the value of b+b?
b + b

#9. Assign a new variable c with the value '1'.
c <- 1

#10. What will be the result of 1 + c? Why?
1 + c

#But:
c <- "1"
1 + c
1 + (as.numeric(c))
c <- as.numeric(c)
1 + c

## Task 2

#1. Assign a new object v that is a vector with the elements c(2, 4, 5, 6, 4, -1).
v <- c(2, 4, 5, 6, 4, -1) #or easier:
-v

#2. Add 1 to each element of the vector?
v + 1 #recycling rule!

#3. Swap the sign of the vector.
v*(-1)

#4. Get the second element of the vector.
v[2]

#5. Get all elements of the vector except the last one.
v[-6]
v[-length(v)] #or:
v[1:(length(v)-1)]

#6. How many values of 4 does the vector contain?
length(v[v==4]) #or:
sum(v==4)

#7. Swap the sign of all negative values.
abs(v) #or:
v[v < 0] <- -v[v < 0]

## Task 3

#1. Define a vector f1 containing 5 arbitrary elements of the type character.
f1 <- sample(LETTERS, 5) #short for: sample(x=LETTERS, size=5, replace=FALSE)

#2. Define a vector f2 containing 5 arbitrary elements of the type factor.
f2 <- factor(sample(LETTERS, 5))

#3. Define a vector f3 containing 5 other arbitrary elements of the type numeric.
f3 <- c(4, 9, 8.6, 7, -6.25)

#4. Create a list L containing the vectors f1, f2, f3.
mylist <- list(f1, f2, f3)

#5. Look at the structure of the list.
str(mylist)
mylist[3]
mylist[3] +1
mylist[[3]]
mylist[[3]] +1

#6. Create a data.frame df1 using L. Look at the structure again.
df1 <- data.frame(mylist)
colnames(df1) <- c("letters", "factors", "numbers")
str(df1)

#7. What are the element on the second row?
df1[2,]

#8. What are the element on the second column?
df1[,2]

#9. What are the values between the 2nd and the 4th rows?
df1[2:4,] #2:4 equals c(2, 3, 4) equals seq(from=2, to=4, by=1).

#10. Save the data set as a csv. Note: Use row.names = FALSE as additional argument!
write.csv2(df1, file="mydf.csv", row.names=FALSE)
getwd()

#11. Load the data set into R as a new object df2 and compare it with the original one df1. What is the problem here?
df2 <- read.csv2("mydf.csv")

identical(df1, df2)
str(df1)
str(df2)

df2 <- read.csv2("mydf.csv", stringsAsFactors=TRUE)
df2[,1] <- as.character(df2[,1]) #[,1] selects the first column and all rows of the data frame df2.

df1 == df2
all.equal(df1, df2)
#I don't know what the problem is. Formerly: "The second variable is not a factor!" Might have changed with newer R version.


# Exercise 2:

## Task 1

#1. Define a vector x containing 100 values between 0 and 10.
x <- runif(n=100, min=0, max=10) #runif creates n (here: 100) uniformly distributed values.
#If equally-spaced (was not specified!): 
x <- seq(0, 10, length.out = 100)

#2. Define a second variable y using sin(x).
y <- sin(x)

#3. Plot both variables in a scatter plot with the according x and y axis. Use the additional argument pch = 20. What does it do?
plot(x, y, pch=20)

#4. Do the same, but swap the axis now. However, the axis name should stay the same as before.
plot(y, x, pch=20, xlab = "x", ylab = "y")

#5. Now, do the same as a line plot. Add a title. Add the additional argument lty = 2.
x <- sort(x)
y <- sin(x)
plot(y, x, type="l", lty=2, main = "Sinus function")

#6. Combine a scatter and a line plot: make a scatter plot and use the lines() function to add a line.
plot(x,y) #Scatter plot
lines(c(0:10), rep(0,11)) #rep(0,11) repeats the number 0 11 times.

#7. Create a new variable x <- c(1:10, 1:20, 1:30, 1:40, 1:50). Plot a histogram using 5 breaks.
x <- c(1:10, 1:20, 1:30, 1:40, 1:50)
hist(x, breaks=5)

#8. Add a line to the histogram with x-axis values 1:50 and y-axis values 50:1. Use the additional argument lwd = 2.
lines(x=1:50, y=50:1, lwd=2)

#9. Define a data set using the command df <- data.frame(x = c(rnorm(100), rexp(100)), group = rep(1:2, each = 100)).
df <- data.frame(x = c(rnorm(100), rexp(100)), group = rep(1:2, each = 100))

#10. Make a Boxplot of the variable x.
boxplot(df$x)

#11. Make a Boxplot for both variables.
boxplot(x ~ group, data=df)

# Exercise 3:

## Task 1

#1. Load the data set Orange that contains only three variables.
dat <- Orange #already included in R

# Farben für jede Tree-Gruppe definieren
tree_colors <- as.numeric(Orange$Tree)
palette_colors <- rainbow(length(unique(Orange$Tree)))
palette_colors #Or:
Orange$Tree |> unique() |> length() |> rainbow()

#2. Plot the two variables age and circumference against each other using a scatter plot
#3. Add color as an additional aesthetic mapping for the variable Tree.

# Plot erstellen
plot(Orange$age, Orange$circumference,
     col = palette_colors[tree_colors],
     pch = 16,
     xlab = "Age",
     ylab = "Circumference",
     main = "Orange Tree Growth")

# Legende hinzufügen
legend("topleft", legend = levels(Orange$Tree),
     col = palette_colors, pch = 16, title = "Tree")


#Or with ggplot2:
library(ggplot2) #If you want to make sure that a package is loaded if installed, or installed and then loaded if it was not yet installed, use the following codes:
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
library(ggplot2)
ggplot(Orange, aes(age, circumference, color = Tree)) + geom_point()


## Exercise 3, Task 2:

#1. Now plot them as lines and not as points using geom_line
ggplot(Orange, aes(age, circumference, color = Tree)) + geom_line()

ggplot(Orange, aes(age, circumference, color = Tree)) + geom_line() + theme_bw()
ggplot(Orange, aes(age, circumference, color = Tree)) + geom_line() + theme_minimal()
ggplot(Orange, aes(age, circumference, color = Tree)) + geom_line() + theme_classic()

ggplot(Orange, aes(age, circumference, color = Tree)) +
  geom_point()

#2. Generate 2 objects: One plot using points and one using lines. Plot the two objects in one plot
if (!requireNamespace("cowplot", quietly = TRUE)) install.packages("cowplot")
library(cowplot)
g1 <- ggplot(Orange, aes(age, circumference, color = Tree))+
  geom_point()
g2 <- ggplot(Orange, aes(age, circumference, color = Tree))+
  geom_line()

plot_grid(g1,g2)

#3. Add another layer to your plot: + facet_wrap(~Tree). What does the function do?
ggplot(Orange, aes(age, circumference, color = Tree))+
  geom_point()+
  geom_line()+
  facet_wrap(~Tree)
  
layout(matrix(1:6, nrow=2, byrow=TRUE))
layout.show(n=6) #See how (in which order) the matrix grid will be filled with plots.
plot(Orange[Orange$Tree==1,]$circumference, main="1", type="b", ylim=c(0,250))
plot(Orange[Orange$Tree==2,]$circumference, main="2", type="b", ylim=c(0,250))
plot(Orange[Orange$Tree==3,]$circumference, main="3", type="b", ylim=c(0,250))
plot(Orange[Orange$Tree==4,]$circumference, main="4", type="b", ylim=c(0,250))
plot(Orange[Orange$Tree==5,]$circumference, main="5", type="b", ylim=c(0,250))
dev.off()

layout(matrix(1:6, nrow=2, byrow=TRUE))
for(i in 1:5){
  plot(Orange[Orange$Tree==i,]$circumference, main=i, type="b", ylim=c(0,250))
}


## Exercise 3, Task 3:

library(datasets)
#1. In this task we use the data set ChickWeight. Load the data set from the package datasets.
data(ChickWeight)
dat <- ChickWeight
#2. Plot the distribution of the variable weight using a histogram.
hist(dat$weight, main="Histogram for weight", xlab="Weight in ...[unit]")
#3. Change the data type of the variable Time to a factor.
dat$Time <- factor(dat$Time)
#4. Plot a boxplot for each time point.
boxplot(dat$weight ~ dat$Time)
#5. Plot the weight as boxplots for the different types of Diet.
boxplot(dat$weight ~ dat$Diet)
#6. Do the same, as before, but now combine the three variables: Use Time on the x-axis, weight on the y-axis and use Diet as a fill argument.
cols <- c("skyblue", "tomato", "palegreen3", "gold")  # eine Farbe je Diet
boxplot(weight ~ Time * Diet, data = dat, col = rep(cols, times = nlevels(dat$Time)), xlab = "Time", ylab = "Weight", las = 2)
legend("topleft", title = "Diet", fill = cols, legend = levels(dat$Diet), bty = "n")

#Or with ggplot2:
ggplot(dat, aes(Time, weight, fill = Diet)) + geom_boxplot()

#Add color as an additional aesthetic mapping for the variable Tree:
ggplot(Orange, aes(age, circumference, color = Tree)) + geom_point()

#Or with lattice:
library(lattice)

xyplot(circumference ~ age,
       data   = Orange,
       groups = Tree,                  
       pch    = 16,                    
       cex    = 0.8,
       auto.key = list(points = TRUE,  
                       lines  = FALSE,
                       title  = "Tree",
                       space  = "right"), #Or: "left", "top", "bottom". 
       xlab = "Age",
       ylab = "Circumference")


# Exercise 4:

# Today we will start to analyze the NHANES data from your R Data Project. Choose one sub-sample to work with. There are 5 data sets. You may also want to look at the documentation of the data.

## Task 1 (General):

#1. Load the data in R.
setwd("S:\\AG_Holle\\TN\\3_Lehre\\R-Kurs 2025_26")
dat <- read.csv2("NHANES1.csv") #Or interactively: read.csv2(file.choose())

#2. What is the dimension of the data set? How many rows (samples), and how many columns (variables) does the data set contain? What are the variable names of the data set?
dim(dat)
nrow(dat)
ncol(dat)
colnames(dat) #Or: names(dat)

#3. All the variables in the data set are either of a class integer, numeric or boolean (i.e., logical). However, some of the variables should be factors rather than numerical variables. Change the class of these variables to factor.
lapply(head(dat), table) #Or:
str(dat)

dat$srhgnrl <- as.factor(dat$srhgnrl)
dat$srphbad_prv30d <- as.factor(dat$srphbad_prv30d)
dat$srmhbad_prv30d <- as.factor(dat$srmhbad_prv30d)
dat$adlimp_prv30d <- as.factor(dat$adlimp_prv30d)
dat$educ <- as.factor(dat$educ)
dat$martlst <- as.factor(dat$martlst)
dat$ethnic <- as.factor(dat$ethnic)
dat$increl <- as.factor(dat$increl)
dat$diab_lft <- as.factor(dat$diab_lft)
dat$jobstat_lwk <- as.factor(dat$jobstat_lwk)
dat$alc_lft <- as.factor(dat$alc_lft)
dat$smokstat <- as.factor(dat$smokstat)
dat$milk_month <- as.factor(dat$milk_month)

#Or:
library(Hmisc)
vars <- Cs(hivpos, srhgnrl, srphbad_prv30d, srmhbad_prv30d, adlimp_prv30d, educ, martlst, male, ethnic, increl, asthma_ever, asthma_now, ovrwght_ever, arthrit_ever, stroke_ever, livdis_ever, cbronch_now, livdis_now, cancer_ever, rel_heartdis, rel_asthma, rel_diab, heartdis_ever, lungpath_ever, diab_lft, jobstat_lwk, wrkt_irreg, workpollut, sleep_probl, cannab_ever, harddrg_ever, smokstat, age.cat, smoke_bin, bmi_cat, age_bin)

summary(dat[,vars])

dat[,vars] <- lapply(dat[,vars], as.factor)

#4. How many women and how many men are there in your data set?
table(dat$male, exclude=NULL)
#Achtung: male ist genau verkehrt herum definiert! Richtig herum labeln:
dat$sex <- factor(dat$male, levels=c(FALSE,TRUE), labels=c("female","male"))

#5. What is the mean BMI in the overall population? What is the mean BMI for men and women?
mean(dat$bmi, na.rm=TRUE)
tapply(dat$bmi, dat$sex, mean, na.rm=T)
aggregate(bmi ~ sex, data=dat, FUN=mean)
mean(dat[dat$sex=="male",]$bmi, na.rm=TRUE)
mean(dat[dat$sex=="female",]$bmi, na.rm=TRUE)

#6. Who has an higher mercury level in blood: men or women? People with chronic bronchitis or people without it? ‘Hispanic’, ‘White’, ‘Black’ or ‘Other/Mixed’ people?
tapply(dat$hg, dat$sex, summary, na.rm=T)
tapply(dat$hg, dat$cbronch_now, summary, na.rm=T)
tapply(dat$hg, dat$ethnic, summary, na.rm=T)
tapply(dat$hg, dat$ethnic, mean, na.rm=T)

t.test(hg ~ sex, data=dat)
t.test(hg ~ cbronch_now, data=dat)
kruskal.test(hg ~ ethnic, data=dat) #or:
oneway.test(hg ~ ethnic, data=dat)

#Or:
mean(dat$hg[dat$sex=="male"], na.rm=T) #etc.

#7. Use the function summary to get the summarized information on all the variables in the data set.
summary(dat)

## Exercise 4, Task 2 (Visualization):

#1. Plot the variable rr_sys as a function of bmi.
plot(dat$bmi, dat$rr_sys)

#2. Now we want to plot the variable rr_sys against diab_lft. Which plot should we use here?
boxplot(dat$rr_sys ~ dat$diab_lft)
is.factor(dat$diab_lft) #TRUE
dat$diab_lft <- factor(dat$diab_lft, levels = c(0,1,2), labels = c("None","Prediabetes","Diabetes"))
boxplot(dat$rr_sys ~ dat$diab_lft)
boxplot(rr_sys ~ diab_lft, data=dat)

ggplot(dat, aes(diab_lft, rr_sys))+
  geom_boxplot()

#3. Plot the BMI against educ and give a short interpretation.
boxplot(dat$bmi ~ dat$educ)

ggplot(dat, aes(educ, bmi))+
  geom_boxplot()
dev.off()

#4. Plot the histogram of the high-density lipoprotein (HDL) cholesterol levels. How does the distribution of HDL look like?
hist(dat$hdl, breaks=50)
ggplot(dat, aes(hdl)) + geom_histogram(bins=50)

#5. Can you convert the variable HDL so that its distribution looks more normal? Create such a variable and add it to your data set.
hist(log(dat$hdl), breaks=15)
dat$log_hdl <- log(dat$hdl)
hist(dat$log_hdl, breaks=15)

## Exercise 4, Task 3 (Confidence intervals):

#1. Is the variable height normally distributed in males and females? Check this by analyzing graphically the two sub-samples. Use qqplots: qqnorm() and qqline().
hist(dat[dat$sex=="female",]$height, xlab="Height in cm", main="Females")
hist(dat[dat$sex=="male",]$height, xlab="Height in cm", main="Males")

qqnorm(dat[dat$sex=="female",]$height)
qqline(dat[dat$sex=="female",]$height)

qqnorm(dat[dat$sex=="male",]$height)
qqline(dat[dat$sex=="male",]$height)

#2. Suppose that the value of the variance for the male height is known and equal to 64. Construct a 95% confidence interval for the mean.
mean(dat[dat$sex=="male",]$height, na.rm=T)
mean(dat[dat$sex=="male",]$height, na.rm=T) -1.96*(sqrt(64) / sqrt(sum(!is.na(dat$height[dat$sex=="male"]))))
mean(dat[dat$sex=="male",]$height, na.rm=T) +1.96*(sqrt(64) / sqrt(sum(!is.na(dat$height[dat$sex=="male"]))))

#3. Repeat the procedure for the female population, with unknown variance and using a confidence level of 95%.
mean(dat[dat$sex=="female",]$height, na.rm=T)
low <- mean(dat$height[dat$sex=="female"],na.rm=T) - qnorm(0.975)*sqrt(64/(length(dat$height[dat$sex])-sum(is.na(dat$height[dat$sex]))))
up <- mean(dat$height[dat$sex=="female"],na.rm=T) + qnorm(0.975)*sqrt(64/(length(dat$height[dat$sex])-sum(is.na(dat$height[dat$sex]))))
c(low,up)
#CI is so narrow because we have 5000 people in the data set.
#In fact, because we aassume unknown variance, the following would be more adequate (and is easier):
t.test(dat$height[dat$sex == "female"], conf.level = 0.95)$conf.int

#Repeat the procedure for the female population, with unknown variance and using a confidence level of 90%
females <- dat[dat$sex=="female",,]
males <- dat[dat$sex=="male",]

DescTools::MeanCI(x=females$height, conf.level=0.90, sides="two.sided", na.rm=T)
DescTools::MeanCI(x=males$height, conf.level=0.90, sides="two.sided", na.rm=T)


## Exercise 4, Task 4 (Tests):

#4.1 Is the variance the same in both populations? Perform an appropriate test. What is the null hypothesis?
var.test(females$height, males$height)

#4.2 Which test can we use, if, instead, we want to check if males are on average taller than females? Set an adequate alternative hypothesis.
t.test(dat[dat$sex=="male",]$height, dat[dat$sex=="female",]$height, alternative='greater') #or:
t.test(height ~ sex, data=dat, alternative='less')

#4.3 Analyze the confidence interval obtained in the previous point.
ci <- t.test(dat$height[dat$sex == "male"], dat$height[dat$sex == "female"], alternative = "greater")
ci$conf.int
#Why doesn't it have an upper bound?
#-> Obere Grenze fehlt bei einseitigem KI per Definition.

#4.4 Now look at the distribution of the variable weight: can we graphically state its normality? Perform a transformation in order to recover it.
hist(dat$weight, breaks=50)
qqnorm(dat$weight)
qqline(dat$weight)

hist(log(dat$weight), breaks=50)
qqnorm(log(dat$weight))
qqline(log(dat$weight))

#4.5 Test if the mean of the variable weight is 80 kg, testing H0: log(weight) = log(80) vs. H1: log(weight) != log(80). Can you state an interval estimate with a level of 0.99?

t.test(log(dat$weight), mu=log(80), alternative="two.sided")
#Can you state an interval estimate with a level of 0.99?
t.test(log(dat$weight), mu=log(80), alternative="two.sided", conf.level=0.99)

#4.6 Provide a punctual and an interval estimate for the prevalence of heart diseases and lung pathology.

# HEART DISEASE
y_hd <- factor(dat$heartdis_ever, levels = c(FALSE, TRUE), labels = c("no","yes"))
tab_hd <- table(y_hd)
prop.test(x = unname(tab_hd["yes"]), n = sum(tab_hd))
# optional:
binom.test(unname(tab_hd["yes"]), sum(tab_hd))

# LUNG PATHOLOGY
y_lp <- factor(dat$lungpath_ever, levels = c(FALSE, TRUE), labels = c("no","yes"))
tab_lp <- table(y_lp)
prop.test(x = unname(tab_lp["yes"]), n = sum(tab_lp))
x <- unname(tab_lp["yes"])
n <- sum(tab_lp)

#Oder: (Einfacher, kein Paket nötig + gibt auch p-Werte aus.)
binom.test(x, n)

#4.7 Test if the prevalence [of heartdis_ever] is statistically different between men and women
table(dat[,c("heartdis_ever","sex")])
chisq.test(table(dat[,c("heartdis_ever","sex")]))
fisher.test(table(dat[,c("heartdis_ever","sex")]))
prop.test(table(dat[,c("heartdis_ever","sex")]))
prop.test(table(dat[,c("sex","heartdis_ever")]))
#Ergibt alles dieselben p-Werte.
prop.test(table(dat$sex, dat$heartdis_ever)) #alternative Syntax.

#4.8 Turn the variable smokstat into a binary variable (put the non-smokers and the people who almost never smoked into one group).
dat$smokebin <- ifelse(dat$smokstat %in% 1:2, 0, 1)
dat[is.na(dat$smokstat),"smokebin"] <- NA
table(dat$smokebin, exclude=NULL)
dat$smokebin <- as.factor(dat$smokebin)
#Just to demonstrate how to dichotomize a continuous variable. The variable smoke_bin already exists in the data set and is (effectively) identical to smokebin!

#4.9 Do you expect smokers to have a higher cancer prevalence than non-smokers? Test if there is a significant difference in cancer prevalence between smokers and non-smokers (using the variable from before). If so, which group has a higher prevalence? Did you get the result you expected?
prop.table(table(dat[,c("cancer_ever","smokebin")]), margin=2)
dat$cancer_ever <- factor(dat$cancer_ever, levels=c(TRUE, FALSE))
prop.test(table(dat$smoke_bin, dat$cancer_ever)) #Ist aus irgendeinem Grund "verdreht", d.h. Prävalenz vermeintlich 86.3% vs. 94.1% und nicht 13.7% und 5.9%. Keine Ahnung, warum.
#-> Strangely, the cancer prevalence is higher among non-smokers than among smokers. Possible explanations:
#1.) Confounding by age!
#2.) Competing risks: A smoker who dies early from CVD cannot get cancer anymore.
#3.) Accelerated cancer death: Smoking is an adverse factor regarding prognosis. Given that both a smoker and a non-smoker get the same cancer, the smoker is expected to die faster. Thus, there are fewer prevalent cancer cases. Incidence would be a better measure than prevalence.
#4.) "Cancer" is very heterogeneous. Some develop slowly and are not very deadly (e.g. most skin cancer types or thyroid cancer), others develop very fast and kill fast (e.g. lung cancer, esp. small-cell lung cancer). Maybe the types of cancers among the smokers tend to be more of the fast-developing and deadly type.

#4.10 Do the same test in the subgroup of people who are between 20 and 49 years old. What do you see now? How can you explain the results?

prop.test(table(dat[dat$age<50,]$smoke_bin, dat[dat$age<50,]$cancer_ever))
#Note: Instead of dat[dat$age<50,]$age, one could also write dat$age[dat$age<50]. Slightly shorter and less error prone, because you don't need the comma at the end.
#Or:
subset(dat, age<50)
#Or:
library(data.table)
dt <- data.table(dat)
dt[age<50]

## Exercise 4, Task 5 (Discrete and non-parametric tests):

#5.1 Use a chi-square test in order to test whether the presence of chronic bronchitis and the current smoking status are independent.
tab_smo_bron <-  table(dat[,c("smokstat","cbronch_now")])
chisq.test(tab_smo_bron)

#5.2 Use a Fisher test to verify the independence between sex and the presence of any kind of liver disease.
fisher.test(table(dat[,c("sex","livdis_now")])) #or:
fisher.test(dat$sex, dat$livdis_now)

#5.3 Perform a sign test both on hdl and on log-hdl to test the hypothesis that the median of the cholesterol level is 1.30. Is the median significantly different from 1.30? Do you obtain the same results using hdl and logHdl?

library(BSDA)
SIGN.test(dat$hdl, md = 1.30)
SIGN.test(log(dat$hdl), md = log(1.30))

#5.4 Use a Mann-Whitney test to test the null hypothesis 𝐻0 ∶ 𝑚𝑎𝑙𝑒𝑤𝑒𝑖𝑔ℎ𝑡 = 𝑓𝑒𝑚𝑎𝑙𝑒𝑤𝑒𝑖𝑔ℎ𝑡.
wilcox.test(weight ~ sex, data=dat) #or:
coin::wilcox_test(weight ~ as.factor(sex), data = dat) #or:
library(coin)
wilcox_test(weight ~ as.factor(sex), data = dat)

#5.5 It has been shown that there is a ‘social gradient’ in health such that the richer you are, the more likely you are to have better health. Plot general self-rated health against relative income so that you can get an impression whether this is confirmed by our data. Which kind of plot is reasonable? Consider using a mosaic plot. E.g. function mosaicplot().

mosaicplot(srhgnrl ~ increl, main = 'self-rated health vs. relative income', xlab = 'self-rated health', ylab = 'relative income', data = dat) #or probably nicer / easier to grasp the message:
plot(dat[,c("increl","srhgnrl")])
dev.off()

#5.6 Test the relation for statistical significance using an appropriate test.
chisq.test(dat$srhgnrl, dat$increl) #Both variables are categorical (nominal), so we cannot use t.test or other tests that assume metric or ordinal data.

#5.7 Categorize the variable bmi into an underweight (BMI<18.5), normal weight (18.5 <= BMI < 25), overweight (25 <= BMI < 30) and obese (BMI >= 30) group. Turn the variable into a factor. You may use the function cut().

#5.8 What is the proportion of overweight or obese people according to the categorized BMI? What is the proportion of people ever diagnosed with being overweight (variable ovrwght_ever)? How many overweight people were actually ever diagnosed with being overweight?
prop.table(table(dat$bmi_cat))

bmi_cat <- NULL
bmi_cat[dat$bmi<18.5] <- "Underweight"
bmi_cat[dat$bmi>=18.5&dat$bmi<25] <- "Normal weight"
bmi_cat[dat$bmi>=25&dat$bmi<30] <- "Overweight"
bmi_cat[dat$bmi>=30] <- "Obese"
bmi_cat <- as.factor(bmi_cat)
dat$bmi_cat <- bmi_cat
prop.table(table(dat$ovrwght_ever))

#Or:
dat$bmi_cat2 <- ifelse(dat$bmi <18.5, "Underweight", 
  ifelse(dat$bmi >=18.5 & dat$bmi <25, "Normal weight",
  ifelse(dat$bmi >=25 & dat$bmi <30, "Overweight",
  "Obese")))
table(dat$bmi_cat, dat$bmi_cat2) #identical

#Or:
dat$bmi_cat3 <- cut(dat$bmi, breaks=c(0, 18.5, 25, 30, 100), right=FALSE)
table(dat$bmi_cat, dat$bmi_cat3) #identical

#How many overweight people were actually ever diagnosed with being overweight?
prop.table(table(dat[dat$bmi_cat2=="Overweight",]$ovrwght_ever)) #Or:
prop.table(table(dat$ovrwght_ever, dat$bmi_cat), margin = 2)

#5.9 Is there a difference in diabetes prevalence between obese people diagnosed with overweight and those who were never diagnosed? What about self-rated health? How do you explain the results?

obese <- dat[dat$bmi_cat=="Obese",]
tapply(obese$diab_lft, obese$ovrwght_ever, summary)
tapply(obese$diab_lft, obese$ovrwght_ever, function(x) (table(x)))
tapply(obese$diab_lft, obese$ovrwght_ever, function(x) prop.table(table(x)))
mytab <- table(obese[,c("diab_lft","ovrwght_ever")])
fisher.test(mytab)

#Self-rated health:
mytab <- table(obese[,c("srhgnrl","ovrwght_ever")])
prop.table(mytab, margin=1)
chisq.test(mytab)


# Exercise 5:

#Task 1 (linear regression)

#1. How strong is the relationship between BMI and systolic blood pressure?
cor.test(dat$rr_sys, dat$bmi)
cor.test(dat$bmi, dat$rr_sys)
summary(lm(rr_sys ~ bmi, data=dat))

#2. create a binary age ( younger: 𝑎𝑔𝑒 ≤ 50 and older: 𝑎𝑔𝑒 > 50) and a binary smoking variable
dat$age_dicho <- cut(dat$age, c(0,50,100), right = T) #Achtung: right=F (s. Raphael) ist FALSCH! #or:
dat$age_dicho2 <- ifelse(dat$age <=50, "<=50", ">50")

#Does the relationship between systolic blood pressure and BMI change when you adjust for age (categorized)?
summary(lm(rr_sys ~ bmi + age_dicho, data = dat))
#Interpret the coeﬀ icients of the resulting model.
#...

#Would you say that BMI has a clinically relevant impact on blood pressure, according to your model?
#...

#3. Try to find a better model to predict systolic blood pressure by including more covariates. Select a number of candidate covariates which in your opinion may be related to systolic blood pressure, and then choose a model selection strategy and a criterion/test for comparing models. Describe the model with the best fit according to your search, and interpret the model coefficients.

sub_dat <- dat[ ,c('rr_sys', 'bmi', 'sex', 'age_dicho', 'diab_lft', 'heartdis_ever', 'alc_lft', 'smokstat')]
complete.data <- sub_dat[complete.cases(sub_dat), ]  # complete.cases deletes all rows with at least one NA
complete.data$bmi <- complete.data$bmi - mean(complete.data$bmi) #Mean centering. Makes the intercept interpretable.

# full model
mod1 <- lm(rr_sys ~ bmi + sex + age_dicho + diab_lft + heartdis_ever + alc_lft + smokstat, data=complete.data)
summary(mod1)

# stepwise selection
library(MASS)
mod2 <- stepAIC(mod1) #default for direction is "backward"
mod3 <- stepAIC(mod1, direction="backward") #same!
mod4 <- stepAIC(mod1, direction="forward")

summary(mod2)
summary(mod4)

#Task 2 (logistic regression)

# 1. Analyze the relationship between lifetime diagnosis of cancer and exposure to pollutants, using the categorized age variable (Note: No information on pollutant exposure was collected from participants aged 80+, so these cannot be included in the analysis). Does the adjustment for age change the picture? Interpret the model coeﬀ icients including the intercept.

dat$age_cat <- cut(dat$age, c(0, 18, 35, 50, 65, 80, 100))

mylogreg1 <- glm(cancer_ever ~ workpollut, data=dat, family="binomial")
summary(mylogreg1)

mylogreg2 <- glm(cancer_ever ~ workpollut + age_cat, data=dat, family="binomial")
summary(mylogreg2)

# 2.  Try to find a good model of cancer diagnosis, describe and interpret it as you did for systolic blood pressure.

sub.data <- dat[ ,c('cancer_ever', 'cd', 'pb', 'hg', 'drnkprd_prv12mo', 'cigsprd_prv30d', 'bmi', 'sex', 'age.cat', 'diab_lft', 'rdyfood_prvmo')]
complete.data <- sub.data[complete.cases(sub.data), ]
mod3 <- glm(cancer_ever ~ cd + pb + hg + drnkprd_prv12mo + cigsprd_prv30d + bmi + sex + age.cat + as.factor(diab_lft) + rdyfood_prvmo, family = binomial(logit), data = complete.data)
mod4 <- stepAIC(mod3)
summary(mod4)
# explanation
# only intercept
mod5 <- glm(cancer_ever ~ 1, data = dat, family=binomial)
summary(mod5)
(tab <- table(dat$cancer_ever))
tab["TRUE"]/(tab["TRUE"] + tab["FALSE"])
exp(mod5$coefficients["(Intercept)"])/(1 + exp(mod5$coefficients["(Intercept)"]))
# with a predictor
mod6 <- glm(cancer_ever ~ workpollut, data = dat, family = binomial(logit))
summary(mod6)
(tab <- table(dat$workpollut, dat$cancer_ever))
#For people not exposed to work pollution
p.ne <- tab["FALSE","TRUE"]/(tab["FALSE","TRUE"] + tab["FALSE","FALSE"])
exp(mod6$coefficients["(Intercept)"])/(1+exp(mod6$coefficients["(Intercept)"]))
#For people exposed to work pollution
p.e <- tab["TRUE","TRUE"]/(tab["TRUE","TRUE"] + tab["TRUE","FALSE"])
exp(mod6$coefficients["(Intercept)"] + mod6$coefficients["workpollutTRUE"])/(1+exp(mod6$coefficients["(Intercept)"]+mod6$coefficients["workpollutTRUE"]))
# log-odd baseline (no workpollution)
log(p.ne/(1-p.ne))
mod6$coefficients["(Intercept)"]
# log-odds workpollution
log(p.e/(1-p.e))
mod6$coefficients["(Intercept)"] + mod6$coefficients["workpollutTRUE"]
# log odds-ratio
log((p.e/(1-p.e))/(p.ne/(1-p.ne)))
summary(mod6)
