
library(mada) #This package is used to perform a bivariate meta-analysis of sensitivity and specificity of diagnostic tests. In other words, sensitivity and specificity are pooled simultaneously to take their negative correlation into account.

data(AuditC) #Loads a sample data set. Note that the data needs to have the colums TP, FN, FP, TN with the case numbers of true-positives, false-negatives, false-positives, true-negatives for every study.
dat <- AuditC #Let's assign the built-in data set to an object "dat".

# Descriptives:
madad(dat)
madad(dat, correction=FALSE)
madad(dat, correction=0.001)
madad(dat, correction=0.001, method="clopper-pearson")
crosshair(dat)
crosshair(dat, correction=0.001, method="clopper-pearson", pch=3, ylim=c(0.2, 1), yaxs="i", xaxs="i", col=1:nrow(dat))
forest(madad(dat, correction=FALSE), main="Forest plot of diagnostic accuracy studies\ncreated with the mada package", snames=LETTERS[1:14])

summary(reitsma(dat)) #reitsma() performs the meta-analysis, summary(reitsma()) provides a more informative output. However, it is a bit inconvenient if the pooled sensitivity and false-positive rate (= 1-specificity) are all you want.
summary(reitsma(dat))$coefficients #This extracts the coefficients (Se+ 1-Sp) from the output.
fit_reitsma <- reitsma(dat)
b <- summary(fit_reitsma)
c <- b$coefficients
c #The same "broken down".

plot(fit_reitsma, main="SROC curve (bivariate model) for AUDIT−C data")
points(fpr(dat), sens(dat), pch=2)
legend("bottomright", c("data", "summary estimate"), pch = c(2,1))
legend("bottomleft", c("SROC", "conf. region"), lwd = c(2,1))
dev.off()

#Leave out one study at a time and perform the meta-analysis again, using a for-loop:
for(i in 1:nrow(dat)){
  print(paste("Leave out study", i))
  print(summary(reitsma(dat[-i,]))$coefficients)
  cat("\n")
}

#Leave out one study at a time and perform the meta-analysis again, using a while-loop:
i <- 1
while(i <= nrow(dat)){
  print(paste("Leave out study", i))
  print(summary(reitsma(dat[-i,]))$coefficients)
  cat("\n")
  i <- i+1
}

