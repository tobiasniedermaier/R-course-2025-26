
rm(list=ls(all=TRUE))
library(pROC)
data(aSAH)
dat <- aSAH
women <- subset(dat, dat$gender=="Female")
men <- subset(dat, dat$gender=="Male")

roc_men_ndka <- roc(men$outcome, men$ndka, auc=T, percent=T)
plot.roc(roc_men_ndka, print.auc=T, main="Test", xaxs="i", yaxs="i")

# Now we generalize it in a function so that we don't need to repeat everything if we want to do the same for women:

pROC <- function(data,marker){
 temp <- roc(data$outcome, data[,marker], auc=T, percent=T)
 plot.roc(temp, print.auc=T, main=paste("ROC Curve of", marker), xaxs="i", yaxs="i")
}

roc_men_ndka <- pROC(men,"ndka")
roc_women_ndka <- pROC(women,"ndka")
roc_men_s100b <- pROC(men,"s100b")

