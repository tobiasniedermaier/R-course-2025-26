
rm(list=ls(all=TRUE))
library(pROC)
data(aSAH)
dat <- aSAH
women <- subset(dat, dat$gender=="Female")
men <- subset(dat, dat$gender=="Male")

roc_men_ndka <- roc(men$outcome, men$ndka, auc=TRUE, percent=TRUE)
plot.roc(roc_men_ndka, print.auc=TRUE, main="Test", xaxs="i", yaxs="i")

# und das will ich jetzt verallgemeinern, sodass ich es nicht wiederholen muss, wenn ich dasselbe für Frauen mache

pROC <- function(data,marker){
 temp <- roc(data$outcome, data[,marker], auc=T, percent=T)
 plot.roc(temp, print.auc=T, main=paste("ROC Curve of", marker), xaxs="i", yaxs="i")
}

roc_men_ndka <- pROC(men, "ndka")
roc_women_ndka <- pROC(women, "ndka")
roc_men_s100b <- pROC(men, "s100b")
