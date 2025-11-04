
library(binom) #for exact confidence intervals of proportions
bin <- function(x,y, methods="exact", ...){binom.confint(x,y, methods=methods, ...)}
specify_decimal <- function(x, k=1){as.numeric(noquote(trimws(format(round(x, k), nsmall=k))))}
binom <- function(x,y, n=3, methods="exact", ...){specify_decimal(bin(x,y, methods=methods, ...)[,c("mean", "lower", "upper")],n)*100}
library(pROC) #just to get the sample data set "aSAH" with some diagnostic accuracy data including true positives (TP), false negatives (FN), false positives (FP), and true negatives (TN).

analyze_diag_acc <- function(data, outcome, marker, cutoff, n=3){

 #1.) Number of subjects with/without outcome:
 n_outcome_marker <- sum(data[,outcome] & !is.na(data[,marker] & !is.na(data[,outcome])), na.rm=T)
 n_no_outcome_marker <- sum(data[,outcome]==0 & !is.na(data[,marker] & !is.na(data[,outcome])), na.rm=T)
 
 TP <- sum(data[,outcome] & !is.na(data[,marker]) & data[,marker]>cutoff, na.rm=T)
 FN <- sum(data[,outcome] & !is.na(data[,marker]) & data[,marker]<=cutoff, na.rm=T)
 FP <- sum(data[,outcome]==0 & !is.na(data[,marker]) & (data[,marker]>cutoff), na.rm=T)
 TN <- sum(data[,outcome]==0 & !is.na(data[,marker]) & (data[,marker]<=cutoff), na.rm=T)
 
 #2.) Sensitivity:
 Se_marker <- binom(TP, (TP+FN), n=n)[1]
 Se_lower_ci <- binom(TP, (TP+FN), n=n)[2]
 Se_upper_ci <- binom(TP, (TP+FN), n=n)[3]
 Sens <- paste0(round(Se_marker, 1), " (", round(Se_lower_ci, 1), "-", round(Se_upper_ci, 1), ")")

 #3.) Specificity:
 Sp_marker <- binom(TN, (TN+FP), n=n)[1]
 Sp_lower_ci <- binom(TN, (TN+FP), n=n)[2]
 Sp_upper_ci <- binom(TN, (TN+FP), n=n)[3]
 Spec <- paste0(round(Sp_marker, 1), " (", round(Sp_lower_ci, 1), "-", round(Sp_upper_ci, 1), ")")
 
 #4.) PPV:
 PPV_marker <- binom(TP, (TP+FP), n=n)[1]
 PPV_lower_ci <- binom(TP, (TP+FP), n=n)[2]
 PPV_upper_ci <- binom(TP, (TP+FP), n=n)[3]
 PPV <- paste0(round(PPV_marker, 1), " (", round(PPV_lower_ci, 1), "-", round(PPV_upper_ci, 1), ")")
 
 #5.) NPV:
 NPV_marker <- binom(TN, (TN+FN), n=n)[1]
 NPV_lower_ci <- binom(TN, (TN+FN), n=n)[2]
 NPV_upper_ci <- binom(TN, (TN+FN), n=n)[3]
 NPV <- paste0(round(NPV_marker, 1), " (", round(NPV_lower_ci, 1), "-", round(NPV_upper_ci, 1), ")")

 res <- data.frame(TP, FN, FP, TN, Se_marker, Se_lower_ci, Se_upper_ci, Sens, Sp_marker, Sp_lower_ci, Sp_upper_ci, Spec, PPV_marker, PPV_lower_ci, PPV_upper_ci, PPV, NPV_marker, NPV_lower_ci, NPV_upper_ci, NPV)
 return(as.data.frame(res))
}

# aSAH$outcome <- as.numeric(aSAH$outcome) -1
# (res <- analyze_diag_acc(data=aSAH, outcome="outcome", marker="s100b", cutoff=0.1)) #More suiteblae if you actually want to use the output of the function for further calculations, e.g. take the point estimates of sensitivity or PPV.
# do.call(rbind, res) #More suiteble for putting the output in a table.
