
###################################################################
# Survival-Analysen using the "survival" and "survminer" packeges #
###################################################################

library(survival) #package for survival analysis
library(survminer) #for nicer plots using the function ggsurvplot()
head(pbc) #data set included in the survival package

# Take a look at the data set ----
?pbc                    # Description of the data set
head(pbc)
str(pbc)

# Check for missing values
sum(is.na(pbc))  # Total missing values in the dataset
sum(is.na(pbc$age))  # Missing values in age
sum(is.na(pbc$time)) # Missing values in survival time
sum(is.na(pbc$status)) # Missing values in event status

# You may want to remove rows with NAs for Cox regression:
pbc_cc <- na.omit(pbc)

#Create the survival object. 
(surv_km <- survfit(Surv(pbc$time, pbc$status==2) ~1))
#Define status==2 as death. Thus, status==1 is treated as censored.
pbc$SurvObj <- with(pbc, Surv(time, status == 2))

#?survfit

#survfit(formula, data, weights, subset, na.action, etype, id, istate, timefix=TRUE, ...)
#Arguments
#formula 	a formula object, which must have a Surv object as the response on the left of the ~ operator and, if desired, terms separated by + operators on the right. One of the terms may be a strata object. For a single survival curve the right hand side should be ~ 1. 

######################
# 1. Kaplan-Meier curve:

plot(surv_km)

#A little bit more comprehensive:
plot(
  surv_km,
  xlab = "Time since inclusion (days)",
  ylab = "Survival probability",
  main = "Kaplan-Meier curve (all patients)"
)

# Important summary statistics
summary(surv_km)           # survival at specific times
surv_median(surv_km)       # median survival (survminer)


######################
# 2.  Kaplan-Meier curve stratified by sex:

surv_km_by_sex <- survfit(SurvObj ~ sex, data = pbc, conf.type = "log-log")

plot(surv_km_by_sex)
plot(surv_km_by_sex, mark.time=T) #with "ticks" at events
plot(
  surv_km_by_sex,
  xlab = "Time (days)",
  ylab = "Survival probability",
  col  = 1:2,
  lty  = 1:2,
  main = "Survival by sex"
)
legend("bottomleft", legend = levels(pbc$sex), col = 1:2, lty = 1:2)


######################
# 3.  Nicer plots with ggsurvplot:

ggsurvplot(surv_km_by_sex)
ggsurvplot(surv_km_by_sex, fun="cumhaz") #"cumhaz" for the cumulative hazard function
ggsurvplot(surv_km_by_sex, fun="event") #"event" for cumulative events
ggsurvplot(surv_km_by_sex, fun="pct") #"pct" for survival probability in percentage.
ggsurvplot(surv_km_by_sex, conf.int=T) #with CIs
ggsurvplot(surv_km_by_sex, risk.table=T) #with "risk table"


######################
# 4.  Group comparisons with survdiff():

#test for difference between male and female:
survdiff(SurvObj ~ sex, data=pbc) #chi-squared test and p value


######################
# 5.  Cox regression (multivariate):
#see http://rstudio-pubs-static.s3.amazonaws.com/5896_8f0fed2ccbbd42489276e554a05af87e.html

cox1 <- coxph(SurvObj ~ age + sex + hepato + bili + albumin + platelet + stage, data=pbc)

summary(cox1)          # HRs, CIs, p values
exp(coef(cox1))        # Hazard Ratios
exp(confint(cox1))     # 95%-CIs for the HRs


ggforest(cox1) #Forest plot with HRs and CIs for all variables!
ggforest(cox1, main="Cox model: Hazard ratios")
# ggforest generates a forest plot to visualize the HRs and 95% CIs for each covariate
# HRs greater than 1 indicate an increased hazard, and HRs less than 1 indicate a decreased hazard for that variable.

######################
# 6. Check for violation of proportional hazard (constant HR over time)

(zph1 <- cox.zph(cox1)) #Stage is statistically significant. Note: This suggests a violation of the proportional hazards assumption!

#Schoenfeld residuals:
plot(zph1)

#For all variables using the survminer function (nicer):
ggcoxzph(zph1) # This plots Schoenfeld residuals for each variable
# Use ggcoxzph() to visualize the proportional hazards assumption
# ggcoxzph() helps visualize the Schoenfeld residuals for each covariate. The plot shows whether the residuals are random (no violation) or follow a pattern (violation of PH assumption)


# Solution 1: Stratification by status
cox2 <- coxph(SurvObj ~ age + sex + hepato + bili + albumin + platelet + strata(stage), data=pbc)
summary(cox2)
(zph2 <- cox.zph(cox2)) #better

# Solution 2: Time-varying effect for stage (e.g., the effect of stage could change over time)
cox3 <- coxph(Surv(time, status == 2) ~ age + sex + hepato + bili + albumin + platelet + tt(stage), data = pbc, tt = function(x, t) x * log(t))
summary(cox3)
