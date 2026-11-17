
############################
# EQ-5D distributions

# Author: Tobias Niedermaier

## R code to generate and plot the distribution of all possible EQ-5D-5L values, using the German Value Set:

# All possible combinations and corresponding (negative) utility changes: [Note that the reference value is 1 in case of no restrictions in any of the 5 domains. The values below (one per domain) are subtracted according to the stated severity of the restrictions]

expand.grid(c(0,0.026,0.042,0.139,0.224), c(0,0.050,0.056,0.169,0.260), c(0,0.036,0.049,0.129,0.209), c(0,0.057,0.109,0.404,0.612), c(0,0.030,0.082,0.244,0.356))

# Resulting utility values:
1-rowSums(expand.grid(c(0,0.026,0.042,0.139,0.224), c(0,0.050,0.056,0.169,0.260), c(0,0.036,0.049,0.129,0.209), c(0,0.057,0.109,0.404,0.612), c(0,0.030,0.082,0.244,0.356)))

# Plot of the resulting values (=hypothetical distribution of EQ-5D-5L values if all possible combinations occured with the same probability):
plot(sort(1-rowSums(expand.grid(c(0,0.026,0.042,0.139,0.224), c(0,0.050,0.056,0.169,0.260), c(0,0.036,0.049,0.129,0.209), c(0,0.057,0.109,0.404,0.612), c(0,0.030,0.082,0.244,0.356)))), pch=".")
dev.off()
# Note that a substantial proportion of possible answers yield a negative utility value ("Worse than death")!


## R code to generate and plot the distribution of all possible HUI-2 values (Health Utility Index 2):

dim(expand.grid(c(1,0.95,0.86,0.61), c(1,0.97,0.84,0.73,0.58), c(1,0.93,0.81,0.70,0.53), c(1,0.95,0.88,0.65), c(1,0.97,0.91,0.80), c(1,0.97,0.85,0.64,0.38), c(1,0.97,0.88))) #24000

plot(sort(1.06*apply(expand.grid(c(1,0.95,0.86,0.61), c(1,0.97,0.84,0.73,0.58), c(1,0.93,0.81,0.70,0.53), c(1,0.95,0.88,0.65), c(1,0.97,0.91,0.80), c(1,0.97,0.85,0.64,0.38), c(1,0.97,0.88)) , 1, prod))-0.06, type="p", pch=".", ylab="Utility", xlab="Possible health states (from worst to best)")
dev.off()
summary(sort(1.06*apply(expand.grid(c(1,0.95,0.86,0.61), c(1,0.97,0.84,0.73,0.58), c(1,0.93,0.81,0.70,0.53), c(1,0.95,0.88,0.65), c(1,0.97,0.91,0.80), c(1,0.97,0.85,0.64,0.38), c(1,0.97,0.88)) , 1, prod))-0.06)

#Minimum: 
1.06*(0.61*0.58*0.53*0.65*0.80*0.38*0.88)-0.06  #-0.02543718

#Maximum: 
1.06*(1*1*1*1*1*1*1)-0.06 #1

## R code to generate and plot the distribution of all possible HUI-3 values (Health Utility Index ):

dim(expand.grid(c(1,0.98,0.89,0.84,0.75,0.61), c(1,0.95,0.89,0.80,0.74,0.61), c(1,0.94,0.89,0.81,0.68), c(1,0.93,0.86,0.73,0.65,0.58), c(1,0.95,0.88,0.76,0.65,0.56), c(1,0.95,0.85,0.64,0.46), c(1,0.92,0.95,0.83,0.60,0.42), c(1,0.96,0.90,0.77,0.55))) #972000

plot(sort((1.371*apply(expand.grid(c(1,0.98,0.89,0.84,0.75,0.61), c(1,0.95,0.89,0.80,0.74,0.61), c(1,0.94,0.89,0.81,0.68), c(1,0.93,0.86,0.73,0.65,0.58), c(1,0.95,0.88,0.76,0.65,0.56), c(1,0.95,0.85,0.64,0.46), c(1,0.92,0.95,0.83,0.60,0.42), c(1,0.96,0.90,0.77,0.55)), 1, prod))-0.371), type="p", pch=".", ylab="Utility", xlab="Possible health states (from worst to best)")

#Minimum: -0.3590273

#Maximum: 1.000
