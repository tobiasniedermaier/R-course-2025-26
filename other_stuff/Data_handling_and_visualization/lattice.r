
#########################
# The "lattice" package #
#########################

# https://www.stat.ubc.ca/~jenny/STAT545A/block09_xyplotLattice.html

gdURL <- "http://www.stat.ubc.ca/~jenny/notOcto/STAT545A/examples/gapminder/data/gapminderDataFiveYear.txt"
gDat <- read.delim(file = gdURL)

#Drop Oceania, which only has two countries
jDat <- droplevels(subset(gDat, continent != "Oceania"))
str(jDat)

library(lattice)

#scatterplots:
xyplot(lifeExp ~ gdpPercap, jDat) #without grid
xyplot(lifeExp ~ gdpPercap, jDat, grid=T) #with grid
xyplot(lifeExp ~ gdpPercap, jDat, grid=T, scales=list(x=list(log=10, equispaced.log=F)), type=c("p","r"))
xyplot(lifeExp ~ gdpPercap, jDat, grid=T, scales=list(x=list(log=10, equispaced.log=F)), type=c("p", "smooth"), col.line="darkorange", lwd=3) #with bold, smooth, orange line

#Equivalents in base R:
plot(jDat$gdpPercap, jDat$lifeExp)
plot(jDat$gdpPercap, jDat$lifeExp, panel.first = grid())
plot(jDat$gdpPercap, jDat$lifeExp, log="x", panel.first = grid())
lines(lowess(jDat$gdpPercap, jDat$lifeExp)) #"smoothed" line added


#R in Action, p. 257, Listing 11.1:
attach(mtcars)
plot(wt, mpg, main="Basic Scatter plot of MPG vs. Weight", xlab="Car Weight (lbs/1000)",ylab="Miles Per Gallon ", pch=19)
abline(lm(mpg~wt), col="red", lwd=2, lty=1)
lines(lowess(wt,mpg), col="blue", lwd=2, lty=2) #!

#In lattice:
detach(mtcars)
xyplot(mpg ~ wt, mtcars, type=c("p","smooth","r"), cex=1.2, col="black", col.line="blue", lty=2, lwd=2, pch=20, main="Lattice Scatter plot of MPG vs. Weight", xlab="Car Weight (lbs/1000)",ylab="Miles Per Gallon ")


#Scatter plots with different colors according to group:
#base R:
set.seed(123)
DF <- data.frame(A=rnorm(10), B=rpois(10,10), C=sample(1:3, 10, replace=T))

plot(B ~ A, pch=16, col=C, data = DF)
legend('bottomright', paste('Group',1:3,sep=" "), col=1:3, pch=16)

#lattice:
xyplot(B ~ A, DF, group=C, auto.key=T)

#Multi-panel plot:
xyplot(lifeExp ~ gdpPercap | continent, jDat,
       grid = TRUE,
       scales = list(x = list(log = 10, equispaced.log = FALSE)))
#And with different colors in each plot:
xyplot(lifeExp ~ gdpPercap | continent, jDat,
       group = continent,
       grid = TRUE,
       scales = list(x = list(log = 10, equispaced.log = FALSE)))

#same in ggplot2 for comparison:
multiplot <- ggplot(jDat, aes(x=gdpPercap, y=lifeExp)) + geom_point() 
final_plot <- multiplot + facet_wrap(~continent)
final_plot

#and finally with base R:
par(mfrow=c(2,2))
strat <- unique(jDat$continent)
for (i in 1:length(strat)){
 dat <- jDat[jDat$continent==strat[i],]
 plot(dat$gdpPercap, dat$lifeExp, 
 main=strat[i], 
 xlab="gdpPercap",
 ylab="lifeExp")
}

#Histograms:

#base R:
hist(mtcars$mpg, breaks=nrow(mtcars), col=heat.colors(nrow(mtcars)), xlab="mpg", ylab="count", main="Base R histogram")

#ggplot2:
qplot(mpg, data=mtcars, main="ggplot2 (qplot) histogram") #Or:
ggplot(mtcars, aes(x=mpg)) + geom_histogram(stat="bin") + ggtitle("ggplot histogram") + theme_bw()

#lattice:
histogram(~mpg, data=mtcars, main="lattice histogram", col=heat.colors(25), breaks=20)


#Boxplots:

#base R:
boxplot(mpg~cyl, data=mtcars, main="Car Milage Data", xlab="Number of Cylinders", ylab="Miles Per Gallon")

#lattice:
bwplot(cyl~mpg, data=mtcars, main="Car Milage Data", ylab="Number of Cylinders", xlab="Miles Per Gallon")

#ggplot2:
(ggboxplot <- ggplot(data=mtcars, aes(x=cyl, y=mpg, group=cyl)) + geom_boxplot() + theme_bw() + scale_x_discrete(name = "Number of cylinders") + scale_y_continuous(name = "Miles per gallon"))


