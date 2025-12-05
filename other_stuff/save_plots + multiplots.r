
################################
# How to save plots in R

boxplot(mpg ~ am, data=mtcars)

#First way (R console):
#File -> save as -> chose your preferred format and the location where to save the file to.
#You can change the dimensions simply by rearranging the height and width of the output window.

#Second way (R Studio):
#Your plot should appear in "Plots" at the bottom-right. (Depends on your R Studio configuration.)
#Click Export -> Save as Image, Save as PDF, or copy to clipboard.
#If you select image, you can choose the file format (png, jpg, tiff, bmp, metafile, svg, eps) and the dimensions (width and heigth).

#Third way (using only code, no interaction with the user interface - recommended, because you can specify exactly what your code should look like.):

#- Create a graphics device:
svg("my_svg_plot.svg", width=7, height=10)

#- Insert the code to generate your plot:
boxplot(mpg ~ am, data=mtcars)

#- Close the graphics device again: 
dev.off()

#! Don't forget this last step!!

#Check where the plot was saved:
getwd()
#You should find a file named my_svg_plot.svg in this directory.

#This "Third way" is what I would recommend because it generates your plot every time you run the codes, without additional manual work. (There is manual work, of course, but only the one time when you write the code and not every time you run it and want to save the plot.)

#If you want your plots to be saved in a different directory, chage the working directory before you create and save the first plot:

setwd("path/to/my/results/directory")
svg("my_svg_plot.svg", width=7, height=10) #You can specify the width and heigth (both in inches) of the plot, among other options.
boxplot(mpg ~ am, data=mtcars)
dev.off()

#Note that there are many different graphics devices. You can also save your plot as pdf, bmp, png, jpeg, and a few more. see:
?Devices

#Hint: tiff() gives high-quality plots (in vector graphics), usually suitable for publications.
#Recommendation: Add the argument compress="zip+p" to get much smaller files with almost identical quality. Check ?tiff to see which compression methods are available. (lzw+p, jpeg, lzma, and many more.)


#As always, the workflow is ENTIRELY different if you use Hadley Wickham's ggplot:

library(ggplot2)
ggplot(mtcars, aes(x = factor(am), y = mpg)) +  geom_boxplot()
 # +labs(x = "Transmission (0 = Automatic, 1 = Manual)",
 #      y = "Miles Per Gallon (mpg)",
 #      title = "Boxplot of MPG by Transmission Type")

#save the plot:
ggsave("grouped_boxplot.jpeg")
#Check ??ggsave for further options.


#Small side note: Journals typically only allow a limited number of plots per article. But you can circumvent this limitation by combining several plots into one.

#Easiest way of doing that:
par(mfrow = c(2,3))

boxplot(mtcars$mpg, horizontal=TRUE)
plot(mtcars$mpg, mtcars$disp)
hist(mtcars$wt)
boxplot(mpg ~ am, data=mtcars, horizontal=TRUE)
barplot(mtcars$hp)
plot(mtcars$mpg, mtcars$wt, pch=2)

dev.off()


tiff(filename = "multiplot.tif", width = 10, height = 8, res=300, units = "in", compression="zip+p")

mylayout <- layout(matrix(c(1, 1, 2,
							3, 4, 2,
							3, 5, 5, 
							6, 6, 6), nrow=4, byrow = TRUE))
layout.show(n=6) #A very complicated arrangement of plots.

boxplot(mtcars$mpg, horizontal=TRUE)
plot(mtcars$mpg, mtcars$disp)
boxplot(mpg ~ am, data=mtcars, horizontal=TRUE)
hist(mtcars$wt)
barplot(mtcars$hp)
x <- seq(from = 0, to = 4 * pi, length.out = 100)
y <- sin(x)
plot(x, y, type = "l")

dev.off()
