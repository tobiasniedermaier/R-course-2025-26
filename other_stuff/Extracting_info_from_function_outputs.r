
################################################
# Extracting information from function outputs

#In some cases, functions return data frames, which are very easy to handle. Example:
(res <- aggregate(mpg ~ cyl + am, data=mtcars, FUN=summary))
is.data.frame(res) #TRUE
res[res$cyl==4 & res$am==1,]
#same as:
summary(mtcars[mtcars$cyl==4 & mtcars$am==1,]$mpg)

#In other cases, functions return vectors of type numeric or logical. This is also easy to handle. Example:
mtcars$mpg > 20
table(mtcars$mpg > 20)
#Note that we can acutally count the number of TRUE values because TRUE and FALSE are internally coded as 1 and 0 in R:
sum(mtcars$mpg > 20) #14

#In many cases, function return lists, which can be subsetted. Note that the outputs don't always look like typical lists because there are so-called "printing methods". These are custom functions in which it is defined how the output of a list should be presented. Examples:
(res_lm_mtcars <- summary(lm(mpg ~ cyl + hp + wt, data=mtcars)))
#Now we cannot simply use something like res_lm_mtcars$Coefficients or res_lm_mtcars$cyl because this is not how the results are acutally stored in the object.

res_lm_mtcars[] #Now we can see the actual list structure and what we need to do to extract e..g a specific coefficient.
str(res_lm_mtcars) #This would also work and additionally contains information on the data types, but it omits parts of the list outputs.

#Now we can see what we need to type to extract (for instance) the adjusted R-squared:
res_lm_mtcars$adj.r.squared

#Or the regression coefficients with p values etc.:
res_lm_mtcars$coefficients

#This is quite helpful if we want (for example) to add model outputs to a plot:
(res_lm_mtcars_test <- summary(lm(mpg ~ wt, data=mtcars)))
(p_value <- res_lm_mtcars_test$coefficients[,"Pr(>|t|)"]["wt"])

plot(mtcars$wt, mtcars$mpg)
abline(lm(mpg ~ wt, data=mtcars))
text(4, 23, paste0("p value: ", round(p_value, 5)))

#Small problem: The p value is so small now that it only shows 0. How we could fix that:
p_value2 <- ifelse(p_value >0.0001, p_value, "<0.0001")
plot(mtcars$wt, mtcars$mpg)
abline(lm(mpg ~ wt, data=mtcars))
text(4, 23, paste0("p value: ", p_value2))

