
#assignment vs. super assignment

rm(list=ls(all=TRUE))

#normal assignment:
x <- 5
x #5

rm(list=ls(all=TRUE))
x <<- 5
x #5

y <- function(){z <- 5; return(z)}
y() #5
z #not found
z <- y()
z #5

#super assignment
rm(list=ls(all=TRUE))
y <- function(){z <<- 5; return(z)}
y() #5
z #5 (!)
rm(list=ls(all=TRUE))

#equivalent:
y <- function(){assign("z", 5, envir= .GlobalEnv)}
z #Error: object 'z' not found
y() #seemingly nothing happens
z #5 (!)
rm(list=ls(all=TRUE))

#another example:
test <- function(x){if (x %% 2 == 0) y <<- "odd" else y <<- "even"}
test(5)
y #"even"
test(6)
y #"odd"

test2 <- function(x) {if (x %% 2 == 0) return(c("odd")) else return(c("even"))} #"classical" variant without side-effects

#In words: Functions are usually "self-contained" / have no "side-effects". 
#That means calling a function alone (without explicitely assigning the result to an object) does not assign any values to objects in the work space. 
#(Although the result may of course be assigned to an object explicitely.) 
#In contrast, with "super assignment" (out-of-scope assignment, global assignment), the function call alone will already assign a value to an object in the work space.

#scope of functions:
a <- 0
b <- 2
FUN <- function(a, b){
return(a+b)}
FUN(a=1, b=1)
FUN(a=1, b=b)
FUN(a=1, b=3)

test <- function(){
if (exists("a")) stop("Object already exists! No actions were done.")
else a <<- 5
}
rm(a) #make sure object a is deleted if it exists
test()
a #5
a <- 10
test()
a #still 10


g <- function(){
 i <- 0
 f <- function(){
    i <<- i+1
    cat("this function has been called ", i, " times", "\n")
    date()  
}}
 
f <- g()
#first call
f()
#second call
f()
#Note that, we used the <<- operator that assigns in the parent environment.
#This is equivalent to:
#assign("i", i+1, envir = parent.env(environment()))


################
# Environments #
################

#https://www.r-bloggers.com/environments-in-r/

#start with empty workspace:
rm(list=ls(all=TRUE))

#create a new environment:
env1 <- new.env()
assign("a", 5, envir=env1)
a #Error: object 'a' not found
env1$a #5
ls() #[1] "env1"
ls(env=env1) #"a"
get("a", env=env1) #5

env2 <- new.env()
assign("a", 1, env=env2) 
ls() #[1] "env1" "env2"
get("a", env=env2) #1
env2$a #1
env1$a + env2$a #6 (5+1)

y <- function(){assign("z", 5, envir= env1)}
y()
env1$z #5

env1$add1 <- function(x){x+1}
env1$add1(4) #5
add1(4) #Error in add1.default(4) : no terms in scope

env1$mtcars #NULL
env1$mtcars <- mtcars
env1$mtcars$mpg

#D.h. mit environments kann man praktisch eine zweite R-Session eröffnen, in der dieselben Variablen andere Werte zugewiesen bekommen und mit diesen kann man auch simultan rechnen. Außerdem kann man damit super assignment sicher verwenden, indem man ebd. nur in neuer environment macht.

summary(env1$mtcars)
env1$iris <- iris
ls.str(env1)

#neue environment mit zwei data frames und sonst nichts:
env3 <- new.env()
env3$iris <- iris
env3$mtcars <- mtcars
eapply(env3, summary)


####################
# short circuiting #
####################

larger_5 <- function(x){
x > 5 && return(x)
}

larger_5(3) #FALSE
larger_5(8) #8

#identisch:
larger_5_alt <- function(x){
if (x<=5) return(FALSE)
else return(x)
}

larger_5_alt(3) #FALSE
larger_5_alt(8) #8


