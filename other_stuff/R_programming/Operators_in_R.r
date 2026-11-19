
### Operators in R

# Learning objectives:
#- Operators are functions.
#- Functions in the form f(x, y) can be rewritten (by the user) as x %f% y. (So-called infix functions.)
#- Operators (x %f% y) can be rewritten as `%f%`(x, y). (Or '%f%'(x, y) or "%f%"(x, y), it doesn't matter.)
#- Functions can be assigned to variables and passed as arguments to other functions.

## User-defined infix functions:
#!%in%-Operator ("not in"):
'%!in%' <- function(x, y) !(x %in% y)

toys1 <- c("blue_car", "red_car", "green_car")
toys2 <- c("red_car", "yellow_car", "black_car")

toys1 %in% toys2
`%in%`(toys1, toys2)
intersect(toys1, toys2)
toys1[toys1 %in% toys2]

toys2 %in% toys1
toys2 %!in% toys1
toys1 %!in% toys2


## More complex operators:

#xor-Operator:
'%xor%' <- function(x,y)((x | y) & !(x & y)) #Just for illustration. R already has a function xor(). But now we implemented it as "infix" function.

TRUE %xor% FALSE
FALSE %xor% TRUE
TRUE %xor% TRUE
FALSE %xor% FALSE

A <- 1:5
B <- 3:7
C <- 5:9

A > B #FALSE FALSE FALSE FALSE FALSE
A < B #TRUE TRUE TRUE TRUE TRUE
A %in% B #FALSE FALSE  TRUE  TRUE  TRUE
B %in% C #FALSE FALSE  TRUE  TRUE  TRUE
C %in% B #TRUE  TRUE  TRUE FALSE FALSE
(A %in% B) | (B %in% A) #TRUE TRUE TRUE TRUE TRUE
(A %in% B) %xor% (B %in% A) #TRUE  TRUE FALSE  TRUE  TRUE


#Symmetric setdiff-Operator: (returns all elements that occur only in a or in b)
'%sdf%' <- function(a,b){
	sdfxy <- setdiff(a,b)
	sdfyx <- setdiff(b,a)
	return(union(sdfxy,sdfyx))
}

c(1, 2, 3, 4, 5) %sdf% c(4, 5, 6, 7, 8)
c(4, 5, 6, 7, 8) %sdf% c(1, 2, 3, 4, 5)
#The function is "symmetric" (a %sdf% b returns the same elements as b %sdf% a), but they are ordered differently: The output always starts with the elements in the first vector.

## String manipulations:

#"String paste" infix-Operatoren:
'%p%' <- function(a,b){paste(a,b, sep="")} #e.g., "part1" %p% "part2" #concatenates the strings, with no space in between. 
`%p%` <- function(a, b){paste0(a, b)} #Same.

"Hello" %p% "World"

'%P%' <- function(a,b){paste(a,b)} #e.g., "part1" %P% "part2" #concatenates the strings, with space in between. 

"Hello" %P% "World"

#You can turn many "normal" functions [f(a, b)] into an infix function [a %f% b] and vice versa. Some more examples:

mtcars[2,5]
`[`(mtcars, 2, 5)

#When could this be useful?
mylist <- list(a=1:3, b=4:10, c=2:6)
#Extract the second element of a, b, and c:

lapply(mylist, `[`, 2)

# Equivalent:
lapply(mylist, function(x) x[2])

#Not to be confused with:
`[`(mylist, 2) #which equals:
mylist[2]


#Extract [2, 5] from each data frame:
mylist <- list(mtcars, 2*mtcars, 3*mtcars)
lapply(mylist, `[`, 2, 5)
#Or maybe more intuitive:
lapply(mylist, function(x){x[2, 5]})
#...


3 + 5
`+`(3, 5) #Even familiar operators such as +, -, * and == are functions in R. Their function-call syntax is usually hidden behind special operator syntax.

cor(1:10, (1:10)^2)
'%cor%' <- function(a, b){cor(a, b)}
(1:10) %cor% (1:10)^2 #Same calculation, just the coding "style" changes. Not necessarily better than cor(a, b).

#You could even write:
'%cor%' <- cor
(1:10) %cor% (1:10)^2


5==5
`==`(5, 5)
5==4
`==`(5, 4)

#When could that be useful?

mylist <- list(mtcars, 2*mtcars, 3*mtcars)
Reduce("+", mylist)
Reduce(`+`, mylist)

Reduce("*", list(2, 3, 4))# 24
Reduce("*", list(2, 3, 4), accumulate=TRUE)
# Returns the intermediate results: 2, 6, 24
# With a list as input, the accumulated results are returned as a list.
