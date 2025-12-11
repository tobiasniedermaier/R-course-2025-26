
#A function to perform a (logistic or other) regression without using the equation syntax. Advantage: can be used to perform a number of different regressions, with different data sets and/or covariates adjusted for, using the Map() function.

## My own version, written in 2019 or so:

reg <- function(data, outcome, covariates, family="binomial"){
 mylist <- list() #create an empty list to store regression equation
 for(i in c(covariates)){mylist[[i]] <- paste("+", data, "$", i, sep="")} #paste together regression equation, step 1 --> "+ data$var1 + data$var2 + ..."
 res1 <- paste(noquote(unlist(mylist))) #remove list and quotes
 res2 <- paste(res1, collapse=" ")
 rhs <- substr(res2, 2, nchar(res2)) #remove trailing "+"

 lhs <- paste(data, "$", outcome, sep="") #left-hand side of equation
 equation <- paste(lhs, "~", rhs) #put together regression equation
 res <- glm(eval(parse(text=paste(equation))), family=family) #conduct any regression from type "generalized linear model"
 return(res)
}
(a <- reg(data="mtcars", outcome="am", covariates=c("cyl","mpg")))
(b <- glm(am ~ cyl + mpg, data=mtcars, family="binomial")) #same result

## Improved Version by ChatGPT 5 Thinking:

reg <- function(data, outcome, covariates, family = "binomial") {
  ## data darf entweder der Name des Datensatzes (als String) ODER
  ## direkt ein data.frame-Objekt sein:
  if (is.character(data)) {
    dat <- get(data, envir = parent.frame())
  } else {
    dat <- data
  }

  ## (optional, aber hilfreich) – prüfen, ob Variablen existieren:
  missing_vars <- setdiff(c(outcome, covariates), names(dat))
  if (length(missing_vars) > 0L) {
    stop(
      "Variablen nicht im Datensatz gefunden: ",
      paste(missing_vars, collapse = ", ")
    )
  }

  ## Formelseite rechts: "cyl + mpg + ..."
  rhs <- paste(covariates, collapse = " + ")

  ## Ganze Formel als Objekt: am ~ cyl + mpg
  form <- as.formula(paste(outcome, "~", rhs))

  ## Regression ausführen (glm baut sich family intern selbst)
  glm(formula = form, data = dat, family = family)
}


# What was improved in the reg() function and why

# This version of reg() is a refactoring of the original code with the same goal:
# run a generalized linear model (e.g. logistic regression) without using formula syntax explicitly in the call, so that it can be easily looped over or used with Map().

# The main improvements are:

# 1. Avoiding eval(parse(...))

# The original implementation built a model expression as a text string and then ran it with:

# glm(eval(parse(text = paste(equation))), family = family)


# This pattern (eval(parse(text = ...))) is generally discouraged in R because:

# - It is hard to debug and reason about.
# - It is easy to introduce subtle bugs or security issues when constructing code as plain text.
# - It bypasses many of R’s usual scoping and checking mechanisms.

# The new version instead constructs a formula object using as.formula():

# form <- as.formula(paste(outcome, "~", rhs))
# glm(formula = form, data = dat, family = family)

# This is safer, more idiomatic R, and easier for learners to understand.

# 2. Simpler construction of the right-hand side (RHS)

# Originally, the code built a string like " + data$var1 + data$var2 + ..." using a loop and then removed the leading "+" with substr(). This required several intermediate objects.
# The refactored version builds the RHS directly from the covariate names:

# rhs <- paste(covariates, collapse = " + ")
# form <- as.formula(paste(outcome, "~", rhs))

# Advantages:
# - Fewer steps and less string manipulation.
# - Easier to read and maintain.
# - Closer to the way formulas are normally written in R.

# 3. More flexible data argument

# In the original version, data had to be a string containing the name of a dataset, e.g. "mtcars". The new version accepts either:
# - A character string (dataset name), or
# - A data frame object directly.

# Internally, it does:

# if (is.character(data)) {
  # dat <- get(data, envir = parent.frame())
# } else {
  # dat <- data
# }

# This allows both of the following to work:

# reg("mtcars", "am", c("cyl", "mpg"))
# reg(mtcars,   "am", c("cyl", "mpg"))

# This flexibility is useful when using Map() or other functional programming tools.

# 4. Explicit checking that variables exist

# The new function checks whether the outcome and all covariates are actually present in the dataset:

# missing_vars <- setdiff(c(outcome, covariates), names(dat))
# if (length(missing_vars) > 0L) {
  # stop(
    # "Variables not found in data: ",
    # paste(missing_vars, collapse = ", ")
  # )
# }

# Benefits:
# Students get a clear, informative error message if they misspell a variable name or use a variable not in the data.
# Debugging becomes much easier, especially for beginners.

# 5. Cleaner and more readable code
# The refactored function removes several intermediate objects (mylist, res1, res2, lhs, equation) and replaces them with a small number of clearly named steps:
# Normalize data to a data frame object (dat).
# Validate that variables exist.
# Build the RHS string ("cyl + mpg + ...") from covariates.
# Build a formula object.
# Run glm() with that formula and dat.
# This structure is closer to how one would write the model by hand (e.g. am ~ cyl + mpg) and is therefore easier for learners to connect to standard R usage.

# 6. Backwards compatibility of usage
# The function still supports calls in the style of the original example:
# a <- reg(data = "mtcars", outcome = "am", covariates = c("cyl", "mpg"))
# b <- glm(am ~ cyl + mpg, data = mtcars, family = "binomial")

# The results remain the same, but the underlying implementation is more robust, more idiomatic, and easier to extend or adapt in future.



