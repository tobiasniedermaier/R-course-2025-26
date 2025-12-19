
### Find a variable by its (partial) name ###

# A recurring situation in data analysis: You have a data set with hundrets or thousands of variables and you are looking for a specific one.
# You remember that the variable contained a certain sequence of letters. Let's take the dataset penguins_raw (built-in in R) as example.
head(penguins_raw, 3)
# Just for illustration, you have to imagine that the data set has 100x more variables, and the variables of interest are somewhere in the middle and hard to find.

# The sequence you remember is "ulmen" (because maybe you forgot if it was Culmen *something something* or Kulmen *something something*).

# A small helper function:
find_cols <- function(dat, varpart){colnames(dat)[grepl(varpart, colnames(dat))]}
find_cols(dat=penguins_raw, varpart="Culmen")
# Returns "Culmen Length (mm)" and "Culmen Depth (mm)".

# Now another example: You remember the variable name, but you are unsure how exactly it was spelled.
# For demonstration purposes, let's rename the 11th variable in mtcars (carb) to a (misspelled) variable "geer":
colnames(mtcars)[11] <- "geer"
# Now you want to show the variables with either the name "gear" or "geer":
find_cols(dat=mtcars, varpart="ge[a|e]r")
# The [a|e] construct (i.e., the letter at the third position could be either an a or an e) is inherited from the grepl function on which the user-defined find_cols function is based.

## See ?regex for all options on how to use those so-called Regular Expressions (regex) in R.
