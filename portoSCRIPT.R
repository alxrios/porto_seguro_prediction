#########################################################################
# This script is intended for helping with the portoMKDWN.Rmd document. #
#########################################################################

### TO DO: Convert all variables that are factors as it, taking care of the ordinals

# Necessary libraries will be loaded here:

library(kableExtra)

# Check if a particular package is installed

checklib <- installed.packages()
dim(checklib)
sum(checklib[, 1] == "kableExtra")
sum(checklib[, 1] == "knitr")

# The first thing to do will be setting the working enviroment to the one
# where the data files are.

getwd()
setwd("..") # Moves backward in the current directory
setwd(".//datasets//porto_seguro_safe_driver_prediction")
porto_train <- read.csv("train.csv")

# Dimensions of the dataset
dim(porto_train)

# Columnnames
names(porto_train)
table(names(porto_train))
data.frame(colnames(porto_train))
data.frame(index = 1:dim(porto_train)[2], colnames(porto_train))
# Columnames with kableExtra
names_dt <- data.frame(index = 1:dim(porto_train)[2], colnames(porto_train))
names_dt[1] <- lapply(names_dt[1], function(x) {
  cell_spec(x, bold = T, 
            color = spec_color(x, end = 0.1))
})
names_dt[2] <- cell_spec(names_dt[[2]], color = "white", bold = T,
                         background = spec_color(1:59, end = 0.9, option = "A", direction = -1))
kbl(names_dt, escape = F, align = "c", caption = "All variable names.") %>%
  kable_classic("striped", full_width = F)


# Since the dataset has the missing values codified has "-1" and R uses NA for this, 
# let's change all the -1 to NA.

naConvert <- function(value) {
  # Gets any value and if is equal to -1 returns NA, any other case returns the
  # same value
  result <- value
  if (!is.na(value) & value == -1) {
    result <- NA
  }
  return(result)
}

# A little testing...
naConvert("hi")
naConvert(5)
naConvert(-1)
naConvert(-1.5)

vapply(sample(c(1:100, rep(-1, 100)), 25), naConvert, numeric(1))
test_sample <- sample(c(1:100, rep(-1, 100)), 25)
test_sample
vapply(test_sample, naConvert, 1)

# Let's take a small sample of the dataset for testing the function
rnd_seed <- round(runif(n = 1, min = 1, max = 10000))
print(rnd_seed)
set.seed(rnd_seed)
# Seed candidates: 8511, 7466, 1953
set.seed(8511)
porto_sample <- porto_train[sample(1:595212, 100), sample(1:59, 25)]
# Are observations classified has -1 present in the sample obtained?
for (i in 1:25) {
  # IF -1's are found, a positive number bigger than zero is returned
  cat("Variable ", names(porto_sample)[i], ":", " ", sum(porto_sample[, i] == -1), "\n", sep = "")
}
# Now we know that there are actually observations classified as -1 in the sample
# let's check again the function
# First with the variable ps_car_05_cat
vapply(FUN = naConvert, porto_sample$ps_car_05_cat, FUN.VALUE = 1)
summary(vapply(FUN = naConvert, porto_sample$ps_car_05_cat, FUN.VALUE = 1))
# Next with the variable Variable ps_reg_03
vapply(FUN = naConvert, porto_sample$ps_reg_03, FUN.VALUE = 1)
summary(vapply(FUN = naConvert, porto_sample$ps_reg_03, FUN.VALUE = 1))
# Variable Variable ps_car_14
vapply(FUN = naConvert, porto_sample$ps_car_14, FUN.VALUE = 1)
summary(vapply(FUN = naConvert, porto_sample$ps_car_14, FUN.VALUE = 1))
# Variable Variable ps_car_01_cat
vapply(FUN = naConvert, porto_sample$ps_car_01_cat, FUN.VALUE = 1)
summary(vapply(FUN = naConvert, porto_sample$ps_car_01_cat, FUN.VALUE = 1))
# And last variable ps_car_03_cat
vapply(FUN = naConvert, porto_sample$ps_car_03_cat, FUN.VALUE = 1)
summary(vapply(FUN = naConvert, porto_sample$ps_car_03_cat, FUN.VALUE = 1))


# Now let's check which observations have -1's for the whole dataset
# Also, let's store the names of the variables with -1's and the quantity of them
# for a future checking
# varnames <- character() # Stores the variable names with missings | DELETE
missingCount <- numeric() # Stores the number of missings found (-1's)
varpos <- numeric() # Stores the position of that variable in colnames(dataset)
for (i in colnames(porto_train)) {
  missingSum <- sum(porto_train[, i] == -1)
  cat("Variable ", i, ":", " ", missingSum, "\n", sep = "")
  if (missingSum != 0) {
    # varnames <- c(varnames, i)
    missingCount <- c(missingCount, missingSum)
    varpos <- c(varpos, which(i == colnames(porto_train)))
  }
}
# After applying the function naConvert we will check the resulting number of NA's
# with this results
# Example:
summary(vapply(FUN = naConvert, porto_train$ps_ind_02_cat, FUN.VALUE = 1))
# Process the change
for (i in colnames(porto_train)) {
  porto_train[, i] <- vapply(X = porto_train[, i], FUN = naConvert, FUN.VALUE = 1)
}

summary(vapply(X = porto_train[, "ps_ind_02_cat"], FUN = naConvert, FUN.VALUE = 1))
# naConvert should cover the case when the value is NA

# Checking after the conversion
for (i in colnames(porto_train)) {
  cat("Variable ", i, ":", " ", sum(porto_train[, i] == -1, na.rm = T), "\n", sep = "")
}

# Now let's check that the number of NA's coincide the original number of -1's 
# obtained for each variable with summary funciton
for (i in colnames(porto_train)) {
  print(summary(porto_train[, i]))
}


# When printing the first time the quantity of -1's in a variable let's store the 
# names of the variables that contain -1's, for comparing before and after the results
# for both groups of variables (with -1's and without it)

# A data.frame for checking that the quantity of missing observations is the same
# before and after applying the function
checkFrame <- data.frame(variable = colnames(porto_train)[varpos], missingBefore = missingCount,
                         missingAfter = rep(0, length(varpos)))

for (i in colnames(porto_train)[varpos]) {
  checkFrame[which(checkFrame$variable == i), "missingAfter"] <- summary(porto_train[, i])[7]
}
checkFrame
# Let's add a new row with the totals and a new column with 1 if both columns are equal
# and 0 otherwise
checkFrame[dim(checkFrame)[1] + 1, ] <- c("total", colSums(checkFrame[, c("missingBefore", "missingAfter")]))
checkFrame$equal <- as.numeric(checkFrame$missingBefore == checkFrame$missingAfter)

# Create another dataframe with the variables without missing after and before for checking
# that no NA has been introduced by mistake
colnames(porto_train)[which(colnames(porto_train) != varnames)]


### test if test[-(a row of unique characters), works] on a dataset test <- iris
### test for a dataframe with rownames. Put for test new rownames created with 
### the values of Species column + the row number resulting in unique rownames
test <- iris
rownames(test) <- paste(test$Species, 1:dim(test)[1])
test[-c("setosa 1"), ]
# It can't be done this way, so variable possition must be saved

# A dataframe for checking that the rest of variables remain at zero missings
# after using the function naConvert
checkFrame2 <- data.frame(variable = colnames(porto_train)[-varpos])
# Varnames aren't needed now since we are storing the posiion names
checkComplete <- numeric() # Variable to store the results of checking that no new
# NA's were introduced
for (i in colnames(porto_train)[-varpos]) {
  checkComplete <- c(checkComplete, sum(is.na(porto_train[, i])))
}
checkFrame2$missingAfter <- checkComplete
checkFrame2

# Trying to print column names in wide format

data.frame(index = 1:dim(porto_train)[2], colnames(porto_train))
porto_names <- data.frame(index = 1:dim(porto_train)[2], colnames(porto_train))
namesframe <- data.frame(index = 1:10, colnames = colnames(porto_train)[1:10], index = 11:20, 
                         colnames = colnames(porto_train)[11:20], index = 21:30, 
                         colnames = colnames(porto_train)[21:30], index = 31:40,
                         colnames = colnames(porto_train)[31:40], index = c(41:59, ""), 
                         colnames = c(colnames(porto_train)[41:59], ""))

namesframe

