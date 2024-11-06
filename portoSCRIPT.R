#########################################################################
# This script is intended for helping with the portoMKDWN.Rmd document. #
#########################################################################

### TO DO: Convert all variables that are factors as it, taking care of the ordinals

# Necessary libraries will be loaded here:

library(ggplot2)

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
vapply(FUN = naConvert, porto_sample$ps_car_05_cat, FUN.VALUE = numeric(1))
summary(vapply(FUN = naConvert, porto_sample$ps_car_05_cat, FUN.VALUE = (numeric(1))))
# Next with the variable Variable ps_reg_03
vapply(FUN = naConvert, porto_sample$ps_reg_03, FUN.VALUE = numeric(1))
summary(vapply(FUN = naConvert, porto_sample$ps_reg_03, FUN.VALUE = numeric(1)))
# Variable Variable ps_car_14
vapply(FUN = naConvert, porto_sample$ps_car_14, FUN.VALUE = numeric(1))
summary(vapply(FUN = naConvert, porto_sample$ps_car_14, FUN.VALUE = numeric(1)))
# Variable Variable ps_car_01_cat
vapply(FUN = naConvert, porto_sample$ps_car_01_cat, FUN.VALUE = numeric(1))
summary(vapply(FUN = naConvert, porto_sample$ps_car_01_cat, FUN.VALUE = numeric(1)))
# And last variable ps_car_03_cat
vapply(FUN = naConvert, porto_sample$ps_car_03_cat, FUN.VALUE = numeric(1))
summary(vapply(FUN = naConvert, porto_sample$ps_car_03_cat, FUN.VALUE = numeric(1)))


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
summary(vapply(FUN = naConvert, porto_train$ps_ind_02_cat, FUN.VALUE = numeric(1)))
# Process the change
for (i in colnames(porto_train)) {
  porto_train[, i] <- vapply(X = porto_train[, i], FUN = naConvert, FUN.VALUE = numeric(1))
}

summary(vapply(X = porto_train[, "ps_ind_02_cat"], FUN = naConvert, FUN.VALUE = numeric(1)))
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

# Converting all factor variables to factor 
porto_train$target <- factor(porto_train$target)
porto_train$ps_ind_01 <- factor(porto_train$ps_ind_01, ordered = T)
porto_train$ps_ind_02_cat <- factor(porto_train$ps_ind_02_cat)
porto_train$ps_ind_03 <- factor(porto_train$ps_ind_03, ordered = T)
porto_train$ps_ind_04_cat <- factor(porto_train$ps_ind_04_cat)
porto_train$ps_ind_05_cat <- factor(porto_train$ps_ind_05_cat)

# Let's explore now each variable one by one

# 1) Variable: id

length(unique(porto_train$id))
length(unique(porto_train$id)) == dim(porto_train)[1]
typeof(porto_train$id)
head(porto_train$id)

#Numeric variable, it takes 595212 unique values so, has its name indicates, it acts as
#an identifier variable. So for prediction purposes it will be useless.

# 2) Variable: target

sum(is.na(porto_train$target))
# Target variable has no missing observations.
summary(porto_train$target)
table(porto_train$target)
round(table(porto_train$target)/length(porto_train$target), 2)
barplot(table(porto_train$target)/length(porto_train$target), col = "steelblue4", 
        xlab = "target", ylab = "frequency", main = "Frequencies of the variable target")


ggplot(as.data.frame(round(table(porto_train$target)/length(porto_train$target), 2)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", fill = "steelblue4",
                                           color = "steelblue4") +
  labs(title = "Frequencies of variable target", x = "target") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# 3) Variable: ps_ind_01

summary(porto_train$ps_ind_01)
hist(porto_train$ps_ind_01)
sum(is.na(porto_train$ps_ind_01))
# Has no missing values
table(porto_train$ps_ind_01)
# Takes discrete values but is no classified has categorical in the variable name
# consult the competition instructions
# From the instructions: "feature names include the postfix bin to indicate binary 
# features and cat to indicate categorical features. 
# Features without these designations are either continuous or ordinal"
#
# Don't have the postfix bin or cat and don't look continuous, so let's asume
# is an ordinal variable
table(porto_train$ps_ind_01)
table(porto_train$ps_ind_01)/length(porto_train$ps_ind_01)*100

ggplot(as.data.frame(round(table(porto_train$ps_ind_01)/length(porto_train$ps_ind_01), 2)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "steelblue4", fill = "steelblue4") +
  labs(title = "Frequencies of variable ps_ind_01", x = "value") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# As can be seen in the plot, the variable has eight different ordered categories, 
# being 0 and 1 the most represented ones.

levels(porto_train$ps_ind_01)
head(porto_train$ps_ind_01)

# 4) Variable: ps_ind_02_cat

# As is indicated in his potfix, this variable is categorical without order.
summary(porto_train$ps_ind_02_cat)
# It has 216 values classified as missing.

# Plot with the relative frequencies without taking care of the na's (incorrect)
ggplot(as.data.frame(round(table(porto_train$ps_ind_02_cat)/length(porto_train$ps_ind_02_cat), 2)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "steelblue4", fill = "steelblue4") +
  labs(title = "Frequencies of variable ps_ind_02_cat", x = "value") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Plot with the absolute frequencies without na's bar
ggplot(as.data.frame(table(porto_train$ps_ind_02_cat)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "steelblue4", fill = "steelblue4") +
  labs(title = "Frequencies of variable ps_ind_02_cat", x = "value") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

summary(porto_train$ps_ind_02_cat)/length(porto_train$ps_ind_02_cat)

# Plot without the na's bar but with the correct frequencies:
ggplot(as.data.frame(round(table(porto_train$ps_ind_02_cat)/length(na.omit(porto_train$ps_ind_02_cat)), 2)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "steelblue4", fill = "steelblue4") +
  labs(title = "Frequencies of variable ps_ind_02_cat", x = "value") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Change in the relative frequencies before and after omitting the missing values
summary(porto_train$ps_ind_02_cat)/length(porto_train$ps_ind_02_cat)
summary(porto_train$ps_ind_02_cat)/length(na.omit(porto_train$ps_ind_02_cat))
# It's not a very significative change since it's a relativelly small number of 
# missings values when compared with the whole variable, but is necessary to take 
# count of this because with variables with more na's the error introduced 
# between the true relative frequency value and the value offered can be big.
#
# Now let's try to plot a new barplot taking account of the na's by adding a new
# bar to the previous plot.

# data.frame for use in the ggplot sentence
auxframe <- data.frame(Var1 = names(summary(porto_train$ps_ind_02_cat)), 
                       Freq = summary(porto_train$ps_ind_02_cat))

# barplot with the absolute frequencies, taking care of the number of na's
ggplot(auxframe,
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "steelblue4", fill = "steelblue4") +
  labs(title = "Frequencies of variable ps_ind_02_cat", x = "value") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# barplot with the relative frequencies, taking care of the number of na's
auxframe2 <- data.frame(Var1 = names(summary(porto_train$ps_ind_02_cat)), 
                       Freq = round(summary(porto_train$ps_ind_02_cat)/length(porto_train$ps_ind_02_cat), 4))
ggplot(auxframe2,
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "steelblue4", fill = "steelblue4") +
  labs(title = "Frequencies of variable ps_ind_02_cat", x = "value") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 5) Variable: ps_ind_03

summary(porto_train$ps_ind_03)
# Has no missing values
hist(porto_train$ps_ind_03)
# His histogram looks discrete instead of continous, so can be an ordinal variable?
table(porto_train$ps_ind_03)
# It looks categorical but not have the potfix cat, so it must be an ordinal variable.
#
# barplot with the absolute frequencies
ggplot(as.data.frame(table(porto_train$ps_ind_03)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "steelblue4", fill = "steelblue4") +
  labs(title = "Frequencies of variable ps_ind_03", x = "value") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
# barplot with the relative frequencies
ggplot(as.data.frame(round(table(porto_train$ps_ind_03)/length(porto_train$ps_ind_03), 2)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "steelblue4", fill = "steelblue4") +
  labs(title = "Frequencies of variable ps_ind_03", x = "value") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# 6) Variable: ps_ind_04_cat

# Has the potfix cat, so it must be categorical without order
summary(porto_train$ps_ind_04_cat)
# It's a binary variable, again we're not going to represent the missing values
# in the plot because there is a very small number of them

# Relative frequencies barplot
ggplot(as.data.frame(round(table(porto_train$ps_ind_04_cat)/length(na.omit(porto_train$ps_ind_04_cat)), 2)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "steelblue4", fill = "steelblue4") +
  labs(title = "Frequencies of variable ps_ind_04_cat", x = "value") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 7) Variable: ps_ind_05_cat

# Potfix cat, so it's also a unordered categorical variable
summary(porto_train$ps_ind_05_cat)
# 5809 missing values
table(porto_train$ps_ind_05_cat)
# Seven different categories, from zero to six.
# Now the number of missing values is a significant one in relation to the size of 
# the other categories so let's add a new bar to the barplot with the number of 
# missing values.
auxframe <- data.frame(Var1 = names(summary(porto_train$ps_ind_05_cat)), 
                       Freq = summary(porto_train$ps_ind_05_cat))

auxframe2 <- data.frame(Var1 = names(summary(porto_train$ps_ind_05_cat)), 
                        Freq = round(summary(porto_train$ps_ind_05_cat)/length(porto_train$ps_ind_05_cat), 3))

auxframe2

ggplot(auxframe2, aes(x = Var1, y = Freq)) + geom_bar(stat = "identity",
                                                     color = "steelblue4", fill = "steelblue4") +
  labs(title = "Frequencies of variable ps_ind_05_cat", x = "value") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

