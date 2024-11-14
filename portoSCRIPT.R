#########################################################################
# This script is intended for helping with the portoMKDWN.Rmd document. #
#########################################################################

### TO DO: Convert all variables that are factors as it, taking care of the ordinals
###        Change the color's barplot for the target variable

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
porto_train$ps_ind_06_bin <- factor(porto_train$ps_ind_06_bin)
porto_train$ps_ind_07_bin <- factor(porto_train$ps_ind_07_bin)
porto_train$ps_ind_08_bin <- factor(porto_train$ps_ind_08_bin)
porto_train$ps_ind_09_bin <- factor(porto_train$ps_ind_09_bin)
porto_train$ps_ind_10_bin <- factor(porto_train$ps_ind_10_bin)
porto_train$ps_ind_11_bin <- factor(porto_train$ps_ind_11_bin)
porto_train$ps_ind_12_bin <- factor(porto_train$ps_ind_12_bin)
porto_train$ps_ind_13_bin <- factor(porto_train$ps_ind_13_bin)
porto_train$ps_ind_14 <- factor(porto_train$ps_ind_14, ordered = T)
porto_train$ps_ind_15 <- factor(porto_train$ps_ind_15, ordered = T)
porto_train$ps_ind_16_bin <- factor(porto_train$ps_ind_16_bin)
porto_train$ps_ind_17_bin <- factor(porto_train$ps_ind_17_bin)
porto_train$ps_ind_18_bin <- factor(porto_train$ps_ind_18_bin)
porto_train$ps_reg_01 <- factor(porto_train$ps_reg_01, ordered = T)
porto_train$ps_reg_02 <- factor(porto_train$ps_reg_02, ordered = T)
porto_train$ps_car_01_cat <- factor(porto_train$ps_car_01_cat)
porto_train$ps_car_02_cat <- factor(porto_train$ps_car_02_cat)
porto_train$ps_car_03_cat <- factor(porto_train$ps_car_03_cat)
porto_train$ps_car_04_cat <- factor(porto_train$ps_car_04_cat)
porto_train$ps_car_05_cat <- factor(porto_train$ps_car_05_cat)
porto_train$ps_car_06_cat <- factor(porto_train$ps_car_06_cat)
porto_train$ps_car_07_cat <- factor(porto_train$ps_car_07_cat)
porto_train$ps_car_08_cat <- factor(porto_train$ps_car_08_cat)
porto_train$ps_car_09_cat <- factor(porto_train$ps_car_09_cat)
porto_train$ps_car_10_cat <- factor(porto_train$ps_car_10_cat)
porto_train$ps_car_11_cat <- factor(porto_train$ps_car_11_cat)
porto_train$ps_car_11 <- factor(porto_train$ps_car_11, ordered = T)
porto_train$ps_car_15 <- factor(porto_train$ps_car_15, ordered = T)

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

# 8) Variable: ps_ind_06_bin

# Has the postfix bin so it's a categorical variable with only two categories.
summary(porto_train$ps_ind_06_bin)
# Has no missing values
table(porto_train$ps_ind_06_bin)

# Relatve frequencies barplot
ggplot(as.data.frame(round(table(porto_train$ps_ind_06_bin)/length(porto_train$ps_ind_06_bin), 2)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "steelblue4", fill = "steelblue4") +
  labs(title = "Frequencies of variable ps_ind_06_bin", x = "value") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 61 % of the observations take the category 0 and 39 % take the category 1.

# 9) Variable: ps_ind_07_bin

# Also a categorical variable with only two categories.
summary(porto_train$ps_ind_07_bin)
# Has no missing values.
table(porto_train$ps_ind_07_bin)

# Relatve frequencies barplot
ggplot(as.data.frame(round(table(porto_train$ps_ind_07_bin)/length(porto_train$ps_ind_07_bin), 2)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "steelblue4", fill = "steelblue4") +
  labs(title = "Frequencies of variable ps_ind_07_bin", x = "value") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 10) Variable: ps_ind_08_bin

summary(porto_train$ps_ind_08_bin)
# Variable has no missing values
table(porto_train$ps_ind_08_bin)

# Relative frequencies barplot

ggplot(as.data.frame(round(table(porto_train$ps_ind_08_bin)/length(porto_train$ps_ind_08_bin), 2)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "steelblue4", fill = "steelblue4") +
  labs(title = "Frequencies of variable ps_ind_08_bin", x = "value") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# 11) Variable: ps_ind_09_bin

summary(porto_train$ps_ind_09_bin)
# No missings
table(porto_train$ps_ind_09_bin)
# Correct, is a categorical variable, can be changed from numeric to factor

ggplot(as.data.frame(round(table(porto_train$ps_ind_09_bin)/length(porto_train$ps_ind_09_bin), 2)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "steelblue4", fill = "steelblue4") +
  labs(title = "Frequencies of variable ps_ind_09_bin", x = "value") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# 12) Variable: ps_ind_10_bin

summary(porto_train$ps_ind_10_bin)
# No missings.
table(porto_train$ps_ind_10_bin)
# Correct, it's a binary factorial variable, and can be converted without worries
# from numerical to factor.

ggplot(as.data.frame(table(porto_train$ps_ind_10_bin)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "steelblue4", fill = "steelblue4") +
  labs(title = "Frequencies of variable ps_ind_10_bin", x = "value") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# 13) Variable: ps_ind_11_bin

summary(porto_train$ps_ind_11_bin)
# No missings.
table(porto_train$ps_ind_11_bin)
# Can be changed to factor without problem

# Relative frequencies barplot

ggplot(as.data.frame(table(porto_train$ps_ind_11_bin)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "steelblue4", fill = "steelblue4") +
  labs(title = "Frequencies of variable ps_ind_11_bin", x = "value") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# 14) Variable: ps_ind_12_bin
summary(porto_train$ps_ind_12_bin)
# No missing values
table(porto_train$ps_ind_12_bin)
# It's really a categorical variable, so can be changed from numeric to factor without problem
table(porto_train$ps_ind_12_bin)/length(porto_train$id)*100

# Barplot of the absolute frequencies
ggplot(as.data.frame(table(porto_train$ps_ind_12_bin)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "steelblue4", fill = "steelblue4") +
  labs(title = "Frequencies of variable ps_ind_12_bin", x = "value") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 15) Variable: ps_ind_13_bin

summary(porto_train$ps_ind_13_bin)
# No missing values

table(porto_train$ps_ind_13_bin)
table(porto_train$ps_ind_13_bin)/length(porto_train$id)*100

# Barplot of the absolute frequencies
ggplot(as.data.frame(table(porto_train$ps_ind_13_bin)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "steelblue4", fill = "steelblue4") +
  labs(title = "Frequencies of variable ps_ind_13_bin", x = "value") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 16) Variable: ps_ind_14

summary(porto_train$ps_ind_14)
# The variable has no missing values.
hist(porto_train$ps_ind_14)
# Histogram looks like the one of a categorical variable
table(porto_train$ps_ind_14)
# Confirmed is categorical and not have the potfix cat nor bin, so it must have
# an ordinal character
#
# Let's obtain now the relative frequencies
round(table(porto_train$ps_ind_14)/length(porto_train$id), 3)*100

# Absolute frequencies barplot
ggplot(as.data.frame(table(porto_train$ps_ind_14)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "steelblue4", fill = "steelblue4") +
  labs(title = "Frequencies of variable ps_ind_14", x = "value") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 17) Variable: ps_ind_15

summary(porto_train$ps_ind_15)
# Has no missing values.

hist(porto_train$ps_ind_15)
# Looks categorical and didn't have the potfix _cat or _bin
table(porto_train$ps_ind_15)
# Looks like there are 14 categories from zero to thirteen, so it's type can be
# changed from numeric to format with order.

head(porto_train$ps_ind_15)
ggplot(as.data.frame(round(table(porto_train$ps_ind_15)/length(porto_train$id), 2)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "steelblue4", fill = "steelblue4") +
  labs(title = "Frequencies of variable ps_ind_15", x = "value") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 18) Variable: ps_ind_16_bin

summary(porto_train$ps_ind_16_bin)
# Has no missing values.
# Has the name indicates it's a binomial variable, so it's type should be transformed from
# numeric to factor
table(porto_train$ps_ind_16_bin)
# Confirmed, only takes 0 or 1 values.

# Relative frequencies barplot
ggplot(as.data.frame(round(table(porto_train$ps_ind_16_bin)/length(porto_train$ps_ind_16_bin), 2)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "steelblue4", fill = "steelblue4") +
  labs(title = "Frequencies of variable ps_ind_16_bin", x = "value") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 19) Variable: ps_ind_17_bin

# This variable also should be a binomial one.
summary(porto_train$ps_ind_17_bin)
# Has no missings
table(porto_train$ps_ind_17_bin)
# Ok, only takes two values

# Relative frequencies barplot
ggplot(as.data.frame(round(table(porto_train$ps_ind_17_bin)/length(porto_train$ps_ind_16_bin), 2)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "steelblue4", fill = "steelblue4") +
  labs(title = "Frequencies of variable ps_ind_17_bin", x = "value") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# 20) Variable: ps_ind_18_bin

# Another binomial variable.
summary(porto_train$ps_ind_18_bin)
# Has no missing values
table(porto_train$ps_ind_18_bin)
# Confirmed, only takes two values

# Relative frequencies barplot
ggplot(as.data.frame(round(table(porto_train$ps_ind_18_bin)/length(porto_train$ps_ind_16_bin), 2)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "steelblue4", fill = "steelblue4") +
  labs(title = "Frequencies of variable ps_ind_18_bin", x = "value") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 21) Variable: ps_reg_01

head(porto_train$ps_reg_01)
summary(porto_train$ps_reg_01)
# Has no missing values
length(table(porto_train$ps_reg_01))
# Only ten different values
hist(porto_train$ps_reg_01)
# Seems factorial
table(porto_train$ps_reg_01)
# Relative frequencies
round(table(porto_train$ps_reg_01)/length(porto_train$ps_reg_01), 2)

# Relative frequencies barplot
ggplot(as.data.frame(round(table(porto_train$ps_reg_01)/length(porto_train$ps_reg_01), 2)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "steelblue4", fill = "steelblue4") +
  labs(title = "Frequencies of variable ps_reg_01", x = "value") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Histogram
ggplot(porto_train, aes(x = ps_reg_01)) + geom_histogram(bins = 10, fill = "gray9") +
  theme_dark() + labs(title = "Histogram of variable ps_reg_01", x = "value") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 22) Variable: ps_reg_02

head(porto_train$ps_reg_02)
summary(porto_train$ps_reg_02)
# Has no missings.
# Looks like a continous variable.
length(table(porto_train$ps_reg_02))
# Only have 19 different values for 595212 observations.
table(porto_train$ps_reg_02)
hist(porto_train$ps_reg_02)
# Looks continuous

# Relative frequencies barplot

barplot(table(porto_train$ps_reg_02))

# Relative frequencies barplot
ggplot(as.data.frame(round(table(porto_train$ps_reg_02)/length(porto_train$ps_reg_02), 3)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "steelblue4", fill = "steelblue4") +
  labs(title = "Frequencies of variable ps_reg_02", x = "value") + theme_classic() +
  geom_text(aes(label = Freq), hjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) + coord_flip()

# 23) Variable: ps_reg_03

summary(porto_train$ps_reg_03)[7]
# This variable has 107772 missing values.
length(table(porto_train$ps_reg_03))
# Variable takes 5012 different values.
# This variable is actually continuous.
missingFrame <- data.frame(observed = length(porto_train$ps_reg_03[which(!is.na(porto_train$ps_reg_03))]), 
           missing = sum(is.na(porto_train$ps_reg_03)))

sum(missingFrame)
# Result is correct (equals the number of rows)
missingFrame/length(porto_train$ps_reg_03)

# Histogram
ggplot(porto_train, aes(x = ps_reg_03)) + geom_histogram(color = "white", fill = "gray16") +
  theme_dark() + labs(title = "Histogram of variable ps_reg_03", x = "value") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Option one, plot the histogram and below a summary table but extended with more quantiles.
# Option two, a boxplot and below the summary extended.
# Option three, put both and the summary information extended below.

# Boxplot
ggplot(porto_train, aes(x = ps_reg_03)) + geom_boxplot(color = "mediumseagreen") + coord_flip() +
  labs(title = "Boxplot of variable ps_reg_03", x = "value") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

quantile(porto_train$ps_reg_03, na.rm = T, probs = seq(0, 1, 0.1))

# Quantiles in column format
print.data.frame(data.frame(quantile = names(quantile(porto_train$ps_reg_03, na.rm = T,
                                     probs = seq(0, 1, 0.1))),
           value = unname(quantile(porto_train$ps_reg_03, na.rm = T, probs = seq(0, 1, 0.1)))), 
           row.names = F)

# Widening the number of quantiles

quantile(porto_train$ps_reg_03, probs = c(seq(0, 0.9, 0.1), seq(0.9, 1, 0.01)), na.rm = T)

print.data.frame(data.frame(quantile = names(quantile(porto_train$ps_reg_03, na.rm = T, 
                                                      probs = c(seq(0, 0.9, 0.1), 
                                                                seq(0.9, 1, 0.01)))),
                            value = unname(quantile(porto_train$ps_reg_03, na.rm = T, 
                                                    probs = c(seq(0, 0.9, 0.1),
                                                              seq(0.9, 1, 0.01))))), 
                 row.names = F)

# How many values are bigger than 99 % quantile?

length(porto_train$ps_reg_03[which(porto_train$ps_reg_03 > 1.88)])
# 4910
summary(porto_train$ps_reg_03[which(porto_train$ps_reg_03 > 1.88)])
quantile(porto_train$ps_reg_03[which(porto_train$ps_reg_03 > 1.88)], probs = seq(0, 1, 0.1))
quantile(porto_train$ps_reg_03[which(porto_train$ps_reg_03 > 1.88)], probs = c(seq(0, 0.9, 0.1), seq(0.9, 1, 0.01)))
# 4.04 seems like really extreme observation
# How many observations are bigger than 2.77=
length(porto_train$ps_reg_03[which(porto_train$ps_reg_03 > 2.77)])
# 49
summary(porto_train$ps_reg_03[which(porto_train$ps_reg_03 > 2.77)])
sort(porto_train$ps_reg_03[which(porto_train$ps_reg_03 > 2.77)])
# Value of the target variable for this observations?
porto_train$target[which(porto_train$ps_reg_03 > 2.77)]
table(porto_train$target[which(porto_train$ps_reg_03 > 2.77)])
table(porto_train$target[which(porto_train$ps_reg_03 > 2.77)])/49
# orginal value
table(porto_train$target)/length(porto_train$target)
(0.08163265 - 0.03644752)/0.03644752
((0.08163265 - 0.03644752)/0.03644752)*100
# A 124 % increment

# 24) Variable: ps_car_01_cat

summary(porto_train$ps_car_01_cat)
# This variable has 107 missing values, a very small number when compared to the
# size of the dataset, so we're not going to include a bar with them into the variable's barplot.
length(table(porto_train$ps_car_01_cat))
table(porto_train$ps_car_01_cat)

ggplot(as.data.frame(round(table(porto_train$ps_car_01_cat)/length(na.omit(porto_train$ps_car_01_cat)), 3)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "steelblue4", fill = "steelblue4") +
  labs(title = "Frequencies of variable ps_car_01_cat", x = "value") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 25) Variable: ps_car_02_cat

summary(porto_train$ps_car_02_cat)
# Has 5 missings
length(table(porto_train$ps_car_02_cat))
# Takes only two different values

# Relative frequencies barplot
ggplot(as.data.frame(round(table(porto_train$ps_car_02_cat)/length(na.omit(porto_train$ps_car_02_cat)), 3)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "steelblue4", fill = "steelblue4") +
  labs(title = "Frequencies of variable ps_car_02_cat", x = "value") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 26) Variable: ps_car_03_cat

summary(porto_train$ps_car_03_cat)
# This variable has 411231 missing values, a big number so let's represent it into
# the next barplot
length(table(porto_train$ps_car_03_cat))
# Only two categories
# Relative frequencies of the missing values
summary(porto_train$ps_car_03_cat)/length(porto_train$ps_car_03_cat)
# Auxiliar dataframe for using in the ggplot sentence
auxframe <- data.frame(Var1 = names(summary(porto_train$ps_car_03_cat)),
                       Freq = round(summary(porto_train$ps_car_03_cat)/length(porto_train$ps_car_03_cat), 2))

ggplot(auxframe,
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "steelblue4", fill = "steelblue4") +
  labs(title = "Frequencies of variable ps_car_03_cat", x = "value") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Barplot without the missing values bar
ggplot(as.data.frame(round(table(porto_train$ps_car_03_cat)/length(na.omit(porto_train$ps_car_03_cat)), 3)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "steelblue4", fill = "steelblue4") +
  labs(title = "Frequencies of variable ps_car_03_cat", x = "value") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 27) Variable: ps_car_04_cat

summary(porto_train$ps_car_04_cat)
# This variable has no missing values
length(table(porto_train$ps_car_04_cat))
# Ten different categories
table(porto_train$ps_car_04_cat)

# Relative frequencies barplot
ggplot(as.data.frame(round(table(porto_train$ps_car_04_cat)/length(porto_train$ps_car_04_cat), 2)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "steelblue4", fill = "steelblue4") +
  labs(title = "Frequencies of variable ps_car_04_cat", x = "value") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# This plot better with absolute frequencies, and the relative ones in the summary line
summary(porto_train$ps_car_04_cat)/length(porto_train$ps_car_05_cat)

# Absolute frequencies barplot
ggplot(as.data.frame(table(porto_train$ps_car_04_cat)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "steelblue4", fill = "steelblue4") +
  labs(title = "Frequencies of variable ps_car_04_cat", x = "value") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# 28) Variable: ps_car_05_cat

summary(porto_train$ps_car_05_cat)
# This variable has 266551 missing values
# How many different categories?
length(table(porto_train$ps_car_05_cat))
# Two different categories
table(porto_train$ps_car_05_cat)

# Auxiliar dataframe for passing it into the ggplot function
auxframe <- data.frame(Var1 = names(summary(porto_train$ps_car_05_cat)),
                       Freq = round(summary(porto_train$ps_car_05_cat)/length(porto_train$ps_car_05_cat), 2))

# Relative frequencies barplot
ggplot(auxframe,
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "steelblue4", fill = "steelblue4") +
  labs(title = "Frequencies of variable ps_car_04_cat", x = "value") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Relative frequencies barplot without missings bar
ggplot(as.data.frame(round(table(porto_train$ps_car_05_cat)/length(na.omit(porto_train$ps_car_05_cat)), 2)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "steelblue4", fill = "steelblue4") +
  labs(title = "Frequencies of variable ps_car_05_cat", x = "value") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# 29) Variable: ps_car_06_cat

summary(porto_train$ps_car_06_cat)
# This variable has no missing values
# How many categories it has?
length(table(porto_train$ps_car_06_cat))
# 18 different categories
table(porto_train$ps_car_06_cat)

# Relative frequencies barplot
ggplot(as.data.frame(round(table(porto_train$ps_car_06_cat)/length(porto_train$ps_car_06_cat), 3)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "steelblue4", fill = "steelblue4") +
  labs(title = "Frequencies of variable ps_car_06_cat", x = "value") + theme_classic() +
  geom_text(aes(label = Freq), hjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) + coord_flip()
# PUT THIS PLOT HORIZONTAL

# Absolute frequencies barplot
ggplot(as.data.frame(table(porto_train$ps_car_06_cat)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "steelblue4", fill = "steelblue4") +
  labs(title = "Frequencies of variable ps_car_06_cat", x = "value") + theme_classic() +
  geom_text(aes(label = Freq), hjust = 0.5) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) + coord_flip()

print.data.frame(data.frame(category = names(sort(summary(porto_train$ps_car_06_cat), decreasing = T)),
           frequency = round(100*sort(summary(porto_train$ps_car_06_cat)/length(porto_train$ps_car_06_cat),
                            decreasing = T), 2), row.names = NULL), row.names = F)

# 30) Variable: ps_car_07_cat
summary(porto_train$ps_car_07_cat)
# This variable has 11489 missing values
# How many categories it has?
length(table(porto_train$ps_car_07_cat))
# Only two categories
table(porto_train$ps_car_07_cat)
# Auxiliar dataframe for passing into the ggplot function
auxframe <- data.frame(Var1 = names(summary(porto_train$ps_car_07_cat)),
                       Freq = round(summary(porto_train$ps_car_07_cat)/length(porto_train$ps_car_07_cat), 2))

# Relative frequencies barplot
ggplot(auxframe,
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "steelblue4", fill = "steelblue4") +
  labs(title = "Frequencies of variable ps_car_07_cat", x = "value") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Relative frequencies barplot (without NA's bar)
ggplot(as.data.frame(round(table(porto_train$ps_car_07_cat)/length(na.omit(porto_train$ps_car_07_cat)), 3)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "steelblue4", fill = "steelblue4") +
  labs(title = "Frequencies of variable ps_car_07_cat", x = "value") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Absolute frequencies barplot
ggplot(as.data.frame(table(porto_train$ps_car_07_cat)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "steelblue4", fill = "steelblue4") +
  labs(title = "Frequencies of variable ps_car_07_cat", x = "value") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 31) Variable: ps_car_08_cat

summary(porto_train$ps_car_08_cat)
# This variable has no missing values
# How many different categories?
length(table(porto_train$ps_car_08_cat))
# Only two categories
table(porto_train$ps_car_08_cat)

# Relative frequencies barplot
ggplot(as.data.frame(round(table(porto_train$ps_car_08_cat)/length(porto_train$ps_car_08_cat), 2)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "steelblue4", fill = "steelblue4") +
  labs(title = "Frequencies of variable ps_car_08_cat", x = "value") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 32) Variable: ps_car_09_cat

summary(porto_train$ps_car_09_cat)
# This variable has 569 missing values
# How many different categories?
length(table(porto_train$ps_car_09_cat))
# 5 different categories
table(porto_train$ps_car_09_cat)

# Let's represent the relative frequencies barplot but without the NA's bar
# Relative frequencies barplot
ggplot(as.data.frame(round(table(porto_train$ps_car_09_cat)/length(na.omit(porto_train$ps_car_09_cat)), 3)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "steelblue4", fill = "steelblue4") +
  labs(title = "Frequencies of variable ps_car_09_cat", x = "value") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 33) Variable: ps_car_10_cat

summary(porto_train$ps_car_10_cat)
# This variable has no missing values
table(porto_train$ps_car_10_cat)
# Has three categories
ggplot(as.data.frame(table(porto_train$ps_car_10_cat)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "steelblue4", fill = "steelblue4") +
  labs(title = "Frequencies of variable ps_car_10_cat", x = "value") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 34) Variable: ps_car_11_cat

summary(porto_train$ps_car_11_cat)
# Has no missing values
# How many categories?
length(table(porto_train$ps_car_11_cat))
# 104 different categories
table(porto_train$ps_car_11_cat)

# Testing barplot 
barplot(table(porto_train$ps_car_11_cat))
# Too many bars, not too useful, better present a table
# Let's try ggplot just to see how it looks
ggplot(as.data.frame(table(porto_train$ps_car_11_cat)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "steelblue4", fill = "steelblue4") +
  labs(title = "Frequencies of variable ps_car_11_cat", x = "value") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
# Maybe an horizontal one...

print.data.frame(data.frame(category = names(sort(summary(porto_train$ps_car_11_cat), decreasing = T)),
                            frequency = round(100*sort(summary(porto_train$ps_car_11_cat)/length(porto_train$ps_car_11_cat),
                                                       decreasing = T), 2), row.names = NULL), row.names = F)

# 35) Variable: ps_car_11

summary(porto_train$ps_car_11)
# This variable has 5 missing values.
# This variable looks like it should be continuous one since it has no potfix.
# Let's look how many different values it gets.
length(table(porto_train$ps_car_11))
# Only four different values, so it should be an ordinal variable.
table(porto_train$ps_car_11)
head(porto_train$ps_car_11)

# Relative frequencies barplot
ggplot(as.data.frame(round(table(porto_train$ps_car_11)/length(na.omit(porto_train$ps_car_11)), 2)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "steelblue4", fill = "steelblue4") +
  labs(title = "Frequencies of variable ps_car_11", x = "value") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 36) Variable: ps_car_12

summary(porto_train$ps_car_12)
# This variable has only one missing value.
# Seems has can be a continuous varirable, let's see how many different values it takes
length(table(porto_train$ps_car_12))
# 183 different values
# let's explore its histogram
hist(porto_train$ps_car_12)
head(porto_train$ps_car_12)
# Ok, it's a continuous variable

# Histogram
ggplot(porto_train, aes(x = ps_car_12)) + geom_histogram(color = "white", fill = "gray16") +
  theme_dark() + labs(title = "Histogram of variable ps_car_12", x = "value") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Boxplot
ggplot(porto_train, aes(x = ps_car_12)) + geom_boxplot(color = "mediumseagreen") + coord_flip() +
  labs(title = "Boxplot of variable ps_car_12", x = "value") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

quantile(porto_train$ps_car_12, na.rm = T, probs = seq(0, 1, 0.1))
quantile(porto_train$ps_car_12, na.rm = T, probs = seq(0.9, 1, 0.01))

# 37) Variable: ps_car_13

summary(porto_train$ps_car_13)
# This variable has no missing values
# Looks continuous, let's see how many different values it takes
length(table(porto_train$ps_car_13))
# 704482 different values, deffinetly a continuous variable
head(porto_train$ps_car_13)

# Histogram
ggplot(porto_train, aes(x = ps_car_13)) + geom_histogram(color = "white", fill = "gray16") +
  theme_dark() + labs(title = "Histogram of variable ps_car_13", x = "value") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Boxplot
ggplot(porto_train, aes(x = ps_car_13)) + geom_boxplot(color = "mediumseagreen") + coord_flip() +
  labs(title = "Boxplot of variable ps_car_13", x = "value") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

summary(porto_train$ps_car_13)
quantile(porto_train$ps_car_13, na.rm = T, probs = seq(0, 1, 0.1))
quantile(porto_train$ps_car_13, na.rm = T, probs = seq(0.9, 1, 0.01))

# 38) Variable: ps_car_14

summary(porto_train$ps_car_14)
# This variable has 42620 missing values.
length(table(porto_train$ps_car_14))
# 849 different values
head(porto_train$ps_car_14)
# Let's take it has continuous

# Histogram
ggplot(porto_train, aes(x = ps_car_14)) + geom_histogram(color = "white", fill = "gray16") +
  theme_dark() + labs(title = "Histogram of variable ps_car_14", x = "value") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Boxplot
ggplot(porto_train, aes(x = ps_car_14)) + geom_boxplot(color = "mediumseagreen") + coord_flip() +
  labs(title = "Boxplot of variable ps_car_14", x = "value") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

summary(porto_train$ps_car_14)
quantile(porto_train$ps_car_14, na.rm = T, probs = seq(0, 1, 0.1))
quantile(porto_train$ps_car_14, na.rm = T, probs = seq(0.9, 1, 0.01))


# 39) Variable: ps_car_15

summary(porto_train$ps_car_15)
# Has no missing values
head(porto_train$ps_car_15)
# Looks like a coninuous variable, let's see how many different values it takes.
length(table(porto_train$ps_car_15))
# 15 different values
# Let's check it's histogram
hist(porto_train$ps_car_15)
# Now it's barplot
barplot(table(porto_train$ps_car_15))

# Histogram
ggplot(porto_train, aes(x = ps_car_15)) + geom_histogram(color = "white", fill = "gray16") +
  theme_dark() + labs(title = "Histogram of variable ps_car_15", x = "value") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Boxplot
ggplot(porto_train, aes(x = ps_car_15)) + geom_boxplot(color = "mediumseagreen") + coord_flip() +
  labs(title = "Boxplot of variable ps_car_15", x = "value") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Relative frequencies barplot
ggplot(as.data.frame(round(table(porto_train$ps_car_15)/length(porto_train$ps_car_15), 2)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "steelblue4", fill = "steelblue4") +
  labs(title = "Frequencies of variable ps_car_15", x = "value") + theme_classic() +
  geom_text(aes(label = Freq), hjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) + coord_flip()

# 40) Variable: ps_calc_01

summary(porto_train$ps_calc_01)
# No missing values
# Let's check how many different values it takes

# 41) Variable: ps_calc_02


# 42) Variable: ps_calc_03


# 43) Variable: ps_calc_04


# 44) Variable: ps_calc_05


# 45) Variable: ps_calc_06


# 46) Variable: ps_calc_07


# 47) Variable: ps_calc_08


# 48) Variable: ps_calc_09


# 49) Variable: ps_calc_10


# 50) Variable: ps_calc_11


# 51) Variable: ps_calc_12


# 52) Variable: ps_calc_13


# 53) Variable: ps_calc_14
