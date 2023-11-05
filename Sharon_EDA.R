library(tidyverse)
# Every row of ID is unique, setting this as the index.
df <- read.csv("Datasets/X_test_8skS2ey.csv", row.names = "ID")
df
glimpse(df)
summary(df)

# Checking for duplicates in the dataset:
duplicates <- df[duplicated(df),]
duplicates

# Column names:
feature_names <- colnames(df)
feature_names

# number of rows
nrows <- dim(df)[1]
nrows

# number columns
nfeature <- dim(df)[2]
nfeature

# number of unique values in each column
num_unique_values_per_feature <- sapply(df, function(x) length(unique(x)))
num_unique_values_per_feature

# Get a list of the unique values in each feature
unique_values <- lapply(df, unique)

# Print the unique values for each column
for (i in 1: length(unique_values))

  {
  cat("Unique values in column", colnames(df)[i], ":\n")
  print(unique_values[[i]])
}


# Thoughts: first we need to look at the unique values in each column. Are there any columns with
# no actual values? If so, let's drop them. Can any columns be combined or binned?
# Are all of the ID numbers unique? If there are duplicates in the training set, are the target
# variables the same for those indices?
# Cash Price columns are all dbl data type. Is the data consistently double or int?
# make columns appear to be categorical, how many categorical values do we have?
# Since categorical variables must be converted to numeric for the ML algorithm, can we fill in
# empty strings and NA's with 0's?
# It appears that goods codes are actually a mixture of ints and categorical variables. If the
# goods_code is a numeric item code for the item do we need both?
# Number of prod purchase-> if there is no better solution we should be able to replace NA's with
# 0's. Also, that's double, should we retype as int?
# Same with Nb_of_items.
# Histograms for the categorical variables.
# Also want to see scatter plots. Concatenate the target data back into the training set and
# create a scatter plot of it vs each feature.
# RMSE is the metric used on the competition. Is this the correct metric for our needs as well?
# Let's try not to eliminate any rows of data because the target variable is separate from the
# training data so eliminating any rows will make the result invalid.


