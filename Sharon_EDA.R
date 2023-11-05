library(tidyverse)
library(dplyr)
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

# Specify the file path where you want to save the output
output_file <- "unique_values_output.txt"

# Open the file for writing
file_conn <- file(output_file, "w")

# Loop through each column and print unique values to the file
for (i in 1:length(unique_values)) {
  cat("\nUnique values in column", colnames(df)[i], ":\n", file = file_conn)
  cat(unique_values[[i]], file = file_conn)
  cat("Unique values in column", colnames(df)[i], ":\n")
  print(unique_values[[i]])
}

# Close the file
close(file_conn)

cat("Unique values have been written to", output_file, "\n")

# Check data types in each column
data_types <- sapply(df, class)
data_types

# I'm thinking of replacing the NAs in all of the cash_price features with 0s but
# I noticed that it appears that 0 is a used value so I want to look where that is the
# case and try to figure out why.
has_zero <- df %>% filter(any(df == 0)==TRUE)
has_zero
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


