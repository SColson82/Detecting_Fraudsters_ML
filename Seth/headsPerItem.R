# Libraries
library(dplyr)
library(reshape2)

# Read csv file into a dataframe
xTestData <- read.csv("/Users/seththompson/data science project/Detecting_Fraudsters_ML/Seth/Datasets/X_test_8skS2ey.csv")

# reference code for reading the other 3 csv files
xTrainData <- read.csv("/Users/seththompson/data science project/Detecting_Fraudsters_ML/Seth/Datasets/X_train_G3tdtEn.csv")
# yTestData <- read.csv("/Users/seththompson/data science project/Detecting_Fraudsters_ML/Seth/Datasets/Y_test_random_2.csv")
# yTrainData <- read.csv("/Users/seththompson/data science project/Detecting_Fraudsters_ML/Seth/Datasets/Y_train_2_XPXJDyy.csv")

# reference code for formating how the head of a dataframe is printed
# format(head(xTestData,1), justift = "left", scientific = FALSE)

# Create 24 empty dataframes
dfs <- listOfDataFrames <- replicate(24, data.frame(), simplify = FALSE)

# Iterate through the columns and copy data to the corresponding empty dataframe
n <- 0
for (i in seq_along(dfs)) {
  dfs[[i]] <- xTestData[, c(2+n,26+n,50+n,74+n,98+n,122+n)]
  n <- n+1
}

# reference code for getting all columns that correspond with item1
# colIndexes <- c(2,26,50,74,98,122)

# Get heads of all 24 dataframes
headDfs <- lapply(dfs, head)

# Print heads with justified left formatting
n <- 1
for (i in seq_along(headDfs)) {
  print(paste("dataframe",n,"head"))
  print(format(headDfs[[i]],justify = "centre", scientific = FALSE))
  n <- n + 1
}

glimpse(xTrainData)
