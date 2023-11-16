# Libraries
library(dplyr)
library(reshape2)

setwd("~")

# Read csv file into a dataframe

# lists purchases
xTestData <- read.csv("Detecting_Fraudsters_ML/Seth/Datasets/X_test_8skS2ey.csv")

#lists fraud flags
yTestData <- read.csv("Detecting_Fraudsters_ML/Seth/Datasets/Y_test_random_2.csv")

# merges test dataframes based off of ID
mergeTestData <- merge(xTestData,yTestData,by="ID")

highlyFraudulent <- mergeTestData[mergeTestData$fraud_flag > 0.80,]
head(highlyFraudulent)