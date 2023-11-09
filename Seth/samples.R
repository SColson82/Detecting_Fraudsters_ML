# Libraries
library(dplyr)
library(reshape2)

setwd("~")

# Read csv file into a dataframe

# lists purchases
xTestData <- read.csv("Detecting_Fraudsters_ML/Seth/Datasets/X_test_8skS2ey.csv")
xTrainData <- read.csv("Detecting_Fraudsters_ML/Seth/Datasets/X_train_G3tdtEn.csv")

#lists fraud flags
yTestData <- read.csv("Detecting_Fraudsters_ML/Seth/Datasets/Y_test_random_2.csv")
yTrainData <- read.csv("Detecting_Fraudsters_ML/Seth/Datasets/Y_train_2_XPXJDyy.csv")

# merges test dataframes based off of ID
mergeTestData <- merge(xTestData,yTestData,by="ID")

# first 100 rows
sampleMergedTestData <- head(mergeTestData[, c(2,26,50,74,98,122,146,148)],100)

#displays rows in sample where fraud flag is over 90%
print("fraud Test:")
fraudTestSample <- sampleMergedTestData[sampleMergedTestData$fraud_flag > 0.90,]
print(fraudTestSample)

#displays rows in sample where fraud flag is under 10%
print("not fraud Test:")
notFraudTestSample <- sampleMergedTestData[sampleMergedTestData$fraud_flag < 0.10,]
print(notFraudTestSample)

# merges train dataframes based off of ID
mergeTrainData <- merge(xTrainData,yTrainData,by="ID")

# first 100 rows
sampleMergedTrainData <- head(mergeTrainData[, c(2,26,50,74,98,122,146,148)],100)

#displays rows in sample where fraud flag is over 90%
print("fraud Train:")
fraudTrainSample <- sampleMergedTrainData[sampleMergedTrainData$fraud_flag > 0.90,]
print(fraudTrainSample)

# According to the training data, a purchase is marked fraudulent if the goods_code is not unique

#displays rows in sample where fraud flag is under 10%
print("not fraud Train:")
notFraudSample <- sampleMergedTestData[sampleMergedTestData$fraud_flag < 0.10,]
print(notFraudSample)



