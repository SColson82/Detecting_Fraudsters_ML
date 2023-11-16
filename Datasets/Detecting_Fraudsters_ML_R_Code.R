#This will be used for this class's project. 

library(ROCR)

setwd("C:/Users/gavmc/Documents/Detecting_Fraudsters_ML/Datasets")
print(getwd())

xtrain <- read.csv("X_train_G3tdtEn.csv")
xtest <- read.csv("X_test_8skS2ey.csv")
ytrain <- read.csv("Y_train_2_XPXJDyy.csv")
ytest <- read.csv("Y_test_random_2.csv")

summary(xtrain)
summary(ytrait)

summary(xtest)
summary(ytest)
