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

df <- read.csv("X_train_G3tdtEn.csv", row.names = "ID")
df


cashPerOrder <- c(na.omit(df$cash_price1 + df$cash_price2 + df$cash_price3 + df$cash_price4
                       +df$cash_price5 + df$cash_price6 + df$cash_price7 + df$cash_price8
                       +df$cash_price9 + df$cash_price10 + df$cash_price11
                       +df$cash_price12 + df$cash_price13 + df$cash_price13
                       +df$cash_price14 + df$cash_price15 + df$cash_price16
                       +df$cash_price17 + df$cash_price18 + df$cash_price19
                       +df$cash_price20 + df$cash_price21 + df$cash_price22
                       +df$cash_price23 + df$cash_price24))


cashPerOrder


boxplot(cashPerOrder)
