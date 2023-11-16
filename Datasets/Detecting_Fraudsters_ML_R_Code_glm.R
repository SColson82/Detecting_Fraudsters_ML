#This will be used for this class's project. 

library(ROCR)
library(data.table)

setwd("C:/Users/gavmc/Documents/Detecting_Fraudsters_ML/Datasets")
print(getwd())

xtrain <- read.csv("X_train_G3tdtEn.csv")
xtest <- read.csv("X_test_8skS2ey.csv")
ytrain <- read.csv("Y_train_2_XPXJDyy.csv")
ytest <- read.csv("Y_test_random_2.csv")

summary(xtrain)
summary(ytrain)

summary(xtest)
summary(ytest)

df <- read.csv("X_train_G3tdtEn.csv", row.names = "ID")
df

df2 <- read.csv("Y_train_2_XPXJDyy.csv", row.names = "ID")

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

summary(cashPerOrder)

df

cashPerOrder <- rowSums(df[, c(25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48)], na.rm = TRUE)

cashPerOrder

df3 <- df

df3 <- df[,-25:-48]

df3

df3$cashTotPrice <- cashPerOrder

df3


boxplot(cashPerOrder)
#putting the y_train data and leaving out the index var

#newYTrain <- c(df2$ID, df2$fraud_flag)

#newYTrain

#as.data.frame(newYTrain)

#newYTrain

#that was incorrect. 

#newYTrain <- c(ytrain$ID, ytrain$fraud_flag)

#newYTrain[111, 111]


df5 <- read.csv("unique_item_counts_fraud_flag.csv")

df5

#This should combine the several unique values that are similar.
df5[df5 == "BABY & CHILD TRAVEL" | df5 == "BABY CHANGING" | df5 == "BABY CHILD TRAVEL" | df5 == "BABY FEEDING" | df5 == "BABY PLAY EQUIPMENT" | df5 == "BABYWEAR" ] <- "Baby"

df5[df5 == "BAGS & CARRY CASES" | df5 == "BAGS CARRY CASES" | df5 == "BAGS WALLETS ACCESSORIES" | df5 == "BAGS, WALLETS & ACCESSORIES"] <- "Bags"




df5
