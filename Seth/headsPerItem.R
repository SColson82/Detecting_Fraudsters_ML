library(dplyr)
library(reshape2)
xTestData <- read.csv("/Users/seththompson/data science project/Detecting_Fraudsters_ML/Seth/Datasets/X_test_8skS2ey.csv")
# xTrainData <- read.csv("/Users/seththompson/data science project/Detecting_Fraudsters_ML/Seth/Datasets/X_train_G3tdtEn.csv")
# yTestData <- read.csv("/Users/seththompson/data science project/Detecting_Fraudsters_ML/Seth/Datasets/Y_test_random_2.csv")
# yTrainData <- read.csv("/Users/seththompson/data science project/Detecting_Fraudsters_ML/Seth/Datasets/Y_train_2_XPXJDyy.csv")

# format(head(xTestData,1), justift = "left", scientific = FALSE)
colIndexes <- c(2,26,50,74,98,122)
df1 <- xTestData[, c(2,26,50,74,98,122)]
df2 <- xTestData[, c(3,27,51,75,99,123)]
df3 <- xTestData[, c(4,28,52,76,100,124)]
df4 <- xTestData[, c(5,29,53,77,101,125)]
df5 <- xTestData[, c(6,30,54,78,102,126)]
df6 <- xTestData[, c(7,31,55,79,103,127)]
df7 <- xTestData[, c(8,32,56,80,104,128)]
df8 <- xTestData[, c(9,33,57,81,105,129)]
df9 <- xTestData[, c(10,34,58,82,106,130)]
df10 <- xTestData[, c(11,35,59,83,107,131)]
df11 <- xTestData[, c(12,36,60,84,108,132)]
df12 <- xTestData[, c(13,37,61,85,109,133)]
df13 <- xTestData[, c(14,38,62,86,110,134)]
df14 <- xTestData[, c(15,39,63,87,111,135)]
df15 <- xTestData[, c(16,40,64,88,112,136)]
df16 <- xTestData[, c(17,41,65,89,113,137)]
df17 <- xTestData[, c(18,42,66,90,114,138)]
df18 <- xTestData[, c(19,43,67,91,115,139)]
df19 <- xTestData[, c(20,44,68,92,116,140)]
df20 <- xTestData[, c(21,45,69,93,117,141)]
df21 <- xTestData[, c(22,46,70,94,118,142)]
df22 <- xTestData[, c(23,47,71,95,119,143)]
df23 <- xTestData[, c(24,48,72,96,120,144)]
df24 <- xTestData[, c(25,49,73,97,121,145)]

dfs <- list(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12,df13,df14,df15,df16,df17,df18,df19,df20,df21,df22,df23,df24)

headDfs <- lapply(dfs, head)
n <- 1
for (i in seq_along(headDfs)) {
  print(paste("dataframe",n,"head"))
  print(format(headDfs[[i]],justify = "centre", scientific = FALSE))
  n <- n + 1
}
