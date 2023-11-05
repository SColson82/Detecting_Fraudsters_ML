library(dplyr)
library(reshape2)
xTestData <- read.csv("/Users/seththompson/data science project/Detecting_Fraudsters_ML/Seth/Datasets/X_test_8skS2ey.csv")
xTrainData <- read.csv("/Users/seththompson/data science project/Detecting_Fraudsters_ML/Seth/Datasets/X_train_G3tdtEn.csv")
yTestData <- read.csv("/Users/seththompson/data science project/Detecting_Fraudsters_ML/Seth/Datasets/Y_test_random_2.csv")
yTrainData <- read.csv("/Users/seththompson/data science project/Detecting_Fraudsters_ML/Seth/Datasets/Y_train_2_XPXJDyy.csv")

# format(head(xTestData,1), justift = "left", scientific = FALSE)
colIndexes <- c(2,26,50,74,98,122)
df1 <- xTestData[, c(2,26,50,98,122)]
df2 <- xTestData[, c(3,27,51,99,123)]
dfs <- list(df1,df2)

headDfs <- lapply(dfs, head)
for (i in seq_along(headDfs)) {
  print("dataframe head: ")
  print(format(headDfs[[i]],justify = "left", scientific = FALSE))
}
