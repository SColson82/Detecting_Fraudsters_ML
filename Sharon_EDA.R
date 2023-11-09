library(tidyverse)
install.packages("dplyr")
library(dplyr)
library(ggplot2)

xTrainData <- read.csv("Datasets/X_train_G3tdtEn.csv")
yTrainData <- read_csv("Datasets/Y_train_2_XPXJDyy.csv")
yTrainData
unique(yTrainData$fraud_flag)
yTrainData %>%
  group_by(fraud_flag) %>%
  count()
fraud <- filter(yTrainData, fraud_flag == 1)
fraud



merged_fraud_df <- left_join(fraud, xTrainData, by = "ID")
merged_fraud_df
colnames(merged_fraud_df)
merged_fraud_df <- merged_fraud_df %>%
  arrange(ID) %>%
  as.data.frame()
colnames(merged_fraud_df)
merged_fraud_df
summary(merged_fraud_df)

# Create a file of the fraud data unique values
# Specify the file path where you want to save the output
output_file <- "fraud_unique_values_output.txt"

# Open the file for writing
file_conn <- file(output_file, "w")

# Loop through each column and print unique values to the file
for (i in 1:length(merged_fraud_df)) {
  cat("\nUnique values in column", colnames(df)[i+1], ":\n", file = file_conn)
  cat(unique_values[[i]], file = file_conn)

  print(unique_values[[i]])
}

# Close the file
close(file_conn)

cat("Unique values have been written to", output_file, "\n")

# Create a file of the fraud data itself
# Specify the file path where you want to save the output
output_file <- "fraud_flag_actual.csv"

# Open the file for writing in CSV mode
file_conn <- write.table(merged_fraud_df, file = output_file, sep = ",", quote = FALSE, row.names = FALSE)

glimpse(merged_fraud_df)

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

# Get the names of all the features in your data
features <- names(xTrainData)

# Convert the closure object to a data frame
df <- as.data.frame(xTrainData)

# Create a loop to create a boxplot and save a png for each feature
for (feature in features) {
  # Create a boxplot of the feature
  p <- ggplot(df, aes(x = feature)) +
    geom_boxplot()

  # Save the boxplot to a png file
  ggsave(paste0("boxplots/boxplot_", feature, ".png"), p)
}

