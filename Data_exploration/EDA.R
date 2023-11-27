library(tidyverse)
library(dplyr)
library(ggplot2)

xTrainData <- read.csv("Datasets/X_train_G3tdtEn.csv",row.names = "ID")
xTrainData
yTrainData <- read.csv("Datasets/Y_train_2_XPXJDyy.csv")
yTrainData

# Assuming yTrainData is your data frame
yTrainData <- yTrainData %>%
  select(ID, fraud_flag)

yTrainData
# Set "ID" as row names
rownames(yTrainData) <- yTrainData$ID
yTrainData
# Keep only the "fraud_flag" column
yTrainData <- yTrainData[, "fraud_flag", drop = FALSE]
yTrainData

logisticModel<-glm(yTrainData ~ xTrainData, family="binomial")
summary(logisticModel)

# Remove the "ID" column
#yTrainData <- yTrainData[, -1]

# Print the modified data frame
yTrainData


yTrainData <- data.frame(yTrainData$fraud_flag)
head(yTrainData)
colnames(yTrainData)

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

