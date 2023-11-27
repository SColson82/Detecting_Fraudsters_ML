library(tidyverse)
xTrainData <- read.csv("Datasets/X_train_G3tdtEn.csv",row.names = "ID")
xTrainData <- apply(xTrainData, 2, function(x) ifelse(x == "", NA, x))
yTrainData <- read.csv("Datasets/Y_train_2_XPXJDyy.csv")
yTrainData <- yTrainData %>%
  select(ID, fraud_flag)
rownames(yTrainData) <- yTrainData$ID
yTrainData <- yTrainData[, "fraud_flag", drop = FALSE]
train_data <- cbind(xTrainData, y = yTrainData)
train_data

# Apply table function across all columns, ignoring NA values
frequency_table <- table(unlist(apply(xTrainData[, paste0("model", 1:24)], 2, function(x) na.omit(as.character(x)))))

# Convert the table to a data frame for better handling
frequency_df <- as.data.frame(frequency_table)
colnames(frequency_df) <- c("Value", "Frequency")
frequency_df

# Define the list of columns to search through
columns_to_search <- paste0("model", 1:24)
columns_to_search
# Initialize an empty list to store the average fraud_flag values
average_fraud_flags <- list()

# Loop through each search value
for (search_value in frequency_df$Value) {
  # Create a subset of train_data for the current search value
  print("Beginning:")
  print(search_value)
  subset_data <- train_data[apply(train_data[columns_to_search], 1, function(row) any(row %in% search_value)), ]

  # Calculate the average of the fraud_flag column and round to 4 decimal places
  average_fraud_flag <- round(mean(subset_data$fraud_flag), 4)

  # Add the result to the list
  average_fraud_flags[[search_value]] <- average_fraud_flag
  print("Ending:")
  print(search_value)
}


# Print the list of average_fraud_flags
print(average_fraud_flags)

# Add the list of average_fraud_flag to frequency_df as a new column
frequency_df$average_fraud_flag <- sapply(frequency_df$Value, function(search_value) average_fraud_flags[[search_value]])

# Print the updated frequency_df
print(frequency_df)
write.csv(frequency_df, "feature_engineering/unique_model_counts_fraud_flag.csv", row.names = FALSE)

# Create a scatter plot of frequency vs average_fraud_flag
ggplot(frequency_df, aes(x = Frequency, y = average_fraud_flag)) +
  geom_point() +
  labs(title = "Scatter Plot of Model Frequency vs Average Fraud Flag",
       x = "Frequency",
       y = "Average Fraud Flag")
