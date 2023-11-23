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
frequency_table <- table(unlist(apply(xTrainData[, paste0("goods_code", 1:24)], 2, function(x) na.omit(as.character(x)))))

# Convert the table to a data frame for better handling
frequency_df <- as.data.frame(frequency_table)
colnames(frequency_df) <- c("Value", "Frequency")

# Filter the values with frequency 100 or greater
filtered_frequency_df <- frequency_df %>%
  filter(Frequency >= 25)
filtered_frequency_df
# Define the list of columns to search through
columns_to_search <- paste0("goods_code", 1:24)

# Initialize an empty list to store the average fraud_flag values
average_fraud_flags <- list()

# Loop through each search value
for (search_value in filtered_frequency_df$Value) {
  # Create a subset of train_data for the common values
  subset_data <- train_data[apply(train_data[columns_to_search], 1, function(row) any(row %in% search_value)), ]

  # Calculate the average of the fraud_flag column and round to 4 decimal places
  average_fraud_flag <- round(mean(subset_data$fraud_flag), 4)

  # Add the result to the list
  average_fraud_flags[[search_value]] <- average_fraud_flag
}


# Add the list of average_fraud_flag to filtered_frequency_df as a new column
filtered_frequency_df$average_fraud_flag <- sapply(filtered_frequency_df$Value, function(search_value) average_fraud_flags[[intersect(search_value, names(average_fraud_flags))]])

# Print the updated filtered_frequency_df
print(filtered_frequency_df)
write.csv(filtered_frequency_df, "feature_engineering/unique_goods_code_counts_fraud_flag.csv", row.names = FALSE)
filtered_frequency_df <- read.csv("feature_engineering/unique_goods_code_counts_fraud_flag.csv")

# Create a scatter plot of frequency vs average_fraud_flag
ggplot(filtered_frequency_df, aes(x = Frequency, y = average_fraud_flag)) +
  geom_point() +
  labs(title = "Scatter Plot of Goods Code Frequency vs Average Fraud Flag",
       x = "Frequency",
       y = "Average Fraud Flag")

# Create a scatter plot of frequency vs average_fraud_flag with log scale
ggplot(filtered_frequency_df, aes(x = Frequency, y = average_fraud_flag)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Scatter Plot of Goods Code Frequency vs Average Fraud Flag (Log Scale)",
       x = "Frequency (log scale)",
       y = "Average Fraud Flag (log scale)")
