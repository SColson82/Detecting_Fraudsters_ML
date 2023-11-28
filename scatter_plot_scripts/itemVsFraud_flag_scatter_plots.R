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
frequency_table <- table(unlist(apply(xTrainData[, paste0("item", 1:24)], 2, function(x) na.omit(as.character(x)))))

# Convert the table to a data frame for better handling
frequency_df <- as.data.frame(frequency_table)
colnames(frequency_df) <- c("Value", "Frequency")
frequency_df
# Define the list of columns to search through
columns_to_search <- paste0("item", 1:24)
columns_to_search
# Initialize an empty list to store the average fraud_flag values
average_fraud_flags <- list()

# Loop through each search value
for (search_value in frequency_df$Value) {
  # Create a subset of train_data for the current search value
  subset_data <- train_data[apply(train_data[columns_to_search], 1, function(row) any(row %in% search_value)), ]

  # Calculate the average of the fraud_flag column and round to 4 decimal places
  average_fraud_flag <- round(mean(subset_data$fraud_flag), 4)

  # Add the result to the list
  average_fraud_flags[[search_value]] <- average_fraud_flag
}

# Print the list of average_fraud_flags
print(average_fraud_flags)

# Add the list of average_fraud_flag to frequency_df as a new column
frequency_df$average_fraud_flag <- sapply(frequency_df$Value, function(search_value) average_fraud_flags[[search_value]])

# Print the updated frequency_df
print(frequency_df)
write.csv(frequency_df, "feature_engineering/unique_item_counts_fraud_flag.csv", row.names = FALSE)

# Create a scatter plot of frequency vs average_fraud_flag
ggplot(frequency_df, aes(x = Frequency, y = average_fraud_flag)) +
  geom_point() +
  labs(title = "Scatter Plot of Frequency vs Average Fraud Flag",
       x = "Frequency",
       y = "Average Fraud Flag")

# Create a scatter plot of frequency vs average_fraud_flag with log scale
ggplot(frequency_df, aes(x = Frequency, y = average_fraud_flag)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Scatter Plot of Frequency vs Average Fraud Flag (Log Scale)",
       x = "Frequency (log scale)",
       y = "Average Fraud Flag (log scale)")










# Add the average fraud_flag values to frequency_df
frequency_df$average_fraud_flag <- average_fraud_flag

# Print the updated frequency_df
print(frequency_df)

# Create a scatter plot
ggplot(frequency_df, aes(x = Frequency, y = average_fraud_flag)) +
  geom_point() +
  labs(x = "Frequency", y = "Average Fraud Flag", title = "Scatter Plot of Frequency vs Average Fraud Flag")

print(train_data)


# Define the value to search for
search_value <- "WOMENS CLOTHES"

# Create a subset of train_data where the value appears in the item1 column
subset_data <- train_data[train_data$item1 == search_value, ]

# Print or further process the subset_data as needed
print(subset_data)



# Define the value to search for
search_value <- "SCHOOL WEAR"

# Create an empty data frame to store the subset
subset_data <- data.frame()

# Define the list of columns to search through
columns_to_search <- paste0("item", 1:24)

# Loop over columns with names containing the word "item"
for (col in columns_to_search) {
  # Create a subset of train_data where the value appears in the current column
  # Create a subset of train_data where the search_value is present in any of the specified columns
  subset_data <- train_data[train_data[,col] == search_value,]

  #data <- (train_data[col])
  print(subset_data)
  # Append the subset to the result data frame
  #subset_data <- rbind(subset_data, subset_col)
}

# Remove duplicates from the final subset
subset_data <- unique(subset_data)

# Print or further process the subset_data as needed
print(subset_data)



# Load the dplyr package
library(dplyr)

# Define the value to search for
search_value <- "SCHOOL WEAR"

# Create an empty data frame to store the subset
subset_data <- data.frame()

# Define the list of columns to search through
columns_to_search <- paste0("item", 1:24)

# Loop over columns with names containing the word "item"
for (col in columns_to_search) {
  # Create a subset of train_data where the value appears in the current column
  subset_col <- train_data %>% filter(.data[[col]] == search_value)

  # Append the subset to the result data frame
  subset_data <- bind_rows(subset_data, subset_col)
}

# Remove duplicates from the final subset
subset_data <- unique(subset_data)

# Print or further process the subset_data as needed
print(subset_data)



# Load the dplyr package
library(dplyr)

# Define the value to search for
search_value <- "SCHOOL WEAR"

# Define the list of columns to search through
columns_to_search <- paste0("item", 1:24)

# Create a subset of train_data where the search_value is present in any of the specified columns
subset_data <- train_data %>%
  filter(across(all_of(columns_to_search), ~. == search_value) %>% rowSums(na.rm = TRUE) > 0)

# Remove duplicates from the final subset
subset_data <- unique(subset_data)

# Print or further process the subset_data as needed
print(subset_data)

# Define the value to search for
search_value <- "WOMENS CLOTHES"
columns_to_search <- grep("item", names(train_data), value = TRUE)
# Create a subset of train_data where the value appears in the item1 or item2 columns
#subset_data <- train_data[train_data$item1 == search_value | train_data$item2 %in% search_value, ]
subset_data <- train_data[apply(train_data[columns_to_search], 1, function(row) any(row %in% search_value)), ]
# Print or further process the subset_data as needed
print(subset_data)
# Calculate the average of the fraud_flag column in subset_data
average_fraud_flag <- round(mean(subset_data$fraud_flag), 4)

# Print the average
print(average_fraud_flag)

