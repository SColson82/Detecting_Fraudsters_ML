library(tidyverse)
library(dplyr)
library(tidymodels)
library(ggplot2)
library(imbalance)
library(randomForest)

# Take caution, this script took over 5 hours to run. 

df <- read.csv("feature_engineering/train_data_model.csv",row.names = "X")
df

glimpse(df)
summary(df)

# Increases font size for all ggplot2 plots
theme_set(theme_gray(base_size=18))
# Increases font size for confusion matrix plots
update_geom_defaults("text", list(size=6))

options(repr.plot.width=6, repr.plot.height=5)


# Split data
set.seed(20231124)
data_split <- initial_split(df, prop = 0.7)
train_data <- training(data_split)
test_data <- testing(data_split)

# Print the shape of original (imbalanced) training dataset
train_y_categ <- train_data %>%
  select(fraud_flag) %>%
  table
message(
  paste0(
    "Original dataset shape ",
    paste(names(train_y_categ), train_y_categ, sep = ": ", collapse = ", ")
  )
)

# Resample the training dataset using SMOTE
smote_train_df <- train_data %>%
  mutate(fraud_flag = factor(fraud_flag)) %>%
  oversample(ratio = 0.99, method = "SMOTE", classAttr = "fraud_flag") %>%
  mutate(fraud_flag = as.integer(fraud_flag)) %>%
  mutate(fraud_flag = as.factor(fraud_flag)) %>%
  mutate(fraud_flag = forcats::fct_recode(fraud_flag, "0" = "1", "1" = "2"))

# Print the shape of resampled (balanced) training dataset
smote_train_y_categ <- smote_train_df %>%
  select(fraud_flag) %>% table
message(
  paste0(
    "Resampled dataset shape ",
    paste(names(smote_train_y_categ), smote_train_y_categ, sep = ": ", collapse = ", ")
  )
)
smote_train_df$fraud_flag <- as.factor(smote_train_df$fraud_flag)
smote_train_y_categ

# Define the random forest model with a progress bar and time estimate
start_time <- Sys.time()

rf_model <- rand_forest(trees = 100, mode = "classification") %>%
  set_engine("randomForest", importance = TRUE)

# Display progress and estimated time remaining
for (i in 1:100) {
  if (i %% 10 == 0) {
    current_time <- Sys.time()
    elapsed_time <- difftime(current_time, start_time, units = "secs")
    trees_per_sec <- i / as.numeric(elapsed_time)
    remaining_time <- (100 - i) / trees_per_sec

    cat(sprintf("\r%d trees fitted, Estimated time remaining: %.2f seconds", i, remaining_time))
    flush.console()
  }

  # Fit additional trees to the existing model
  rf_model <- randomForest(fraud_flag ~ ., data = smote_train_df, ntree = i, keep.forest = TRUE,
                           init.forest = rf_model$forest)
}

cat("\n")  # Add a newline after the progress bar
rf_model

# Make predictions on the test set
rf_preds <- predict(rf_model, newdata = test_data)

# Combine predictions with the original test_data
result_df <- cbind(test_data, Predicted_Fraud_Flag = rf_preds)
result_df

write.csv(result_df, "feature_engineering/randomforestresults.csv", row.names = FALSE)

# Make predictions on the test set
rf_preds <- predict(rf_model, newdata = test_data)

# Combine predictions with the original test_data
result_df <- cbind(test_data, Predicted_Fraud_Flag = rf_preds)

# Create a confusion matrix
conf_matrix <- table(result_df$fraud_flag, result_df$Predicted_Fraud_Flag)

# Print the confusion matrix
print("Confusion Matrix:")
print(conf_matrix)

# Convert to a data frame for easier writing to CSV
conf_matrix_df <- as.data.frame.matrix(conf_matrix)

# Calculate metrics
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
recall <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
f1_score <- 2 * (precision * recall) / (precision + recall)

# Print metrics
cat("\nAccuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")

# Convert to a data frame for easier writing to CSV
metrics_df <- data.frame(Accuracy = accuracy, Precision = precision, Recall = recall, F1_Score = f1_score)

# Assuming result_df has columns fraud_flag and Predicted_Fraud_Flag
rmse <- sqrt(mean((as.numeric(result_df$fraud_flag) - as.numeric(result_df$Predicted_Fraud_Flag))^2))

# Print RMSE
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

# Convert to a data frame for easier writing to CSV
rmse_df <- data.frame(RMSE = rmse)

# Write RMSE to CSV
write.csv(rmse_df, "feature_engineering/rmse.csv", row.names = FALSE)

# Write metrics to CSV
write.csv(metrics_df, "feature_engineering/model_metrics.csv", row.names = FALSE)
# Write confusion matrix to CSV
write.csv(conf_matrix_df, "feature_engineering/confusion_matrix.csv", row.names = TRUE)
