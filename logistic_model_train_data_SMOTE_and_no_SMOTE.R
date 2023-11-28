library(tidymodels)
library(imbalance)
library(pROC)
library(ggplot2)
#Item and Make dataset
df <- read.csv("feature_engineering/train_data.csv",row.names = "X")
# Model dataset
#df <- read.csv("feature_engineering/train_data_model.csv",row.names = "X")
df
summary(df)
glimpse(df)


# Increases font size for all ggplot2 plots
theme_set(theme_gray(base_size=18))
# Increases font size for confusion matrix plots
update_geom_defaults("text", list(size=6))

options(repr.plot.width=6, repr.plot.height=5)

#df$fraud_flag <- as.factor(df$fraud_flag)
#Split data
set.seed(20231124)
data_split <- initial_split(df, prop = 0.7)
train_data <- training(data_split)
test_data <- testing(data_split)


# Fit logistic regression model
logistic_model <- glm(fraud_flag ~ ., data = train_data, family = binomial)




# Print a summary of the model
summary(logistic_model)


# Make predictions on the test set
test_data$predicted_prob <- predict(logistic_model, newdata = test_data, type = "response")

# Convert predicted probabilities to predicted class labels (0 or 1)
test_data$predicted_class <- ifelse(test_data$predicted_prob > 0.5, 1, 0)

# Calculate Root Mean Squared Error (RMSE)
rmse <- sqrt(mean((test_data$fraud_flag - test_data$predicted_prob)^2))

# Print RMSE
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

# Create confusion matrix
conf_matrix <- table(test_data$fraud_flag, test_data$predicted_class)

# Print the confusion matrix
conf_matrix

# Compute metrics
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
recall <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
f1_score <- 2 * (precision * recall) / (precision + recall)

# Print metrics
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")



# Create ROC curve
roc_curve <- roc(test_data$fraud_flag, test_data$predicted_prob)

# Convert ROC curve to a data frame
roc_df <- as.data.frame(coords(roc_curve))

# Plot ROC curve
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  #geom_text(aes(label = paste("AUC = ", round(auc(roc_curve), 3), sep = "")),


  #         hjust = 0, vjust = 1, color = "black") +
  labs(title = "ROC Curve (Without SMOTE)", x = "False Positive Rate", y = "True Positive Rate")






#Split data
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
  mutate(fraud_flag = recode(as.integer(fraud_flag), `1` = 0, `2` = 1))
smote_train_df$fraud_flag
# Set the levels of the response variable
smote_train_df$fraud_flag <- factor(smote_train_df$fraud_flag, levels = c("0", "1"))
smote_train_df$fraud_flag
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

smote_train_df$fraud_flag
glimpse(smote_train_df)

# Fit logistic regression model
logistic_model <- glm(fraud_flag ~ ., data = smote_train_df, family = binomial)




# Print a summary of the model
summary(logistic_model)


# Make predictions on the test set
test_data$predicted_prob <- predict(logistic_model, newdata = test_data, type = "response")

# Convert predicted probabilities to predicted class labels (0 or 1)
test_data$predicted_class <- ifelse(test_data$predicted_prob > 0.5, 1, 0)

# Calculate Root Mean Squared Error (RMSE)
rmse <- sqrt(mean((test_data$fraud_flag - test_data$predicted_prob)^2))

# Print RMSE
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

# Create confusion matrix
conf_matrix <- table(test_data$fraud_flag, test_data$predicted_class)

# Print the confusion matrix
conf_matrix

# Compute metrics
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
recall <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
f1_score <- 2 * (precision * recall) / (precision + recall)

# Print metrics
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")

# Create ROC curve with SMOTE
roc_curve_smote <- roc(test_data$fraud_flag, test_data$predicted_prob)
# Convert ROC curve to a data frame
roc_df <- as.data.frame(coords(roc_curve_smote))
# Plot ROC curve with SMOTE
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "red") +
  #geom_text(aes(label = paste("AUC = ", round(auc(roc_curve_smote), 3), sep = "")),
  #          hjust = 0, vjust = 1, color = "black") +
  labs(title = "ROC Curve (With SMOTE)", x = "False Positive Rate", y = "True Positive Rate")

