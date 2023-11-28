library(tidymodels)
library(imbalance)
df <- read.csv("feature_engineering/train_data_model.csv",row.names = "X")
df
summary(df)
glimpse(df)
df$fraud_flag
# Increases font size for all ggplot2 plots
theme_set(theme_gray(base_size=18))
# Increases font size for confusion matrix plots
update_geom_defaults("text", list(size=6))

options(repr.plot.width=6, repr.plot.height=5)

#df$fraud_flag <- as.factor(df$fraud_flag)
df$fraud_flag
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
logistic_model <- glm(fraud_flag ~ ., data = smote_train_df, family = binomial(link = "logit"))
smote_train_df$predicted_prob <- predict(logistic_model, newdata = smote_train_df, type = "response")
# Print a summary of the model
summary(logistic_model)


# Make predictions on the test set
test_data$predicted_prob <- predict(logistic_model, newdata = test_data, type = "response")
test_data$fraud_flag
# Convert predicted probabilities to predicted class labels (0 or 1)
test_data$predicted_class <- ifelse(test_data$predicted_prob > 0.5, 1, 0)

levels(test_data$fraud_flag)
levels(test_data$predicted_class)
# Convert to factors with explicit levels
test_data$fraud_flag <- factor(test_data$fraud_flag, levels = c("0", "1"))
test_data$predicted_class <- factor(test_data$predicted_class, levels = c("0", "1"))
test_data$fraud_flag
test_data$predicted_class
# Create confusion matrix
conf_matrix <- table(test_data$fraud_flag, test_data$predicted_class)
glimpse(test_data)
# Print the confusion matrix
conf_matrix

# Calculate Root Mean Squared Error (RMSE)
rmse <- sqrt(mean((test_data$fraud_flag - test_data$predicted_prob)^2))

# Print RMSE
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

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


library(ggplot2)
ggplot(smote_train_df, aes(x=predicted_prob,
                    color=fraud_flag,
                    linetype=fraud_flag)) +
  geom_density()

