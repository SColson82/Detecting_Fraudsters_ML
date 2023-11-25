library(tidyverse)
library(dplyr)
library(tidymodels)
library(ggplot2)
library(imbalance)
library(randomForest)



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

# Make predictions on the test set
test_data <- testing(data_split)
rf_preds <- predict(fitted_model, new_data = test_data) %>%
  bind_cols(test_data)

# Evaluate the model
rf_metrics <- rf_preds %>%
  metrics(truth = fraud_flag, estimate = .pred_class)

# Calculate Root Mean Squared Error (RMSE)
rmse <- sqrt(mean((rf_preds$fraud_flag - rf_preds$.pred_class)^2))

# Print RMSE
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

# Print the evaluation metrics
rf_metrics


# #df$fraud_flag <- as.factor(df$fraud_flag)
# #Split data
# set.seed(20231124)
# data_split <- initial_split(df, prop = 0.7)
# train_data <- training(data_split)
# test_data <- testing(data_split)

# # Print the shape of original (imbalanced) training dataset
# train_y_categ <- train_data %>%
#   select(fraud_flag) %>%
#   table
# message(
#   paste0(
#     "Original dataset shape ",
#     paste(names(train_y_categ), train_y_categ, sep = ": ", collapse = ", ")
#   )
# )

# # Resample the training dataset using SMOTE
# smote_train_df <- train_data %>%
#   mutate(fraud_flag = factor(fraud_flag)) %>%
#   oversample(ratio = 0.99, method = "SMOTE", classAttr = "fraud_flag") %>%
#   mutate(fraud_flag = as.integer(fraud_flag))

# # Print the shape of resampled (balanced) training dataset
# smote_train_y_categ <- smote_train_df %>%
#   select(fraud_flag) %>% table
# message(
#   paste0(
#     "Resampled dataset shape ",
#     paste(names(smote_train_y_categ), smote_train_y_categ, sep = ": ", collapse = ", ")
#   )
# )
# smote_train_df$fraud_flag <- as.factor(smote_train_df$fraud_flag)
# smote_train_y_categ

# smote_train_df
# glimpse(smote_train_df)



# # Define the recipe
# tree_recipe <- recipe(fraud_flag ~ ., data = smote_train_df) %>%
#   step_dummy(all_nominal(), -all_outcomes()) %>%
#   step_rm(all_outcomes())

# # Preprocess the training data
# preprocessed_train_data <- tree_recipe %>%
#   prep(data = smote_train_df) %>%
#   juice()

# # Split the data into training and testing sets
# # set.seed(123)
# # split_data <- initial_split(preprocessed_train_data, prop = 0.7)
# # train_data_split <- training(split_data)
# # test_data_split <- testing(split_data)

# # Define the random forest model
# rf_model <- rand_forest() %>%
#   set_engine("randomForest", importance = TRUE) %>%
#   set_mode("classification")

# # Define the workflow
# rf_workflow <- workflow() %>%
#   add_model(rf_model) %>%
#   add_formula(fraud_flag ~ .)

# # Train the model
# rf_fit <- rf_workflow %>%
#   fit(data = smote_train_df)

# # Make predictions on the test set
# rf_preds <- predict(rf_fit, new_data = test_data) %>%
#   bind_cols(test_data)

# # Evaluate the model
# rf_metrics <- rf_preds %>%
#   metrics(truth = fraud_flag, estimate = .pred_class)

# # Print the evaluation metrics
# rf_metrics





# # Define the recipe
# tree_recipe <- recipe(fraud_flag ~ ., data = smote_train_df) %>%
#   step_dummy(all_nominal(), one_hot = TRUE)

# # Define the model specification
# tree_spec <- decision_tree(mode = "classification") %>%
#   set_engine("rpart")

# # Combine recipe and model specification into a workflow
# tree_workflow <- workflow() %>%
#   add_recipe(tree_recipe) %>%
#   add_model(tree_spec)

# # Train the model
# tree_fit <- tree_workflow %>%
#   fit(data = smote_train_df)

# # Make predictions on the test set
# predictions <- predict(tree_fit, test_data) %>%
#   bind_cols(test_data)

# # Evaluate the model
# conf_matrix <- conf_mat(predictions, truth = fraud_flag, estimate = .pred_class)
# print(conf_matrix)
