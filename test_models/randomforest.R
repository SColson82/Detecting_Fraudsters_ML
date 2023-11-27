#library(tidyverse)
library(tidymodels)
library(recipes)
library(dplyr)


xTrainData <- read.csv("Datasets/X_train_G3tdtEn.csv",row.names = "ID")
xTrainData <- apply(xTrainData, 2, function(x) ifelse(x == "", NA, x))
yTrainData <- read.csv("Datasets/Y_train_2_XPXJDyy.csv")
yTrainData <- yTrainData %>%
  select(ID, fraud_flag)
rownames(yTrainData) <- yTrainData$ID
yTrainData <- yTrainData[, "fraud_flag", drop = FALSE]
train_data <- cbind(xTrainData, y = yTrainData)
train_data
summary(train_data)
glimpse(train_data)

keywords <- c("cash", "Nb")

train_data <- train_data %>%
  mutate_at(vars(contains(keywords)), as.integer)
colnames(train_data) <- make.names(colnames(train_data))

glimpse(train_data)
summary(train_data)


## Trying with classification trees and struggling with this.
#tree_recipe <- recipe(fraud_flag ~ ., data = train_data) %>%
  # Impute missing values for numeric variables with 0
#  step_mutate_at(all_numeric(), -all_outcomes(), "replace", replace = list(default = 0)) %>%
  # Impute missing values for categorical variables with NA
#  step_mutate_at(all_nominal(), -all_outcomes(), ~ifelse(is.na(.), NA, .)) %>%
  # Identify unknown values
#  step_unknown(all_nominal(), -all_outcomes(), event_level = NA) %>%
  # Encode categorical variables
#  step_dummy(all_nominal(), -all_outcomes()) %>%
  # Exclude the outcome variable from predictors
#  step_rm(all_outcomes())

glimpse(train_data)
summary(train_data)
# Preprocess the training data
#preprocessed_train_data <- tree_recipe %>%
#  prep(data = train_data) %>%
#  juice()
#preprocessed_train_data


## Trying Random Forest

# Define the recipe
tree_recipe <- recipe(fraud_flag ~ ., data = train_data) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_rm(all_outcomes())

# Preprocess the training data
preprocessed_train_data <- tree_recipe %>%
  prep(data = train_data) %>%
  juice()

# Split the data into training and testing sets
set.seed(123)
split_data <- initial_split(preprocessed_train_data, prop = 0.7)
train_data_split <- training(split_data)
test_data_split <- testing(split_data)

# Define the random forest model
rf_model <- rand_forest() %>%
  set_engine("randomForest", importance = TRUE) %>%
  set_mode("classification")

# Define the workflow
rf_workflow <- workflow() %>%
  add_model(rf_model) %>%
  add_formula(fraud_flag ~ .)

# Train the model
rf_fit <- rf_workflow %>%
  fit(data = train_data_split)

# Make predictions on the test set
rf_preds <- predict(rf_fit, new_data = test_data_split) %>%
  bind_cols(test_data_split)

# Evaluate the model
rf_metrics <- rf_preds %>%
  metrics(truth = fraud_flag, estimate = .pred_class)

# Print the evaluation metrics
rf_metrics
