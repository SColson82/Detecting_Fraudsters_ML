library(tidyverse)
library(dplyr)
library(tidymodels)
library(ggplot2)
library(imbalance)




df <- read.csv("feature_engineering/train_data.csv",row.names = "X")
df

glimpse(df)
summary(df)

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
  mutate(fraud_flag = as.integer(fraud_flag))

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

smote_train_df
glimpse(smote_train_df)





# Define the recipe
tree_recipe <- recipe(fraud_flag ~ ., data = smote_train_df) %>%
  step_dummy(all_nominal(), one_hot = TRUE)

# Define the model specification
tree_spec <- decision_tree(mode = "classification") %>%
  set_engine("rpart")

# Combine recipe and model specification into a workflow
tree_workflow <- workflow() %>%
  add_recipe(tree_recipe) %>%
  add_model(tree_spec)

# Train the model
tree_fit <- tree_workflow %>%
  fit(data = smote_train_df)

# Make predictions on the test set
predictions <- predict(tree_fit, test_data) %>%
  bind_cols(test_data)

# Evaluate the model
conf_matrix <- conf_mat(predictions, truth = fraud_flag, estimate = .pred_class)
print(conf_matrix)
