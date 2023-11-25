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

SVMrecipe <- recipe(fraud_flag ~ ., data = smote_train_df) |>
  #Normalize numerical features
  step_normalize(all_numeric_predictors())

model <- svm_rbf(mode = "classification",
                  cost = 0.01, engine = "kernlab", rbf_sigma = 3)

flow <- workflow()|>
  add_recipe(SVMrecipe) |>
  add_model(model)

SVMfit <- fit(flow, smote_train_df)
SVMfit

testPred <-augment(SVMfit, test_data)
testPred |>
  select(starts_with(".pred")) |>
  sample_n(10)

# Print accuracy and confusion matrix
testPred |> accuracy(fraud_flag, .pred_class)
testPred |> conf_mat(fraud_flag, .pred_class)
testPred |>
  conf_mat(fraud_flag, .pred_class) |>
  autoplot(type = "heatmap")

# Once this is finished running, consider going back and
# running:
#svm_rbf(mode="classification",
#        cost=0.1,
#        engine="kernlab",
#        rbf_sigma=0.1)

# But first, try classification trees and logistic regression.
