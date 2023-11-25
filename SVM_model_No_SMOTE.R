library(tidyverse)
library(dplyr)
library(tidymodels)
# Increases font size for all ggplot2 plots
theme_set(theme_gray(base_size=18))
# Increases font size for confusion matrix plots
update_geom_defaults("text", list(size=6))

options(repr.plot.width=6, repr.plot.height=5)
df <- read.csv("feature_engineering/train_data.csv",row.names = "X")
df

glimpse(df)
summary(df)
df$fraud_flag <- as.factor(df$fraud_flag)
#Split data
set.seed(20231124)
data_split <- initial_split(df, prop = 0.7)
train_data <- training(data_split)
test_data <- testing(data_split)

SVMrecipe <- recipe(fraud_flag ~ ., data = train_data) |>
  #Normalize numerical features
  step_normalize(all_numeric_predictors())

model <- svm_poly(mode = "classification",
                  cost = 0.01, engine = "kernlab")

polywflow <- workflow()|>
  add_recipe(SVMrecipe) |>
  add_model(model)

SVMfit <- fit(polywflow, train_data)
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
