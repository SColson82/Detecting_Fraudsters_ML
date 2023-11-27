library(tidyverse)
library(tidymodels)

df <- read.csv("feature_engineering/train_data_model.csv",row.names = "X")
df

glimpse(df)
summary(df)

# Increases font size for all ggplot2 plots
theme_set(theme_gray(base_size=18))
# Increases font size for confusion matrix plots
update_geom_defaults("text", list(size=6))

df <- df |> mutate(fraud_flag = as_factor(str_to_title(fraud_flag)))
glimpse(df)

# Split data
set.seed(20221005)
data_split <- initial_split(df, prop = 0.7, strata = fraud_flag)
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


# Define model
KNNClass <- nearest_neighbor(mode = "classification", neighbors = 5)
glimpse(smote_train_df)

smote_train_df <- smote_train_df |> mutate(fraud_flag = as_factor(str_to_title(fraud_flag)))
smote_train_df$fraud_flag
glimpse(smote_train_df)
# Define recipe
# kNN works best when features are of the same scale
cartRecipe <- recipe(fraud_flag ~ .,
                     data = smote_train_df) |>
  step_normalize(all_numeric_predictors())

# Assemble workflow
cartWflow <- workflow() |>
  add_recipe(cartRecipe) |>
  add_model(KNNClass)

# Fit model
cartFit <- fit(cartWflow, smote_train_df)

# Predict values for the test set and add those values onto to the test set.
testPred <- augment(cartFit, test_data)

# Look at the predicted values and the probability
# for each class that were added to the data frame
testPred |>
  select(starts_with(".pred")) |>
  sample_n(10)

# Print accuracy and confusion matrix
testPred |> accuracy(fraud_flag, .pred_class)
testPred |> precision(fraud_flag, .pred_class)
testPred |> recall(fraud_flag, .pred_class)
testPred |> f_meas(fraud_flag, .pred_class)
confusionMatrix <- testPred |> conf_mat(fraud_flag, .pred_class)
confusionMatrix
confusionMatrix |> autoplot(type = "heatmap")
glimpse(testPred)
