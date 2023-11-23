library(tidyverse)
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

glimpse(train_data)
## Creating scatter plot of nb_of_items Vs fraud_flag
# Create a scatter plot of frequency vs average_fraud_flag
ggplot(train_data, aes(x = Nb_of_items, y = fraud_flag)) +
  geom_point() +
  labs(title = "Scatter Plot of Total Number of Items in Cart vs Fraud Flag",
       x = "Number of Items in Cart",
       y = "Fraud Flag")


# Making numeric columns numeric
keywords <- c("cash", "Nb")

train_data <- train_data %>%
  mutate_at(vars(contains(keywords)), as.integer)
colnames(train_data) <- make.names(colnames(train_data))

glimpse(train_data)
summary(train_data)

# Assuming your cash price columns start from the 25th column and end at the 48th column
cash_price_columns <- paste0("cash_price", 1:24)

# Assuming your Nbr_of_prod_purchas columns start from the 121st column and end at the 144th column
nbr_of_prod_purchas_columns <- paste0("Nbr_of_prod_purchas", 1:24)

# Create a new column total_price
train_data <- train_data %>%
  mutate(across(c(cash_price_columns, nbr_of_prod_purchas_columns), as.numeric)) %>%
  mutate(total_cash_per_cart = rowSums(select(., starts_with("cash_price")) *
                                   select(., starts_with("Nbr_of_prod_purchas")), na.rm = TRUE))

glimpse(train_data)
train_data$total_cash_per_cart

# Create a scatter plot of frequency vs average_fraud_flag
ggplot(train_data, aes(x = total_cash_per_cart, y = fraud_flag)) +
  geom_point() +
  labs(title = "Scatter Plot of Total Value of Items in Cart vs Fraud Flag",
       x = "Total Cash Price in Cart",
       y = "Fraud Flag")
# Save the modified data as a new CSV file
write.csv(train_data, "feature_engineering/dataTotalCartPrice.csv", row.names = FALSE)
summary(train_data)


