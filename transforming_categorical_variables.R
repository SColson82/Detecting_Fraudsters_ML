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

# Making numeric columns numeric
keywords <- c("cash", "Nb")

train_data <- train_data %>%
  mutate_at(vars(contains(keywords)), as.integer)
colnames(train_data) <- make.names(colnames(train_data))

# Assuming your Nbr_of_prod_purchas columns start from the 121st column and end at the 144th column
nbr_of_prod_purchas_columns <- paste0("Nbr_of_prod_purchas", 1:24)

# Create a new column total_price
train_data <- train_data %>%
  mutate(across(c(cash_price_columns, nbr_of_prod_purchas_columns), as.numeric)) %>%
  mutate(total_cash_per_cart = rowSums(select(., starts_with("cash_price")) *
                                         select(., starts_with("Nbr_of_prod_purchas")), na.rm = TRUE))


glimpse(train_data)
summary(train_data)

# First we transform the item categorical variables into numeric values.

# Read in the unique_item_counts.csv as a dataframe and look at it.
unique_items_df <- read.csv("feature_engineering/unique_item_counts.csv")
unique_items_df


# Assuming your_dataframe_name is the name of your dataframe
sorted_unique_items_df <- unique_items_df %>%
  arrange(desc(Frequency))

# Print the sorted dataframe
print(sorted_unique_items_df)


# Look at values with the word women in them
#subset_df <- unique_items_df[grepl("women", unique_items_df$Value, ignore.case = TRUE), ]
#subset_df


# Values you're looking for
#values_to_match <- c("WOMEN S ACCESSORIES", "WOMEN S CLOTHES", "WOMEN S FOOTWEAR", "WOMEN S NIGHTWEAR", "WOMENS ACCESSORIES", "WOMENS CLOTHES", "WOMENS FOOTWEAR")

# Add a new column indicating if any of the values appear in the specified columns
#train_data_subset <- train_data %>%
#  rowwise() %>%
#  mutate(match = any(c_across(item1:item24) %in% values_to_match))

# Filter the data to keep only rows where a match is found
#train_data_subset <- train_data_subset %>%
#  filter(match) %>%
#  select(-match)

# Print a glimpse of the resulting data
#train_data_subset
#glimpse(train_data_subset)

# Values you're looking for
#values_to_replace <- c("WOMEN S ACCESSORIES", "WOMEN S CLOTHES", "WOMEN S FOOTWEAR", "WOMEN S NIGHTWEAR", "WOMENS ACCESSORIES", "WOMENS CLOTHES", "WOMENS FOOTWEAR")

# Replace the values in the specified columns
#train_data <- train_data %>%
#  mutate(across(item1:item24, ~ifelse(. %in% values_to_replace, "womens_apparel", .)))

# Print a glimpse of the resulting data
#glimpse(train_data)


# Values you're looking for
#values_to_match <- c("WOMEN S ACCESSORIES", "WOMEN S CLOTHES", "WOMEN S FOOTWEAR", "WOMEN S NIGHTWEAR", "WOMENS ACCESSORIES", "WOMENS CLOTHES", "WOMENS FOOTWEAR")

# Add a new column indicating if any of the values appear in the specified columns
#train_data_subset <- train_data %>%
#  rowwise() %>%
#  mutate(match = any(c_across(item1:item24) %in% values_to_match))

# Filter the data to keep only rows where a match is found
#train_data_subset <- train_data_subset %>%
#  filter(match) %>%
#  select(-match)

# Print a glimpse of the resulting data
#train_data_subset
#glimpse(train_data_subset)



# Automating this process:

# Create a list of values to replace and their corresponding replacements
replacement_list <- list(
list(c("WOMEN S ACCESSORIES",
    "WOMEN S CLOTHES",
    "WOMEN S FOOTWEAR",
    "WOMEN S NIGHTWEAR",
    "WOMENS ACCESSORIES",
    "WOMENS CLOTHES",
    "WOMENS FOOTWEAR",
    "MEN S ACCESSORIES",
    "MEN S CLOTHES",
    "MEN S FOOTWEAR",
    "MEN S NIGHTWEAR",
    "MEN S SPORTSWEAR",
    "MEN S UNDERWEAR SOCKS",
    "MENS CLOTHES",
    "MENS NIGHTWEAR",
    "MENS UNDERWEAR & SOCKS",
    "SUNGLASSES & READING GLASSES",
    "SUNGLASSES READING GLASSES",
    "LINGERIE & HOISERY",
    "LINGERIE HOISERY",
    "JEWELLERY & WATCHES",
    "JEWELLERY WATCHES"
  ), "apparel"),
list(c("FULFILMENT CHARGE",
    "WARRANTY",
    "SERVICE"
  ), "warranties_fulfillment_service"),
list(c("BEDROOM FURNITURE",
    "CARPETS RUGS FLOORING",
    "CARPETS, RUGS & FLOORING",
    "DOOR FURNITURE",
    "LIVING & DINING FURNITURE",
    "LIVING DINING FURNITURE",
    "SOFT FURNISHINGS",
    "OUTDOOR FURNITURE"
  ), "furniture"),
list(c("BATH & BODYCARE",
    "BATH BODYCARE",
    "CRAFT",
    "DIARIES & ORGANISERS",
    "DIARIES ORGANISERS",
    "DISPOSABLE TABLEWARE CUTLERY",
    "DRESSMAKING",
    "EASTER DECORATIONS",
    "FACIAL SKINCARE",
    "THEMED GIFTS",
    "UNKNOWN",
    "MAKEUP",
    "PAPER NOTEBOOKS",
    "PARTY DECORATIONS",
    "PENS PENCILS",
    "PRODUCT",
    "SPORTS EQUIPMENT",
    "STATIONERY SUNDRIES",
    "SUNCARE",
    "FRAGRANCE",
    "GIFT FOOD DRINK",
    "GIFT WRAP",
    "GREETING CARDS & PERSONALISED STATIONERY",
    "GREETING CARDS PERSONALISED STATIONERY",
    "GYM EQUIPMENT",
    "HAIRCARE",
    "HEALTH & BEAUTY ELECTRICAL",
    "HEALTH BEAUTY ACCESSORIES",
    "HEALTH BEAUTY ELECTRICAL",
    "HEATING & COOLING APPLIANCES",
    "HEATING COOLING APPLIANCES",
    "HI-FI",
    "HOME AND PERSONAL SECURITY",
    "HOME OFFICE",
    "HOME SAFETY EQUIPMENT",
    "HOT DRINK PREPARATION",
    "HOUSEHOLD CLEANING"
  ), "i_other"),
list(c("BARBECUES & ACCESSORIES",
    "BARBECUES ACCESSORIES",
    "BARWARE",
    "BATH LINEN",
    "BATHROOM",
    "BATHROOM ACCESSORIES",
    "BATHROOM FIXTURES",
    "BED LINEN",
    "CHRISTMAS DECORATIONS",
    "COOKING APPLIANCES",
    "COOKWARE",
    "DECORATING",
    "DECORATIVE ACCESSORIES",
    "FILING DESK ACCESSORIES",
    "FITTED KITCHENS",
    "FOOD PREPARATION",
    "FOOD STORAGE",
    "GARDENING EQUIPMENT",
    "LIGHTING",
    "TABLE LINEN",
    "TABLEWARE",
    "WINDOW DRESSING",
    "STORAGE & ORGANISATION",
    "STORAGE ORGANISATION",
    "PICNICWARE",
    "PRESERVING & BAKING EQUIPMENT",
    "PRESERVING BAKING EQUIPMENT",
    "STANDS & BRACKETS",
    "STANDS BRACKETS",
    "OUTDOOR ACCESSORIES",
    "KITCHEN ACCESSORIES",
    "KITCHEN SCALES & MEASURES",
    "KITCHEN SCALES MEASURES",
    "KITCHEN STORAGE",
    "KITCHEN UTENSILS & GADGETS",
    "KITCHEN UTENSILS GADGETS",
    "LAUNDRY & CLOTHESCARE",
    "LAUNDRY CLOTHESCARE"
  ), "home_accessories"),
list(c("AUDIO ACCESSORIES",
    "BLANK MEDIA & MEDIA STORAGE",
    "BLANK MEDIA MEDIA STORAGE",
    "BOOKS",
    "GAMES",
    "GAMING",
    "VIDEOS DVD DIGITAL EQUIPMENT",
    "TELEPHONE ACCESSORIES",
    "TELEPHONES FAX MACHINES TWO-WAY RADIOS",
    "TELEPHONES, FAX MACHINES & TWO-WAY RADIOS",
    "TELEVISIONS & HOME CINEMA",
    "TELEVISIONS HOME CINEMA"
  ), "entertainment"),
list(c("2HP ELITEBOOK 850V6",
    "APPLE PRODUCTDESCRIPTION",
    "APPLE S",
    "COMPUTERS",
    "HP ELITEBOOK 850V6"
  ), "computers"),
list(c("2TOSHIBA PORTABLE HARD DRIVE",
    "2LOGITECH PEBBLE M350 BLUETOOTH MOUSE",
    "2MICROSOFT OFFICE HOME AND STUDENT 2019,",
    "2TARGUS GEOLITE ESSENTIAL CASE",
    "6  SPACE GREY 32GB",
    "AERIALS REMOTE CONTROLS",
    "CABLES & ADAPTERS",
    "CABLES ADAPTERS",
    "COMPUTER NETWORKING",
    "COMPUTER PERIPHERALS & ACCESSORIES",
    "COMPUTER PERIPHERALS ACCESSORIES",
    "COMPUTER SOFTWARE",
    "IMAGING ACCESSORIES",
    "IMAGING EQUIPMENT",
    "LOGITECH PEBBLE M350 BLUETOOTH MOUSE",
    "MICROSOFT OFFICE HOME AND STUDENT 2019,",
    "PORTABLE AUDIO EQUIPMENT",
    "POWER & BATTERIES",
    "POWER BATTERIES",
    "PRINTERS & SCANNERS",
    "PRINTERS SCANNERS",
    "TARGUS GEOLITE ESSENTIAL CASE",
    "TECHNOLOGY ACCESSORIES",
    "TOSHIBA PORTABLE HARD DRIVE"
  ), "computer_accessories"),
list(c("BAGS & CARRY CASES",
    "BAGS CARRY CASES",
    "BAGS WALLETS ACCESSORIES",
    "BAGS, WALLETS & ACCESSORIES",
    "LUGGAGE"
  ), "travel"),
list(c("BABY & CHILD TRAVEL",
    "BABY CHANGING",
    "BABY CHILD TRAVEL",
    "BABY FEEDING",
    "BABY PLAY EQUIPMENT",
    "BABYWEAR",
    "BOYSWEAR",
    "CHILDREN S ACCESSORIES",
    "CHILDREN S FOOTWEAR",
    "CHILDREN S FURNITURE",
    "CHILDRENS FOOTWEAR",
    "GIRLSWEAR",
    "NURSERY ACCESSORIES",
    "NURSERY EQUIPMENT FURNITURE",
    "NURSERY FURNITURE",
    "NURSERY LINEN",
    "NURSERY TOYS",
    "SCHOOLWEAR",
    "TOYS"
  ), "children")
  # Add more pairs as needed
)

# Function to replace values in a dataframe
replace_values <- function(data, replacements) {
  for (replacement_pair in replacements) {
    old_values <- replacement_pair[[1]]
    new_value <- replacement_pair[[2]]

    # Print the old and new values for debugging
    cat("Old Values:", paste(old_values, collapse = ", "), "\n")
    cat("New Value:", new_value, "\n")

    # Replace the values only in columns with "item" in the title
    data <- data %>%
      mutate(across(starts_with("item"), ~case_when(. %in% old_values ~ as.character(new_value), TRUE ~ .)))
  }
  return(data)
}

# Apply the function to the dataframe
train_data <- replace_values(train_data, replacement_list)

# Print a glimpse of the resulting data
glimpse(train_data)
train_data

# Check balance of fraud vs non fraud.
ggplot(train_data, aes(x = factor(fraud_flag), fill = factor(fraud_flag))) +
  geom_bar(stat = "count") +
  scale_x_discrete(labels = c("no fraud", "fraud")) +
  ggtitle("Class Distributions \n (0: No Fraud || 1: Fraud)") +
  theme(plot.title = element_text(size = 10))

# Total cash per cart with outliers included.
ggplot(train_data, aes(x = as.factor(fraud_flag), y = total_cash_per_cart, fill = as.factor(fraud_flag))) +
  geom_boxplot() +
  scale_x_discrete(labels = c("no fraud", "fraud")) +
  ggtitle("Boxplot of Total Cash Per Cart") +
  coord_cartesian(ylim = quantile(train_data$total_cash_per_cart, c(0.05, 0.95)))

# Total items per cart with outliers included.
ggplot(train_data, aes(x = as.factor(fraud_flag), y = Nb_of_items, fill = as.factor(fraud_flag))) +
  geom_boxplot() +
  scale_x_discrete(labels = c("no fraud", "fraud")) +
  ggtitle("Boxplot of Number of Items in Cart") +
  coord_cartesian(ylim = quantile(train_data$Nb_of_items, c(0.05, 0.95)))

# Assuming your data frame is named train_data
# Replace NA values with 0
train_data[is.na(train_data)] <- 0
train_data
# Specify the values you want to count
values_to_count <- c("children", "travel", "computer_accessories", "entertainment",
                     "home_accessories", "i_other", "furniture",
                     "warranties_fulfillment_service", "apparel", "computers")

# Loop through each value and create a new column for each
for (value in values_to_count) {
  # Create a new column with the count of occurrences
  train_data[[value]] <- rowSums(train_data[, grepl("^item", names(train_data))] == value, na.rm = TRUE)

  # Optionally, replace NA values in the new column with 0
  train_data[[value]][is.na(train_data[[value]])] <- 0
}
train_data

# Remove columns with the word "item" in their names
train_data <- train_data[, !grepl("item", names(train_data))]
train_data

# Apply table function across all columns, ignoring NA values
frequency_table <- table(unlist(apply(xTrainData[, paste0("make", 1:24)], 2, function(x) na.omit(as.character(x)))))


# Replace NA values with 0
train_data[is.na(train_data)] <- 0

# Create a frequency table for values in make1 through make24
make_columns <- paste0("make", 1:24)
make_columns
make_frequency <- table(unlist(train_data[, make_columns]))
make_frequency
# Identify values with a count of less than 100
values_to_replace <- names(make_frequency[make_frequency < 100])
values_to_replace

glimpse(train_data)

# Replace values with "other"
train_data <- train_data %>%
  mutate(across(starts_with("make"), ~case_when(. %in% values_to_replace ~ "make_other", TRUE ~ .)))


glimpse(train_data)


# Apply table function across all columns, ignoring NA values
check_frequency_table <- table(unlist(apply(train_data[, paste0("make", 1:24)], 2, function(x) na.omit(as.character(x)))))
check_frequency_table

# Specify the values you want to count
values_to_count <- c("ERCOL FOR RETAILER", "LG", "SILENTNIGHT", "TOMMEE TIPPEE",
                     "ANYDAY RETAILER", "G PLAN VINTAGE", "LOGITECH",
                     "SILVER CROSS", "WEBER", "APPLE", "HALO", "MAXI-COSI", "SONOS", "WEST ELM",
                     "AVF","HERMAN MILLER","MICROSOFT","SONY","BUGABOO","HYPNOS","ma_other","STOKKE",
                     "CROFT COLLECTION","INNOVATION LIVING","PANASONIC","SWOON","CYBEX","JOSEPH JOSEPH","PHILIPS","SWYFT","DYSON","KETTLER","RETAILER","TARGUS","EMMA",
                     "LE CREUSET","SAMSUNG","TEMPUR")

# Loop through each value and create a new column for each
for (value in values_to_count) {
  # Create a new column with the count of occurrences
  train_data[[value]] <- rowSums(train_data[, grepl("^make", names(train_data))] == value, na.rm = TRUE)

  # Optionally, replace NA values in the new column with 0
  train_data[[value]][is.na(train_data[[value]])] <- 0
}
train_data

# Remove columns with the word "item" in their names
train_data <- train_data[, !grepl("make", names(train_data))]
glimpse(train_data)

# Remove columns with the word "item" in their names
train_data <- train_data[, !grepl("goods_code", names(train_data))]
glimpse(train_data)
summary(train_data)

# Apply table function across all columns, ignoring NA values
frequency_table <- table(unlist(apply(xTrainData[, paste0("model", 1:24)], 2, function(x) na.omit(as.character(x)))))
# Convert the frequency table to a data frame
frequency_df <- as.data.frame(frequency_table)

# Sort the data frame by counts in descending order
sorted_frequency_df <- frequency_df[order(-frequency_df$Freq), ]

# Print the sorted frequency table
print(sorted_frequency_df)

# Remove columns with the word "item" in their names
train_data <- train_data[, !grepl("model", names(train_data))]
glimpse(train_data)
summary(train_data)

write.csv(train_data, file = "feature_engineering/train_data.csv", row.names = FALSE)

