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

#Changing Nb_of_items column name so it doesn't get deleted later.
colnames(train_data)[colnames(train_data) == "Nb_of_items"] <- "Total_Nb_In_Cart"

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
ggplot(train_data, aes(x = as.factor(fraud_flag), y = Total_Nb_In_Cart, fill = as.factor(fraud_flag))) +
  geom_boxplot() +
  scale_x_discrete(labels = c("no fraud", "fraud")) +
  ggtitle("Boxplot of Number of Items in Cart") +
  coord_cartesian(ylim = quantile(train_data$Total_Nb_In_Cart, c(0.05, 0.95)))

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

write.csv(train_data, file = "feature_engineering/train_data.csv", row.names = TRUE)

### Looking at Model
# Read in the unique_item_counts.csv as a dataframe and look at it.
unique_models_df <- read.csv("feature_engineering/unique_model_counts.csv")
unique_models_df


# Assuming your_dataframe_name is the name of your dataframe
sorted_unique_models_df <- unique_models_df %>%
  arrange(desc(Frequency))

# Print the sorted dataframe
print(sorted_unique_models_df)


# Create a frequency table for values in make1 through make24
model_columns <- paste0("model", 1:24)
model_columns
model_frequency <- table(unlist(train_data[, model_columns]))
model_frequency
# Identify values with a count of less than 100
values_to_replace <- names(model_frequency[model_frequency < 100])
values_to_replace

glimpse(train_data)

# Replace values with "other"
train_data <- train_data %>%
  mutate(across(starts_with("model"), ~case_when(. %in% values_to_replace ~ "mo_other", TRUE ~ .)))


glimpse(train_data)


# Apply table function across all columns, ignoring NA values
check_frequency_table <- table(unlist(apply(train_data[, paste0("model", 1:24)], 2, function(x) na.omit(as.character(x)))))
check_frequency_table
write.csv(check_frequency_table, file = "feature_engineering/check_frequency_table", row.names = FALSE)
# Specify the values you want to count
values_to_count <- c("2019 APPLE AIRPODS WITH CHARGING CASE",
                     "2019 APPLE MACBOOK PRO 16 TOUCH BAR INTEL CORE I9",
                     '2019 APPLE MACBOOK PRO 16""',
                     "2020 APPLE IMAC 21 5 ALL-IN-ONE INTEL CORE I5 8GB",
                     "2020 APPLE IMAC 27 ALL-IN-ONE",
                     "2020 APPLE IMAC 27 ALL-IN-ONE INTEL CORE I5 8GB RA",
                     "2020 APPLE IMAC 27 ALL-IN-ONE INTEL CORE I7 8GB RA",
                     "2020 APPLE IPAD AIR 10 9 A14 BIONIC PROCESSOR IOS",
                     '2020 APPLE IPAD AIR 10.9""',
                     '2020 APPLE IPAD PRO 11""',
                     '2020 APPLE IPAD PRO 12.9""',
                     "2020 APPLE MAC MINI DESKTOP COMPUTER M1 PROCESSOR",
                     "2020 APPLE MACBOOK AIR",
                     "2020 APPLE MACBOOK AIR 13 3 RETINA DISPLAY M1 PROC",
                     "2020 APPLE MACBOOK PRO",
                     "2020 APPLE MACBOOK PRO 13 TOUCH BAR INTEL CORE I5",
                     "2020 APPLE MACBOOK PRO 13 TOUCH BAR M1 PROCESSOR 8",
                     '2020 APPLE MACBOOK PRO 13""',
                     "2021 APPLE AIRPODS WITH MAGSAFE CHARGING CASE 3RD",
                     "2021 APPLE IMAC 24 ALL-IN-ONE M1 PROCESSOR 8GB RAM",
                     "2021 APPLE IPAD 10 2 A13 BIONIC PROCESSOR IPADOS W",
                     "2021 APPLE IPAD MINI 8 3 A15 BIONIC PROCESSOR IPAD",
                     "2021 APPLE IPAD PRO 11 M1 PROCESSOR IOS WI-FI 128G",
                     "2021 APPLE IPAD PRO 11 M1 PROCESSOR IOS WI-FI 256G",
                     "2021 APPLE IPAD PRO 11 M1 PROCESSOR IOS WI-FI 512G",
                     "2021 APPLE IPAD PRO 11 M1 PROCESSOR IOS WI-FI CELL",
                     '2021 APPLE IPAD PRO 11""',
                     "2021 APPLE IPAD PRO 12 9 M1 PROCESSOR IOS WI-FI 12",
                     "2021 APPLE IPAD PRO 12 9 M1 PROCESSOR IOS WI-FI 1T",
                     "2021 APPLE IPAD PRO 12 9 M1 PROCESSOR IOS WI-FI 25",
                     "2021 APPLE IPAD PRO 12 9 M1 PROCESSOR IOS WI-FI 51",
                     "2021 APPLE IPAD PRO 12 9 M1 PROCESSOR IOS WI-FI CE",
                     '2021 APPLE IPAD PRO 12.9""',
                     "2021 APPLE MACBOOK PRO 14 M1 PRO PROCESSOR 16GB RA",
                     "2021 APPLE MACBOOK PRO 16 M1 PRO PROCESSOR 16GB RA",
                     "2022 APPLE IPAD AIR 10 9 M1 PROCESSOR IPADOS WI-FI",
                     "AIRPODS PRO",
                     "APPLE 20W USB-C POWER ADAPTER FOR IPHONE IPAD",
                     "APPLE AIRPODS PRO WITH MAGSAFE CHARGING CASE",
                     "APPLE AIRPODS PRO WITH WIRELESS CHARGING CASE",
                     "APPLE IPHONE 11",
                     "APPLE IPHONE 12",
                     "APPLE IPHONE 12 MINI",
                     "APPLE IPHONE 12 PRO",
                     "APPLE IPHONE 12 PRO MAX",
                     "APPLE IPHONE SE",
                     "APPLE MAGIC KEYBOARD FOR 11 IPAD PRO 2020 BLACK",
                     "APPLE MAGIC KEYBOARD FOR 11 IPAD PRO 2021 WHITE",
                     "APPLE MAGIC KEYBOARD FOR 12 9 IPAD PRO",
                     "APPLE MAGIC KEYBOARD FOR 12 9 IPAD PRO 2021 BLACK",
                     "APPLE MAGIC KEYBOARD FOR 12 9 IPAD PRO 2021 WHITE",
                     "APPLE MAGIC KEYBOARD WITH NUMERIC KEYPAD  E",
                     "APPLE MAGIC MOUSE 2 2015 WHITE",
                     "APPLE MAGIC MOUSE 2021 SILVER",
                     "APPLE MJ1M2ZM A USB-C TO USB ADAPTOR",
                     "APPLE MLL82ZM A USB-C CHARGE CABLE 2M",
                     "APPLE PENCIL 2ND GENERATION 2018 MATTE WHITE",
                     "APPLE PENCIL, 2ND GENERATION (2018)",
                     "APPLE SMART FOLIO FOR IPAD PRO 12 9 5TH GENERATION",
                     "APPLE WATCH NIKE SERIES 6",
                     "APPLE WATCH NIKE SERIES 6 GPS 44MM SPACE GREY ALUM",
                     "APPLE WATCH NIKE SERIES 7 GPS 41MM MIDNIGHT ALUMIN",
                     "APPLE WATCH NIKE SERIES 7 GPS 41MM STARLIGHT ALUMI",
                     "APPLE WATCH NIKE SERIES 7 GPS 45MM MIDNIGHT ALUMIN",
                     "APPLE WATCH SERIES 6",
                     "APPLE WATCH SERIES 6 GPS 40MM GOLD ALUMINIUM CASE",
                     "APPLE WATCH SERIES 6 GPS 40MM SILVER ALUMINIUM CAS",
                     "APPLE WATCH SERIES 6 GPS 40MM SPACE GREY ALUMINIUM",
                     "APPLE WATCH SERIES 6 GPS 44MM BLUE ALUMINIUM CASE",
                     "APPLE WATCH SERIES 6 GPS 44MM GOLD ALUMINIUM CASE",
                     "APPLE WATCH SERIES 6 GPS 44MM SPACE GREY ALUMINIUM",
                     "APPLE WATCH SERIES 6 GPS CELLULAR 44MM SPACE GREY",
                     "APPLE WATCH SERIES 7 GPS 41MM BLUE ALUMINIUM CASE",
                     "APPLE WATCH SERIES 7 GPS 41MM GREEN ALUMINIUM CASE",
                     "APPLE WATCH SERIES 7 GPS 41MM MIDNIGHT ALUMINIUM C",
                     "APPLE WATCH SERIES 7 GPS 41MM STARLIGHT ALUMINIUM",
                     "APPLE WATCH SERIES 7 GPS 45MM BLUE ALUMINIUM CASE",
                     "APPLE WATCH SERIES 7 GPS 45MM GREEN ALUMINIUM CASE",
                     "APPLE WATCH SERIES 7 GPS 45MM MIDNIGHT ALUMINIUM C",
                     "APPLE WATCH SERIES 7 GPS 45MM STARLIGHT ALUMINIUM",
                     "APPLE WATCH SERIES 7 GPS CELLULAR 41MM GOLD STAINL",
                     "APPLE WATCH SERIES 7 GPS CELLULAR 41MM MIDNIGHT AL",
                     "APPLE WATCH SERIES 7 GPS CELLULAR 41MM STARLIGHT A",
                     "APPLE WATCH SERIES 7 GPS CELLULAR 45MM GOLD STAINL",
                     "APPLE WATCH SERIES 7 GPS CELLULAR 45MM GRAPHITE ST",
                     "APPLE WATCH SERIES 7 GPS CELLULAR 45MM MIDNIGHT AL",
                     "APPLE WATCH SERIES 7 GPS CELLULAR 45MM SILVER STAI",
                     "DYSON CORRALE CORD-FREE HAIR STRAIGHTENERS",
                     "DYSON OUTSIZE ABSOLUTE VACUUM CLEANER",
                     "LG 50NANO806PA 2021 LED HDR NANOCELL 4K ULTRA HD S",
                     "LG OLED48A16LA 2021 OLED HDR 4K ULTRA HD SMART TV",
                     "LG OLED48C14LB 2021 OLED HDR 4K ULTRA HD SMART TV",
                     "LG OLED55A16LA 2021 OLED HDR 4K ULTRA HD SMART TV",
                     "LG OLED55B16LA 2021 OLED HDR 4K ULTRA HD SMART TV",
                     "LG OLED55C14LB 2021 OLED HDR 4K ULTRA HD SMART TV",
                     "LG OLED55CX5LB 2020 OLED HDR 4K ULTRA HD SMART TV",
                     "LG OLED55G16LA 2021 OLED HDR 4K ULTRA HD SMART TV",
                     "LG OLED65A16LA 2021 OLED HDR 4K ULTRA HD SMART TV",
                     "LG OLED65B16LA 2021 OLED HDR 4K ULTRA HD SMART TV",
                     "LG OLED65C14LB 2021 OLED HDR 4K ULTRA HD SMART TV",
                     "LG OLED65G16LA 2021 OLED HDR 4K ULTRA HD SMART TV",
                     "LG OLED77C14LB 2021 OLED HDR 4K ULTRA HD SMART TV",
                     "LG TONE FREE HBS-FN4 TRUE WIRELESS BLUETOOTH IN-EA",
                     "LOGITECH COMBO TOUCH FULL-SIZE BACKLIT KEYBOARD CO",
                     "MICROSOFT 365 PERSONAL OFFICE SOFTWARE PC MAC TABL",
                     "mo_other",
                     "OLED55CX5",
                     "RETAILER",
                     "RETAILER CRISP AND FRESH 200 THREAD COU",
                     "RETAILER CROMWELL CHESTERFIELD LARGE 3",
                     "RETAILER LUXURY NATURAL COLLECTION EGYP",
                     "RETAILER NATURAL COTTON QUILTED MATTRES",
                     "RETAILER SYNTHETIC SOFT TOUCH WASHABLE",
                     "SAMSUNG GALAXY A52S SMARTPHONE ANDROID 6GB RAM 6 5",
                     "SAMSUNG GALAXY S21 5G SMARTPHONE WITH WIRELESS POW",
                     "SAMSUNG GALAXY TAB S7 FE TABLET WITH BLUETOOTH S P",
                     "SAMSUNG GALAXY TAB S7 TABLET WITH BLUETOOTH S PEN",
                     "SAMSUNG GALAXY WATCH 4 CLASSIC BLUETOOTH 46MM STAI",
                     "SAMSUNG QE50QN94A 2021 NEO QLED HDR 2000 4K ULTRA",
                     "SAMSUNG QE55QN94A 2021 NEO QLED HDR 2000 4K ULTRA",
                     "SAMSUNG QE55QN95A 2021 NEO QLED HDR 2000 4K ULTRA",
                     "SAMSUNG QE65QN95A 2021 NEO QLED HDR 2000 4K ULTRA",
                     "SAMSUNG QE75Q80A 2021 QLED HDR 1500 4K ULTRA HD SM",
                     "SAMSUNG THE FRAME 2021 QLED ART MODE TV WITH SLIM",
                     "SONY BRAVIA KE48A9 2020 OLED HDR 4K ULTRA HD SMART",
                     "SONY BRAVIA XR XR50X90J 2021 LED HDR 4K ULTRA HD S",
                     "SONY BRAVIA XR XR55A80J 2021 OLED HDR 4K ULTRA HD",
                     "SONY BRAVIA XR XR55A90J 2021 OLED HDR 4K ULTRA HD",
                     "SONY BRAVIA XR XR55X90J 2021 LED HDR 4K ULTRA HD S",
                     "SONY BRAVIA XR XR65A80J 2021 OLED HDR 4K ULTRA HD",
                     "SONY BRAVIA XR XR65A90J 2021 OLED HDR 4K ULTRA HD",
                     "SONY BRAVIA XR XR65X90J 2021 LED HDR 4K ULTRA HD S",
                     "SONY BRAVIA XR XR77A80J 2021 OLED HDR 4K ULTRA HD"

)

# Loop through each value and create a new column for each
for (value in values_to_count) {
  # Create a new column with the count of occurrences
  train_data[[value]] <- rowSums(train_data[, grepl("^model", names(train_data))] == value, na.rm = TRUE)

  # Optionally, replace NA values in the new column with 0
  train_data[[value]][is.na(train_data[[value]])] <- 0
}
train_data

# Remove columns with the word "item" in their names
train_data <- train_data[, !grepl("make", names(train_data))]
train_data <- train_data[, !grepl("model", names(train_data))]
train_data <- train_data[, !grepl("goods_code", names(train_data))]
train_data <- train_data[, !grepl("cash_price", names(train_data))]
train_data <- train_data[, !grepl("prod_purchas", names(train_data))]
train_data <- train_data[, !grepl("item", names(train_data))]
glimpse(train_data)

write.csv(train_data, file = "feature_engineering/train_data_model.csv", row.names = TRUE)


#looking at mo_other
mo_other_subset <- train_data %>% filter(train_data$mo_other > 0)
mo_other_subset
# Check balance of fraud vs non fraud.
ggplot(mo_other_subset, aes(x = factor(fraud_flag), fill = factor(fraud_flag))) +
  geom_bar(stat = "count") +
  scale_x_discrete(labels = c("no fraud", "fraud")) +
  ggtitle("Class Distributions for mo_other: \n (0: No Fraud || 1: Fraud)") +
  theme(plot.title = element_text(size = 10))
glimpse(mo_other_subset)
mo_other_subset$fraud_flag <- as.factor(mo_other_subset$fraud_flag)
summary(mo_other_subset)
fraud_mo_other_subset <- train_data %>% filter(train_data$mo_other > 0)
