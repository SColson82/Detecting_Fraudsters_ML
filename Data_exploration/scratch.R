library(tidyverse)
library(dplyr)
library(ggplot2)
install.packages("GGally")
library(GGally)
# Every row of ID is unique, setting this as the index.
df <- read.csv("Datasets/X_train_G3tdtEn.csv", row.names = "ID")
df
# df <- read.csv("Datasets/Y_train_2_XPXJDyy.csv", row.names = "ID")
# df
# df<- read.csv("Datasets/X_test_8skS2ey.csv", row.names = "ID")
# df
glimpse(df)
summary(df)

# Checking for duplicates in the dataset:
duplicates <- df[duplicated(df),]
duplicates

# Column names:
feature_names <- colnames(df)
feature_names

# number of rows
nrows <- dim(df)[1]
nrows

# number columns
nfeature <- dim(df)[2]
nfeature


### Create Scatter plots
# Select the numeric columns from your dataframe
numeric_cols <- df %>%
  select_if(is.numeric)

# Get the names of the numeric columns
column_names <- colnames(numeric_cols)

# Create a directory to save the scatterplots (optional)
dir.create("scatterplots", showWarnings = FALSE)

# Loop through each combination of numeric columns and create scatterplots
for (i in 1:(length(column_names) - 1)) {
  for (j in (i + 1):length(column_names)) {
    col1 <- column_names[i]
    col2 <- column_names[j]

    # Create the scatterplot
    plot_title <- paste("Scatterplot of", col1, "vs", col2)
    pdf(paste("scatterplots/scatterplot_", col1, "_vs_", col2, ".pdf"), width = 8, height = 6)  # Adjust the size as needed
    plot(numeric_cols[, col1], numeric_cols[, col2],
         pch = 20,  # Adjust point shape
         cex = 0.7,  # Adjust text size
         main = plot_title,
         xlab = col1,
         ylab = col2)
    dev.off()
  }
}

### Create bar charts
# Select all columns from your dataframe
all_cols <- df

# Create a directory to save the bar charts (optional)
dir.create("bar_charts", showWarnings = FALSE)

# Loop through each combination of columns and create bar charts
for (i in 1:ncol(all_cols)) {
  for (j in 1:ncol(all_cols)) {
    if (i != j) {
      col1 <- colnames(all_cols)[i]
      col2 <- colnames(all_cols)[j]

      # Create a bar chart
      chart_title <- paste("Bar Chart of", col1, "vs", col2)
      pdf(paste("bar_charts/bar_chart_", col1, "_vs_", col2, ".pdf"), width = 8, height = 6)  # Adjust the size as needed
      barplot(table(all_cols[, col1], all_cols[, col2]),
              beside = TRUE,  # Display bars beside each other
              main = chart_title,
              xlab = col1,
              ylab = col2)
      dev.off()
    }
  }
}

### Create histograms
# Select all numeric columns from your dataframe
numeric_cols <- df %>%
  select_if(is.numeric)

# Create a directory to save the histograms (optional)
dir.create("histograms", showWarnings = FALSE)

# Loop through each combination of columns and create histograms
for (i in 1:ncol(numeric_cols)) {
  for (j in 1:ncol(numeric_cols)) {
    if (i != j) {
      col1 <- colnames(numeric_cols)[i]
      col2 <- colnames(numeric_cols)[j]

      # Create histograms
      hist_title <- paste("Histogram of", col1, "vs", col2)
      pdf(paste("histograms/histogram_", col1, "_vs_", col2, ".pdf"), width = 8, height = 6)  # Adjust the size as needed
      hist2d(numeric_cols[, col1], numeric_cols[, col2], main = hist_title)
      dev.off()
    }
  }
}


### Box and Whisker plots
# Select all numeric columns from your dataframe
numeric_cols <- df %>%
  select_if(is.numeric)

# Create a directory to save the box-and-whisker plots (optional)
dir.create("box_plots", showWarnings = FALSE)

# Loop through each combination of columns and create box-and-whisker plots
for (i in 1:ncol(numeric_cols)) {
  for (j in 1:ncol(numeric_cols)) {
    if (i != j) {
      col1 <- colnames(numeric_cols)[i]
      col2 <- colnames(numeric_cols)[j]

      # Create box-and-whisker plots
      boxplot_title <- paste("Box-and-Whisker Plot of", col1, "vs", col2)
      pdf(paste("box_plots/boxplot_", col1, "_vs_", col2, ".pdf"), width = 8, height = 6)  # Adjust the size as needed
      boxplot(numeric_cols[, col1] ~ numeric_cols[, col2], main = boxplot_title)
      dev.off()
    }
  }
}



# Specify the file path where you want to save the output
output_file <- "unique_values_output.txt"

# Open the file for writing
file_conn <- file(output_file, "w")

# Loop through each column and print unique values to the file
for (i in 1:length(unique_values)) {
  cat("\nUnique values in column", colnames(df)[i], ":\n", file = file_conn)
  cat(unique_values[[i]], file = file_conn)
  cat("Unique values in column", colnames(df)[i], ":\n")
  print(unique_values[[i]])
}

# Close the file
close(file_conn)

cat("Unique values have been written to", output_file, "\n")

# Check data types in each column
data_types <- sapply(df, class)
data_types

# I'm thinking of replacing the NAs in all of the cash_price features with 0s but
# I noticed that it appears that 0 is a used value so I want to look where that is the
# case and try to figure out why.
has_zero <- df %>% filter(any(df == 0)==TRUE)
has_zero

# Create a list of cash_price column names
cash_price_columns <- colnames(df)[grepl("^cash_price", colnames(df))]

# Create histograms for each cash_price column with a white background
for (col in cash_price_columns) {
  ggplot(df, aes(x = df[, col])) +
    geom_histogram(binwidth = 100, fill = "lightblue", color = "black") +
    labs(title = col, x = "Cash Price", y = "Frequency") +
    # theme_minimal() +
    theme(panel.background = element_rect(fill = "white"))

  # Save each plot to a file (optional)
  ggsave(filename = paste0("images/",col, "_histogram.png"), plot = last_plot())
}
warnings()

# Create a list of item column names
item_columns <- colnames(df)[grepl("^item", colnames(df))]

# Create histograms for each item column with a white background
for (col in item_columns) {
  p <- ggplot(df, aes(x = df[, col])) +
    geom_histogram(binwidth = 100, fill = "lightblue", color = "black", stat = "count") +
    labs(title = col, x = "Cash Price", y = "Frequency") +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=1.5))  # Increase hjust for more spacing

  # Save each plot to a file (optional)
  ggsave(filename = paste0("images/", col, "_histogram.png"), plot = p)
}

# Create a list of make column names
make_columns <- colnames(df)[grepl("^make", colnames(df))]

# Create bar charts for each make column with a white background
for (col in make_columns) {
  p <- ggplot(df, aes(x = df[, col])) +
    geom_bar(fill = "lightblue", color = "black") +  # Bar chart
    labs(title = col, x = "Make", y = "Frequency") +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))  # Center the labels and add space

  # Widen the x-axis
  p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Center the labels and add space

  # Save each plot to a file (optional)
  ggsave(filename = paste0("images/", col, "_barchart.png"), plot = p)
}

glimpse(df)



