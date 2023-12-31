xTrainData <- read.csv("Datasets/X_train_G3tdtEn.csv",row.names = "ID")
xTrainData <- apply(xTrainData, 2, function(x) ifelse(x == "", NA, x))
xTrainData
yTrainData <- read.csv("Datasets/Y_train_2_XPXJDyy.csv")
yTrainData


# Replace empty strings with NA
xTrainData <- apply(xTrainData, 2, function(x) ifelse(x == "", NA, x))

# Apply table function across all columns, ignoring NA values
frequency_table <- table(unlist(apply(xTrainData[, paste0("make", 1:24)], 2, function(x) na.omit(as.character(x)))))

# Convert the table to a data frame for better handling
frequency_df <- as.data.frame(frequency_table)
colnames(frequency_df) <- c("Value", "Frequency")
glimpse(frequency_df)
write.csv(frequency_df, "feature_engineering/unique_Make_counts.csv", row.names = FALSE)
# Create a vector of colors based on unique values
value_colors <- rainbow(length(frequency_df$Value))

# Set the layout to allow for multiple plots
par(mfrow = c(1, 2), mar = c(5, 5, 4, 2) + 0.1)

# Create a larger plot with rotated axis labels, logarithmic scale, and adjusted padding
png("Make_bar_chart.png", width = 1500, height = 800)  # Adjust width and height as needed
barplot(frequency_df$Frequency, names.arg = frequency_df$Value,
        col = value_colors, main = "Bar Chart of Unique Make Values Frequency (Log Scale)",
        ylab = "Frequency (Log Scale)", log = "y", las = 2, xaxt = "n", cex.names = 0.8)
mtext(side = 1, line = 3, " ")  # Empty title
dev.off()

# Open a new PNG device for the legend
png("Make_legend.png", width = 1500, height = 800)  # Adjust width and height as needed

# Call plot.new() before creating the legend
plot.new()

# Adjust the plot margin to make room for the legend
par(mar = c(5, 15, 4, 2) + 0.1)

# Determine the midpoint to split the legend
midpoint <- length(frequency_df$Value) %/% 3

# Create a legend to the right of the plot
legend("topright", legend = frequency_df$Value[1:midpoint], fill = value_colors[1:midpoint], title = "Unique Values",
       horiz = FALSE, xpd = TRUE, bty = "n", cex = 0.8, ncol=6)

# Save the legend and close the device
dev.off()
