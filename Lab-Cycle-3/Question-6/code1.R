# Load required libraries
library(ggplot2)
library(corrplot)
library(dplyr)
library(tidyr)
library(gridExtra)

# Read the data
boston_data <- read.csv("cleaned_housing-data.csv")

# Create PDF file for all visualizations
pdf("boston_housing_analysis.pdf", width = 10, height = 8)

# 1. Print Summary Statistics
textplot <- grid.table(summary(boston_data))

# 2. Correlation Matrix Plot
corrplot(cor(boston_data),
  method = "color", type = "upper",
  addCoef.col = "black", number.cex = 0.7,
  tl.col = "black", tl.srt = 45,
  main = "Correlation Matrix"
)

# 3. Distribution Plots
par(mfrow = c(3, 5)) # Arrange plots in a 3x5 grid
for (col in names(boston_data)) {
  hist(boston_data[[col]],
    main = paste("Distribution of", col),
    xlab = col, col = "skyblue", border = "white"
  )
}

# 4. Scatter Plots vs MEDV
par(mfrow = c(2, 2))
important_vars <- c("RM", "LSTAT", "PTRATIO", "TAX")
for (var in important_vars) {
  plot(boston_data[[var]], boston_data$MEDV,
    main = paste("Scatter plot:", var, "vs MEDV"),
    xlab = var, ylab = "Median Home Value",
    pch = 19, col = rgb(0, 0, 1, 0.5)
  )
  abline(lm(MEDV ~ get(var), data = boston_data), col = "red")
}

# 5. Box Plots
par(mfrow = c(3, 5))
for (col in names(boston_data)) {
  boxplot(boston_data[[col]],
    main = paste("Boxplot of", col),
    col = "skyblue", border = "black"
  )
}

# 6. Add regression diagnostics
model <- lm(MEDV ~ ., data = boston_data)
par(mfrow = c(2, 2))
plot(model, main = "Regression Diagnostics")

# Close PDF device
dev.off()

# Print confirmation
print("Analysis has been saved to 'boston_housing_analysis.pdf'")

# Print summary statistics to console
print("Summary Statistics:")
print(summary(boston_data))

# Print model summary to console
print("\nRegression Model Summary:")
print(summary(model))
