# Load required libraries
library(caret)
library(car)
library(MASS)
library(ggplot2)
library(gridExtra) # For grid.table to display tables
library(grid) # For managing page breaks

# Read the original data
boston_data <- read.csv("cleaned_housing-data.csv")

# Create PDF for regression analysis
pdf("regression_analysis.pdf", width = 11, height = 8.5)

# Split data into training and testing sets
set.seed(123)
train_index <- createDataPartition(boston_data$MEDV, p = 0.8, list = FALSE)
train_data <- boston_data[train_index, ]
test_data <- boston_data[-train_index, ]

# 1. Data Split Summary
cat("\nData Split Summary:\n")
cat("Training Set:", nrow(train_data), "observations\n")
cat("Testing Set:", nrow(test_data), "observations\n\n")

# Start a new page in the PDF for the data split summary
grid.newpage()
grid.text("Data Split Summary", y = 0.95)
grid.text(paste("Training Set:", nrow(train_data), "observations"), y = 0.85)
grid.text(paste("Testing Set:", nrow(test_data), "observations"), y = 0.80)

# 2. Full Model Analysis
model <- lm(MEDV ~ ., data = train_data)
model_summary <- summary(model)

# Start a new page for the model summary
grid.newpage()
grid.text("Full Model Summary", y = 0.95)
grid.text(capture.output(model_summary), y = 0.85)

# 3. Diagnostic Plots
# Start a new page for diagnostic plots
grid.newpage()
par(mfrow = c(2, 2))
plot(model, main = "Regression Diagnostics for Full Model")

# 4. VIF Analysis
vif_values <- vif(model)
vif_df <- data.frame(
    Variable = names(vif_values),
    VIF = round(vif_values, 3)
)

# Start a new page for the VIF table
grid.newpage()
grid.text("Variance Inflation Factors (VIF)", y = 0.95)
grid.table(vif_df)

# 5. Stepwise Model Analysis
step_model <- stepAIC(model, direction = "both")
step_model_summary <- summary(step_model)

# Start a new page for the stepwise model summary
grid.newpage()
grid.text("Stepwise Model Summary", y = 0.95)
grid.text(capture.output(step_model_summary), y = 0.85)

# 6. ANOVA Comparison
anova_result <- anova(model, step_model)

# Start a new page for the ANOVA table
grid.newpage()
grid.text("ANOVA Comparison of Models", y = 0.95)
grid.table(as.data.frame(anova_result))

# 7. Residual Analysis
# Create residual plots for both models
par(mfrow = c(2, 2))

# Full Model Residuals
plot(predict(model), residuals(model),
    xlab = "Fitted values", ylab = "Residuals",
    main = "Full Model: Residuals vs Fitted"
)
abline(h = 0, col = "red")

# Full Model Q-Q Plot
qqnorm(residuals(model), main = "Full Model: Normal Q-Q Plot")
qqline(residuals(model), col = "red")

# Stepwise Model Residuals
plot(predict(step_model), residuals(step_model),
    xlab = "Fitted values", ylab = "Residuals",
    main = "Stepwise Model: Residuals vs Fitted"
)
abline(h = 0, col = "red")

# Stepwise Model Q-Q Plot
qqnorm(residuals(step_model), main = "Stepwise Model: Normal Q-Q Plot")
qqline(residuals(step_model), col = "red")

# 8. Coefficient Comparison
coef_full <- coef(model)
coef_step <- coef(step_model)

# Get coefficients that are in both models
common_vars <- intersect(names(coef_full), names(coef_step))

# Create comparison dataframe
coef_comparison <- data.frame(
    Variable = common_vars,
    Full_Model = round(coef_full[common_vars], 4),
    Stepwise_Model = round(coef_step[common_vars], 4)
)

# Start a new page for the coefficient comparison table
grid.newpage()
grid.text("Coefficient Comparison Between Models", y = 0.95)
grid.table(coef_comparison)

# Close PDF
dev.off()

# Save models for later use
saveRDS(model, "full_model.rds")
saveRDS(step_model, "step_model.rds")

# Print confirmation
cat("\nRegression analysis has been saved to 'regression_analysis.pdf'\n")
cat("Models have been saved as 'full_model.rds' and 'step_model.rds'\n")

# Print key findings to console
print("Key Findings:")
cat("\n1. Full Model R-squared:", round(summary(model)$r.squared, 4))
cat("\n2. Stepwise Model R-squared:", round(summary(step_model)$r.squared, 4))
cat("\n3. Number of predictors in full model:", length(coef(model)) - 1)
cat("\n4. Number of predictors in stepwise model:", length(coef(step_model)) - 1)
