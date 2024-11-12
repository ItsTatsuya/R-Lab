# Part C: Predictions
# File: 3_predictions.R

# Load required libraries
library(caret)
library(ggplot2)
library(grid)

# Load data and models
boston_data <- readRDS("processed_data.rds")
model <- readRDS("full_model.rds")
step_model <- readRDS("step_model.rds")

set.seed(123) # For reproducibility
train_index <- createDataPartition(boston_data$MEDV, p = 0.8, list = FALSE) # 80% train, 20% test
train_data <- boston_data[train_index, ]
test_data <- boston_data[-train_index, ]

# Make predictions on test set with both models
predictions_full <- predict(model, newdata = test_data)
predictions_step <- predict(step_model, newdata = test_data)

# Calculate performance metrics for full model
metrics_full <- data.frame(
    MSE = mean((test_data$MEDV - predictions_full)^2),
    RMSE = sqrt(mean((test_data$MEDV - predictions_full)^2)),
    R2 = cor(test_data$MEDV, predictions_full)^2,
    MAE = mean(abs(test_data$MEDV - predictions_full))
)

# Calculate performance metrics for stepwise model
metrics_step <- data.frame(
    MSE = mean((test_data$MEDV - predictions_step)^2),
    RMSE = sqrt(mean((test_data$MEDV - predictions_step)^2)),
    R2 = cor(test_data$MEDV, predictions_step)^2,
    MAE = mean(abs(test_data$MEDV - predictions_step))
)

# Print and save metrics
print("Full Model Performance Metrics:")
print(metrics_full)
print("\nStepwise Model Performance Metrics:")
print(metrics_step)

write.csv(metrics_full, "full_model_metrics.csv")
write.csv(metrics_step, "step_model_metrics.csv")

# Create a PDF to save all plots
pdf("predictions_plots.pdf", width = 11, height = 8.5)

# 1. Actual vs Predicted Plot for Full Model
ggplot(
    data.frame(actual = test_data$MEDV, predicted = predictions_full),
    aes(x = actual, y = predicted)
) +
    geom_point(alpha = 0.5) +
    geom_abline(intercept = 0, slope = 1, color = "red") +
    theme_minimal() +
    labs(
        title = "Actual vs Predicted Values (Full Model)",
        x = "Actual Home Values",
        y = "Predicted Home Values"
    )
# Add a page break after each plot to separate them
grid.newpage()

# 2. Feature Importance based on absolute t-values
feature_importance <- abs(summary(model)$coefficients[, "t value"])
feature_importance <- sort(feature_importance, decreasing = TRUE)
print("\nFeature Importance (based on absolute t-values):")
print(feature_importance)
write.csv(feature_importance, "feature_importance.csv")

# 3. Create prediction intervals
pred_intervals <- predict(model, newdata = test_data, interval = "prediction")
prediction_data <- data.frame(
    actual = test_data$MEDV,
    predicted = pred_intervals[, "fit"],
    lower = pred_intervals[, "lwr"],
    upper = pred_intervals[, "upr"]
)

# 4. Actual vs Predicted Plot with Prediction Intervals
ggplot(prediction_data, aes(x = actual, y = predicted)) +
    geom_point(alpha = 0.5) +
    geom_abline(intercept = 0, slope = 1, color = "red") +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    theme_minimal() +
    labs(
        title = "Actual vs Predicted Values with Prediction Intervals",
        x = "Actual Home Values",
        y = "Predicted Home Values"
    )

# Finish saving the PDF
dev.off()
