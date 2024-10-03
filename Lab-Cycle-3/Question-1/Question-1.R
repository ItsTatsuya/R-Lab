# Load necessary libraries
library(ggplot2)
 
# a) Load the train and test datasets
train_df <- read.csv("./train.csv")
test_df <- read.csv("./test.csv")
 
# View the structure of the datasets to understand the variables
str(train_df)
str(test_df)
 
# Let's use 'GrLivArea' as 'size' and 'SalePrice' as 'price' in train_df
# Filter out NA values in the train dataset
train_df <- train_df[!is.na(train_df$GrLivArea) & !is.na(train_df$SalePrice), ]
 
# c) Fit a simple linear regression model using train data
model <- lm(SalePrice ~ GrLivArea, data = train_df)
 
# d) Calculate the regression coefficients
coefficients <- coef(model)
intercept <- coefficients[1]  # Intercept
slope <- coefficients[2]  # Slope
 
cat("Intercept: ", intercept, "\n")
cat("Slope: ", slope, "\n")
 
# e) Plot the regression line along with the scatter plot of the training data points # nolint
ggplot(train_df, aes(x = GrLivArea, y = SalePrice)) +
  geom_point(color = "blue") +  # Scatter plot
  geom_abline(intercept = intercept, slope = slope, color = 'red', linetype = 'dashed') +  # Regression line # nolint
  labs(title = "House Prices vs Size (Training Data)", x = "Size (Square Feet)", y = "Price") + # nolint
  theme_minimal()
 
# Prediction on test dataset
# Assuming 'GrLivArea' is available in test dataset, but 'SalePrice' is not
# Remove any NA values in GrLivArea
test_df <- test_df[!is.na(test_df$GrLivArea), ]
 
# Use the model to predict house prices in the test dataset
test_df$PredictedPrice <- predict(model, newdata = test_df)
 
# If test_df has actual house prices, we can compare; otherwise, we'll only plot predictions # nolint
# Assuming there's no actual 'SalePrice' in the test dataset:
# Plot the predicted prices vs size in the test dataset
ggplot(test_df, aes(x = GrLivArea, y = PredictedPrice)) +
  geom_point(color = "green") +  # Scatter plot of predictions
  labs(title = "Predicted House Prices vs Size (Test Data)", x = "Size (Square Feet)", y = "Predicted Price") + # nolint
  theme_minimal()
 
# If actual SalePrice exists in the test data, you can plot both predictions and actuals # nolint
# Uncomment the lines below if SalePrice is present in test_df
# ggplot(test_df, aes(x = GrLivArea)) +
#   geom_point(aes(y = SalePrice), color = "blue", alpha = 0.5) +  # Actual prices # nolint
#   geom_point(aes(y = PredictedPrice), color = "red", alpha = 0.5) +  # Predicted prices # nolint
#   labs(title = "Actual vs Predicted House Prices", x = "Size (Square Feet)", y = "Price") + # nolint
#   theme_minimal() # nolint