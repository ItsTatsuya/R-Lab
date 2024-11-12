# Load the dataset
data(mtcars)

# View the first few rows of the dataset to confirm the columns
head(mtcars)

# Perform Pearson correlation test between horsepower (hp) and miles per gallon (mpg)
correlation_test <- cor.test(mtcars$hp, mtcars$mpg, method = "pearson")

# Output the results in a readable format
cat("Pearson Correlation Test Results:\n")
cat("====================================\n")
cat("Correlation Coefficient (r):", round(correlation_test$estimate, 3), "\n")
cat("p-value:", format.pval(correlation_test$p.value, digits = 3), "\n")
cat("Confidence Interval:", round(correlation_test$conf.int[1], 3), "to", round(correlation_test$conf.int[2], 3), "\n")
cat("Alternative Hypothesis:", correlation_test$alternative, "\n")
cat("Null Hypothesis:", correlation_test$null.value, "\n")
