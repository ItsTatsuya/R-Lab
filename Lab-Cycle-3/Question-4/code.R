# Load the dataset
data(mtcars)

# Convert gear to a factor since it's a categorical variable
mtcars$gear <- as.factor(mtcars$gear)

# Perform one-way ANOVA to test if there are significant differences in displacement (disp) across gear types
anova_test <- aov(disp ~ gear, data = mtcars)

# Output the ANOVA summary in a readable format
cat("One-Way ANOVA Results:\n")
cat("=======================\n")
anova_summary <- summary(anova_test)

# Extract and display key values
cat("F-statistic:", round(anova_summary[[1]]$`F value`[1], 3), "\n")
cat("p-value:", format.pval(anova_summary[[1]]$`Pr(>F)`[1], digits = 3), "\n")
cat("Degrees of Freedom (Between Groups):", anova_summary[[1]]$Df[1], "\n")
cat("Degrees of Freedom (Within Groups):", anova_summary[[1]]$Df[2], "\n")
cat("Mean Squares (Between Groups):", round(anova_summary[[1]]$`Mean Sq`[1], 3), "\n")
cat("Mean Squares (Within Groups):", round(anova_summary[[1]]$`Mean Sq`[2], 3), "\n")
