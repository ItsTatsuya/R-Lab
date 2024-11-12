data(mtcars)

mtcars$gear <- as.factor(mtcars$gear)

boxplot_hp <- boxplot(mtcars$hp ~ mtcars$gear,
     main = "Box Plot of Horsepower by Number of Gears",
     xlab = "Number of Gears",
     ylab = "Horsepower (hp)",
     col = "lightblue",
     pch = 19
)

legend("topright", legend = levels(mtcars$gear), pch = 19, title = "Number of Gears")

outliers <- boxplot_hp$out

if (length(outliers) > 0) {
     outlier_positions <- which(mtcars$hp %in% outliers)

     text(rep(1:3, length.out = length(outliers)), outliers,
          labels = rownames(mtcars)[outlier_positions], pos = 4, col = "red"
     )
}
