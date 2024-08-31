data(iris)
png(filename = "sepal_vs_petal_length_scatterplot.png", width = 10, height = 6, units = "in", res = 300)
plot(iris$Sepal.Length, iris$Petal.Length,
     xlab = "Sepal Length (cm)",
     ylab = "Petal Length (cm)",
     main = "Scatterplot of Sepal Length vs Petal Length",
     col = "blue",
     pch = 19)
dev.off()
