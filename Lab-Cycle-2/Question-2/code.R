data(mtcars)

png(filename = "output.png", width = 10, height = 6, units = "in", res = 300)

plot(mtcars$disp, mtcars$mpg,
     xlab = "Displacement (cu.in.)",
     ylab = "Miles Per Gallon (mpg)",
     main = "Scatterplot of MPG vs Displacement with Cylinders Colored",
     col = as.factor(mtcars$cyl),
     pch = 19
)

lines(lowess(mtcars$disp, mtcars$mpg), col = "black", lwd = 2)

legend("topright",
     legend = levels(as.factor(mtcars$cyl)),
     col = 1:3, pch = 19, title = "Cylinders"
)

dev.off()
