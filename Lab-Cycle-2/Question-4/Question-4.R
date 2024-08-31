data(mtcars)

mean_mpg <- mean(mtcars$mpg)
sd_mpg <- sd(mtcars$mpg)

hist(mtcars$mpg,
     breaks = 10,  
     col = colorRampPalette(c("lightblue", "darkblue"))(10),
     main = "Histogram of Miles Per Gallon (mpg)",
     xlab = "Miles Per Gallon (mpg)",
     ylab = "Frequency")

abline(v = mean_mpg, col = "red", lwd = 2)
text(mean_mpg, 4, paste("Mean =", round(mean_mpg, 2)), col = "red", pos = 4)
text(mean_mpg, 3, paste("SD =", round(sd_mpg, 2)), col = "red", pos = 4)
