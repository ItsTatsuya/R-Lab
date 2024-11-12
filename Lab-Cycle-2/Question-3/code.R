library(ggplot2)


data(mtcars)

mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$am <- as.factor(mtcars$am)

library(ggplot2)
ggplot(mtcars, aes(x = cyl, fill = am)) +
     geom_bar(position = "dodge") +
     scale_fill_manual(values = c("blue", "red"), labels = c("Automatic", "Manual")) +
     labs(
          title = "Number of Cylinders by Transmission Type",
          x = "Number of Cylinders",
          y = "Count",
          fill = "Transmission Type"
     ) +
     theme_minimal()
