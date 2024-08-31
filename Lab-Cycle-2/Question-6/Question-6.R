data(mtcars)

mtcars$carb <- as.factor(mtcars$carb)

library(ggplot2)
ggplot(mtcars, aes(x=disp, y=wt, color=carb, size=carb)) +
  geom_point(alpha=0.7) +
  geom_smooth(method="loess", se=FALSE, color="black", linetype="dashed") +
  scale_color_brewer(palette="Set1") +
  labs(title="Scatter Plot of Displacement vs Weight",
       x="Displacement (cu.in.)",
       y="Weight (1000 lbs)",
       color="Number of Carburetors",
       size="Number of Carburetors") +
  theme_minimal()
