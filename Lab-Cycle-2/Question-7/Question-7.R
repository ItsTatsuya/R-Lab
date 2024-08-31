library(ggplot2)
library(dplyr)

data <- read.csv("C:/Users/shiya/Downloads/archive/time-series-19-covid-combined.csv")

head(data)

data$Date <- as.Date(data$Date, format="%Y-%m-%d")

daily_data <- data %>%
  group_by(Date) %>%
  summarise(Confirmed = sum(Confirmed, na.rm = TRUE))

head(daily_data)

ggplot(daily_data, aes(x=Date, y=Confirmed)) +
  geom_line(color="blue") +
  labs(title="COVID-19 Confirmed Cases Over Time",
       x="Date",
       y="Number of Confirmed Cases") +
  theme_minimal()
