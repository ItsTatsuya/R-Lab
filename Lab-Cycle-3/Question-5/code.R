# Load necessary libraries
library(dplyr)
library(forecast)

# Load the CSV file
covid_data <- read.csv("time-series-19-covid-combined.csv")

# Rename columns for consistency with the script
colnames(covid_data) <- c("date", "location", "province", "total_cases", "total_recovered", "total_deaths")

# Convert the 'date' column to Date type
covid_data$date <- as.Date(covid_data$date)

# Filter the data for India within the specified date range
india_data <- covid_data %>%
    filter(location == "India" & date >= as.Date("2020-01-22") & date <= as.Date("2020-12-15"))

# Check if data was filtered correctly
if (nrow(india_data) == 0) {
    stop("No data available for India in the specified date range.")
}

# Extract the total positive cases and total deaths columns
positive_cases <- india_data$total_cases
total_deaths <- india_data$total_deaths

# Create a time series object for total positive cases (weekly frequency)
positive_cases_ts <- ts(positive_cases, start = c(2020, 1), frequency = 7)

# Plot the time series for positive cases
png("positive_cases_plot.png")
plot(positive_cases_ts,
    type = "l", col = "blue",
    main = "Total Positive COVID-19 Cases in India (Weekly)",
    xlab = "Weeks", ylab = "Total Positive Cases"
)
dev.off()

# Create a multivariate time series with both positive cases and deaths
covid_multivariate_ts <- ts(cbind(positive_cases, total_deaths), start = c(2020, 1), frequency = 7)

# Plot both series on a single chart
png("multivariate_plot.png")
plot(covid_multivariate_ts,
    plot.type = "single", col = c("blue", "red"),
    main = "Total Positive COVID-19 Cases and Deaths in India (Weekly)",
    xlab = "Weeks", ylab = "Count"
)
legend("topright", legend = c("Total Positive Cases", "Total Deaths"), col = c("blue", "red"), lty = 1)
dev.off()

# Fit an ARIMA model to the total positive cases
arima_model <- auto.arima(positive_cases_ts)
summary(arima_model)

# Forecast the next 5 weeks
forecast_cases <- forecast(arima_model, h = 5)

# Save the forecast output to a text file
sink("output.txt")
print(forecast_cases)
sink()

# Plot the forecast
png("forecast_plot.png")
plot(forecast_cases,
    main = "Forecast of Total Positive COVID-19 Cases in India",
    xlab = "Weeks", ylab = "Total Positive Cases"
)
dev.off()
