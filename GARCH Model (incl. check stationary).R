#install.packages("readr")
#install.packages("rugarch")
#install.packages("tseries")
#install.packages("xts")

# Load the packages
library(readr)
library(rugarch)
library(tseries)
library(xts)


# Set the working directory 
# setwd("C:/Users/zheng/Downloads")

price_data <- read_csv("price.csv")
head(price_data)


###################################################################################
price_data$date <- as.Date(price_data$date, format="%Y-%m-%d")

# Set the date column as row names
rownames(price_data) <- price_data$date

# Extract the price
price_series <- price_data$price


# Convert price_series to xts object
price_xts <- xts(price_series, order.by = as.Date(price_data$date))

# Augmented Dickey-Fuller (ADF) test:
adf_test <- adf.test(price_xts)
print(adf_test)

# Check the result of the ADF test
if (adf_test$p.value < 0.05) {
  print("The series is stationary.")
} else {
  print("The series is not stationary. Consider differencing or transformation.")
}



###################################################################################

# Apply differencing to make the series stationary
price_diff_xts <- diff(price_xts)

# Re-check stationarity
adf_test_diff <- adf.test(price_diff_xts)
print(adf_test_diff)


spec_garch <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "std"
)

# Fit the GARCH model to the stationary series
fit_garch <- ugarchfit(spec_garch, price_xts)

print(fit_garch)




###################################################################################

plot(fit_garch)
print(residuals(fit_garch))



forecast_garch <- ugarchforecast(fit_garch, n.ahead = 10)
print(forecast_garch)