#install.packages("readr")
#install.packages("rugarch")


library(readr)
library(rugarch)

# Set the working directory 
# setwd("C:/Users/zheng/Downloads")

price_data <- read_csv("price.csv")
head(price_data)



######################################################################################
price_data$date <- as.Date(price_data$date, format="%Y-%m-%d")

# Set the date column as row names
rownames(price_data) <- price_data$date

# Extract the price
price_series <- price_data$price




library(xts)

# Convert price_series to xts object
price_xts <- xts(price_series, order.by = as.Date(price_data$date))


# Define the GARCH(1, 1) model
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "std"
)


fit <- ugarchfit(spec, price_xts)
print(fit)





######################################################################################
plot(fit)
print(residuals(fit))


forecast <- ugarchforecast(fit, n.ahead = 10)
print(forecast)