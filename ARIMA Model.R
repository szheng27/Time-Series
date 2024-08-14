library(sarima)
library(forecast)
library(readxl)
library(dplyr)
library(tseries)
library(car)
#simulate 2 example ARIMA (1,1,1)
x <- arima.sim(list(order = c(1,1,1), ar = 0.6, ma = -0.5), n = 1000)
plot(x)
acf(x, type = 'correlation')
acf(x, type = 'partial')

x2 <- arima.sim(list(order = c(1,1,1), ar = 0.6, ma = 0.5), n = 1000)
ts.plot(x2)
acf(x2, type = 'correlation')
acf(x2, type = 'partial')


#################################################################################
#Option I: read from excel:
#FIT and test real data model, set as template, need discuss by customer
POS_Data <- read_excel("C:/Users/zheng/Downloads/MAV POS data.xlsx")
SDM_POS <- filter(POS_Data, Customer == 'SDM')
WM_POS <- filter(POS_Data, Customer == 'WM CA')


##################################################################################
# Option II: Load using SQL database
library(DBI)
library(RSQLite)

# database connection
con <- dbConnect(RSQLite::SQLite(), dbname = "POS.db")

#join tables using UPC
query <- "
  SELECT
    p.UPC,
    p.product_name,
    p.price,
    s.sales_quantity,
    s.sales_date,
    c.Customer,
    c.currency
  FROM
    products AS p
  JOIN
    sales AS s
  ON
    p.UPC = s.UPC
  JOIN
    customer AS c
  ON
    s.customer_id = c.customer_id
"

# Query the joined data from the database
POS_Data <- dbGetQuery(con, query)

#Close database connection
dbDisconnect(con)

# View the first few rows of the data
head(POS_Data)


#FIT and test real data model, set as template, need discuss by customer
SDM_POS <- filter(POS_Data, Customer == 'SDM')
WM_POS <- filter(POS_Data, Customer == 'WM CA')




##############################################################################################
#About 4327OIL OF MOROCCO MASK TRTMENT in SDM
x1 = SDM_POS$`621732704327`
ts.plot(x1)

x1Diff = diff(x1,2)
plot(x1Diff, main = 'ARIMA with 1 difference',xlab = 'week', ylab = 'Difference', type = 'l')

acf(x1Diff, type = 'correlation', main = "ACF of ARIMA, 1 Difference")
acf(x1Diff, type = 'partial', main = "PACF of ARIMA, 1 Difference")

#fit the model 
x1d_arima <- arima(x1Diff, order = c(4,1,2))
#ACF of resid
acf(resid(x1d_arima))
res.x1 <- as.vector(residuals(x1d_arima))
fit.x1 <- as.vector(fitted(x1d_arima))

qqnorm(res.x1, datax = TRUE, pch=16)
qqline(res.x1, datax = TRUE)

plot(fit.x1,res.x1,pch=16, xlab='Fitted Value',ylab='Residual')
abline(h=0)
#run a Ljung-Box test to provide statistical evidence of a good fit:
Box.test(resid(x1d_arima), type = "Ljung-Box")
Box.test(fitted(x1d.arima), type = "Ljung-Box")
#p value greater than 0.05, but residual does not follow the normal assumption.
######################################################################################

#About 3260 CURL ENVY Cream in SDM
x2 = SDM_POS$`621732003260`
ts.plot(x2)

x1Diff = diff(x2,1)
plot(x1Diff, main = 'ARIMA with 1 difference',xlab = 'week', ylab = 'Difference', type = 'l')
#Augmented Dickey-Fuller test
adf.test(x2Diff)

x2Diff = diff(x2,2)
plot(x2Diff, main = 'ARIMA with 2 difference',xlab = 'week', ylab = 'Difference', type = 'l')
#Augmented Dickey-Fuller test
adf.test(x2Diff)

acf(x2Diff, type = 'correlation', main = "ACF of ARIMA, 2 Difference")
acf(x2Diff, type = 'partial', main = "PACF of ARIMA, 2 Difference")


#fit the model 
x2d.arima <- arima(x2Diff, order = c(4,2,2))


res.x2 <- as.vector(residuals(x2d.arima))
fit.x2 <- as.vector(fitted(x2d.arima))

#ACF of resid
acf(resid(x2d.arima), main = "ACF of Residuals ARIMA(4,2,2)")

qqnorm(res.x2, datax = TRUE, pch=16, main = "Normal QQ Plot - ARIMA(4,2,2)")
qqline(res.x2, datax = TRUE)
qqnorm(scale(x2d.arima$residuals));abline(0,1)

plot(fit.x2,res.x2,pch=16, xlab='Fitted Value',ylab='Residual', main = "Fitted Value vs Residuals - ARIMA(4,2,2)")
abline(h=0)

hist(res.x2,xlab='Residual',main='Histogram of Residual - ARIMA(9,2,4)')


#run a Ljung-Box test to provide statistical evidence of a good fit:
Box.test(resid(x2d.arima), lag = 5, type = "Ljung-Box") #0.9974
#p value greater than 0.05 for the residual, and fitted model. we say it is a good fit.

plot(forecast(x2d.arima, h=13))
pred.sz <-predict(x2d.arima, n.head=13)
prediction.sz <-pred.sz$pred


summary(x2d.arima)
######################################################################################
#About 4327 OIL OF MOROCCO MASK TRTMENT in in WMCA
y1 <- WM_POS$`621732704327`
ts.plot(y1)

y1Diff = diff(y1,2)
plot(y1Diff, main = 'ARIMA with 1 difference',xlab = 'week', ylab = 'Difference', type = 'l')

acf(y1Diff, type = 'correlation', main = "ACF of ARIMA, 1 Difference")
acf(y1Diff, type = 'partial', main = "PACF of ARIMA, 1 Difference")

#fit the model 
y1d.arima <- arima(y1Diff, order = c(2,1,3))


res.y1 <- as.vector(residuals(y1d.arima))
fit.y1 <- as.vector(fitted(y1d.arima))

#ACF of resid
acf(resid(y1d.arima))

qqnorm(res.y1, datax = TRUE, pch=16)
qqline(res.y1, datax = TRUE)

plot(fit.y1,res.y1,pch=16, xlab='Fitted Value',ylab='Residual')
abline(h=0)

hist(res.y1,xlab='Residual',main='Histogram of Residual')


#run a Ljung-Box test to provide statistical evidence of a good fit:
Box.test(resid(y1d.arima), type = "Ljung-Box")
Box.test(fitted(y1d.arima), type = "Ljung-Box")
#p value greater than 0.05 for residual but less than 0.05 for the fitted. Not a good fit
######################################################################################
#About 3260 CURL ENVY Cream in WMCA
y2 <- WM_POS$`621732003260`
ts.plot(y2)

y2Diff = diff(y2,2)
plot(y2Diff, main = 'ARIMA with 10 difference',xlab = 'week', ylab = 'Difference', type = 'l')

acf(y2Diff, type = 'correlation', main = "ACF of ARIMA, 10 Difference")
acf(y2Diff, type = 'partial', main = "PACF of ARIMA, 10 Difference")

#fit the model 
y2d.arima <- arima(y2Diff, order = c(9,2,7))


res.y2 <- as.vector(residuals(y2d.arima))
fit.y2 <- as.vector(fitted(y2d.arima))

#ACF of resid
acf(resid(y2d.arima))

qqnorm(res.y2, datax = TRUE, pch=16)
qqline(res.y2, datax = TRUE)

plot(fit.y2,res.y2,pch=16, xlab='Fitted Value',ylab='Residual')
abline(h=0)

hist(res.y2,xlab='Residual',main='Histogram of Residual')


#run a Ljung-Box test to provide statistical evidence of a good fit:
Box.test(resid(y2d.arima), type = "Ljung-Box")
Box.test(fitted(y2d.arima), type = "Ljung-Box")
#p value greater than 0.05 for residual but less than 0.05 for the fitted. Not a good fit

######################################################################################