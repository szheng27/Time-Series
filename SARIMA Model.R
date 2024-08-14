library(zoo)
library(readxl)
TOTALNSA <- read_excel("TOTALNSA.xls")
View(TOTALNSA)
head(TOTALNSA) 
tail(TOTALNSA)
str(TOTALNSA)

data1 = as.numeric(TOTALNSA$TotalVehicleSales)
data1
boxplot(data1)
abline(h=0)
summary(data1)


#Plot the original data set
original_plot = plot(TOTALNSA$Date,TOTALNSA$TotalVehicleSales,
     xlab = "Date", ylab = "Total Vehicle Sale",
     col = "purple", pch = 19, cex = 0.7,type="l")
#we can see there is obvious seasonality, and maybe trend exist.
#Thus we can try SARIMA model.


#Then we try to find its lags, to make it stationary.
par(mfrow = c(1,2),oma=c(0,0,0,0))
acf(TOTALNSA$TotalVehicleSales,lag.max = 50,
    type = "correlation",main="ACF for the \n Vehicle Sales")
acf(TOTALNSA$TotalVehicleSales,lag.max = 50,
    type = "partial",main="PACF for the \n Vehicle Sales")
#we find that s=12 by ACF and PACF graph


#we will try the first difference wt = (1 ??? B)(1 ??? B^12)y
wt.vehicle = -diff(diff(TOTALNSA$TotalVehicleSales,lag=1),lag=12)
par(mfrow=c(1,1),oma=c(0,0,0,0))
plot(wt.vehicle,type="o",pch=16,cex=.5,xlab='Date',ylab='w(t)',xaxt='n')
axis(1)


#After find the wt, we should find the new lags
par(mfrow=c(1,2),oma=c(0,0,0,0))
acf(wt.vehicle,lag.max=50,type="correlation",main="ACF for w(t)")
acf(wt.vehicle, lag.max=50,type="partial",main="PACF for w(t)")

#-----------------fit1 part----------------------
#Hence an ARIMA(0, 1, 1) ? (0, 1, 1)12 model is used to model the data
#check the ACF&PACF of residual plots 
vehicle.fit.sar1<-arima(TOTALNSA$TotalVehicleSales,
                       order=c(0,1,1),seasonal=list(order = c(0,1,1),period=12),)
res.vehicle.sar1<-as.vector(residuals(vehicle.fit.sar1))
library(forecast)
fit.vehicle.sar1<-as.vector(fitted(vehicle.fit.sar1))
par(mfrow=c(1,2),oma=c(0,0,0,0))
acf(res.vehicle.sar1,lag.max=50,type="correlation",main="ACF of
the Residuals")
acf(res.vehicle.sar1,lag.max=50,type="partial",main="PACF of the
Residuals")


#We use 4-in-1 plot of the residuals
par(mfrow=c(2,4),oma=c(0,0,0,0))
qqnorm(res.vehicle.sar1,datax=TRUE,pch=16,xlab='Residual',main='',col = "purple")
qqline(res.vehicle.sar1,datax=TRUE)
plot(fit.vehicle.sar1,res.vehicle.sar1,pch=16, xlab='Fitted
Value',ylab='Residual', col = "purple")
abline(h=0)
hist(res.vehicle.sar1,col="purple",xlab='Residual',main='')
plot(res.vehicle.sar1,type="l",xlab='Observation Order', ylab='Residual',col = "purple")
points(res.vehicle.sar1,pch=16,cex=.5)
abline(h=0)


#find the (0,1,1)x(0,1,1)model parameters
fit1 = arima(TOTALNSA$TotalVehicleSales, order = c(0,1,1),seasonal = list(order=c(0,1,1),period = 12))
summary(fit1)
#Plot the fitted value and compare to the original plot
par(mfrow=c(1,1),oma=c(0,0,0,0))
plot(fitted(fit1))
lines(TOTALNSA$TotalVehicleSales,col = "purple")

#Find forecast and its plot
forecast(fit1)
plot(forecast(fit1))

#-----------------Compare models----------------------
#Then, we try different models to test p,q, and P,Q lags.
fit1 = arima(TOTALNSA$TotalVehicleSales, order = c(0,1,1),seasonal = list(order=c(0,1,1),period = 12))

fit2 = arima(TOTALNSA$TotalVehicleSales, order = c(1,1,1),seasonal = list(order=c(1,1,1),period = 12))

fit3 = arima(TOTALNSA$TotalVehicleSales, order = c(1,1,1),seasonal = list(order=c(0,1,1),period = 12))

fit4 = arima(TOTALNSA$TotalVehicleSales, order = c(2,1,1),seasonal = list(order=c(1,1,1),period = 12))

fit5 = arima(TOTALNSA$TotalVehicleSales, order = c(2,1,2),seasonal = list(order=c(1,1,1),period = 12))

fit6 = arima(TOTALNSA$TotalVehicleSales, order = c(2,1,1),seasonal = list(order=c(1,1,1),period = 12))
#To compare with more models, we use ARC value
fit1$aic
fit2$aic
fit3$aic
fit4$aic
fit5$aic
fit6$aic
#we find that fit2 has the smallest AIC value, so we will try fit2

#------------------fit2 part------------------------------
#Hence an ARIMA(1, 1, 1) ? (1, 1, 1)12 model is used to model the data
#check the ACF&PACF of residual plots 
vehicle.fit.sar2<-arima(TOTALNSA$TotalVehicleSales,
                       order=c(1,1,1),seasonal=list(order = c(1,1,1),period=12),)
res.vehicle.sar2<-as.vector(residuals(vehicle.fit.sar2))
fit.vehicle.sar2<-as.vector(fitted(vehicle.fit.sar2))
par(mfrow=c(1,2),oma=c(0,0,0,0))
acf(res.vehicle.sar2,lag.max=50,type="correlation",main="ACF of
the Residuals")
acf(res.vehicle.sar2,lag.max=50,type="partial",main="PACF of the
Residuals")


#We use 4-in-1 plot of the residuals
par(mfrow=c(2,2),oma=c(0,0,0,0))
qqnorm(res.vehicle.sar2,datax=TRUE,pch=16,xlab='Residual',main='')
qqline(res.vehicle.sar2,datax=TRUE)
plot(fit.vehicle.sar2,res.vehicle.sar2,pch=16, xlab='Fitted
Value',ylab='Residual')
abline(h=0)
hist(res.vehicle.sar2,col="gray",xlab='Residual',main='')
plot(res.vehicle.sar2,type="l",xlab='Observation Order', ylab='Residual')
points(res.vehicle.sar2,pch=16,cex=.5)
abline(h=0)


#find the (1,1,1)x(1,1,1)model parameters
fit2 = arima(TOTALNSA$TotalVehicleSales, order = c(1,1,1),seasonal = list(order=c(1,1,1),period = 12))
summary(fit2)
#Plot the fitted value and compare to the original plot
par(mfrow=c(1,1),oma=c(0,0,0,0))
plot(fitted(fit2))
lines(TOTALNSA$TotalVehicleSales,col = "purple")

#Find forecast and its plot
par(mfrow=c(1,1),oma=c(0,0,0,0)) 
forecast(fit2)
plot(forecast(fit2))
#------------------fit1 v.s fit2---------------------------
#Plot fit3 compare with fit1
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot(fitted(fit1))
  lines(TOTALNSA$TotalVehicleSales,col = "purple")
plot(fitted(fit2))
  lines(TOTALNSA$TotalVehicleSales,col = "purple")
  
sum(abs(fit1$residuals))
sum(abs(fit2$residuals))

