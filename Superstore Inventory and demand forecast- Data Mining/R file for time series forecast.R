library(forecast)
library(zoo)

#Read & Plot Data
scqm <-read.csv("SCQM.csv")

copiers <- subset(scq, Sub.Category == 'Copiers')
phones <- subset(scqm, Sub.Category == 'Phones')
accessories <- subset(scqm, Sub.Category == 'Accessories')
paper <- subset(scqm, Sub.Category == 'Paper')
binders <- subset(scqm, Sub.Category == 'Binders')
chairs <- subset(scqm, Sub.Category == 'Chairs')

#Phones Time Series
phones.ts <- ts(phones$Quantity, start = c(2014,1), end = c(2017,12), frequency = 12)
plot(phones.ts, main='Monthly Quantity')

train <- window(phones.ts, start = c(2014,1), end = c(2017,08))
test <- window(phones.ts, start = c(2017,10), end = c(2017,12))

#Holt Winters ANN (Not Optimal, Best)
hwm <- ets(train, model = "MNM")
Acf(hwm$residuals, main='Holt Winters Residuals ACF')
hwpred <- forecast(hwm, h=4, level=0)
hwpred <- hwpred$mean[2:4]
accuracy(test, hwpred)

hwpred <- forecast(hwm, h=4, level=0)
plot(phones.ts, main = "Phone Quantity Forecast", xlim = c(2014, 2018.3), ylab = 'Quantity')
lines(hwpred$mean, lwd = 2, col = "blue", lty=2)

train <- window(phones.ts, start = c(2014,1), end = c(2017,11))
hwm <- ets(train, model = "MNM")
hwpred <- forecast(hwm, h=4, level=0)
lines(hwpred$mean, lwd = 2, col = "orange", lty=2)

#Accessories Time Series
accessories.ts <- ts(accessories$Quantity, start = c(2014,1), end = c(2017,12), frequency = 12)
plot(accessories.ts, main='Monthly Quantity')

train <- window(accessories.ts, start = c(2014,1), end = c(2017,08))
test <- window(accessories.ts, start = c(2017,10), end = c(2017,12))

#ARIMA Forecasting (Optimal, Best)
Acf(train, main='Train ACF')

am1 <- arima(train, order=c(0,1,0),seasonal = list(order = c(0,1,0), period = 12)) 
Acf(am1$residuals,lag.max=12, main='Residual ACF')
Pacf(am1$residuals,lag.max=12, main='Residual PACF')

am2 <- arima(train, order=c(3,1,3),seasonal = list(order = c(0,1,0), period = 12))
Acf(am2$residuals,lag.max=12, main='Residual ACF')
Pacf(am2$residuals,lag.max=12, main='Residual PACF')

apred <- forecast(am2, h=4, level=0)
apred <- apred$mean[2:4]
accuracy(test, apred)

apred <- forecast(am2, h=4, level=0)
plot(accessories.ts, main = "Accessories Quantity Forecast", xlim = c(2014, 2018.3), ylab = 'Quantity')
lines(apred$mean, lwd = 2, col = "blue", lty=2)

train <- window(accessories.ts, start = c(2014,1), end = c(2017,11))
am2 <- arima(train, order=c(3,1,3),seasonal = list(order = c(0,1,0), period = 12))
apred <- forecast(am2, h=4, level=0)
lines(apred$mean, lwd = 2, col = "orange", lty=2)

#Paper Time Series
paper.ts <- ts(paper$Quantity, start = c(2014,1), end = c(2017,12), frequency = 12)
plot(paper.ts, main='Monthly Quantity')

train <- window(paper.ts, start = c(2014,1), end = c(2017,08))
test <- window(paper.ts, start = c(2017,10), end = c(2017,12))

#Holt Winters ANN (Optimal, Best)
hwm <- ets(train, model = "MAM")
Acf(hwm$residuals, lag.max = 12, main='Holt Winters Residuals ACF')
hwpred <- forecast(hwm, h=4, level=0)
hwpred <- hwpred$mean[2:4]
accuracy(test, hwpred)

hwpred <- forecast(hwm, h=4, level=0)
plot(paper.ts, main = "Paper Quantity Forecast", xlim = c(2014, 2018.3), ylab = 'Quantity')
lines(hwpred$mean, lwd = 2, col = "blue", lty=2)

train <- window(paper.ts, start = c(2014,1), end = c(2017,11))
hwm <- ets(train, model = "MAM")
hwpred <- forecast(hwm, h=4, level=0)
lines(hwpred$mean, lwd = 2, col = "orange", lty=2)

#Binders Time Series
binders.ts <- ts(binders$Quantity, start = c(2014,1), end = c(2017,12), frequency = 12)
plot(binders.ts, main='Monthly Quantity')

train <- window(binders.ts, start = c(2014,1), end = c(2017,08))
test <- window(binders.ts, start = c(2017,10), end = c(2017,12))

#ARIMA Forecasting (Optimal, Best)
Acf(train, main='Train ACF')

am1 <- arima(train, order=c(0,1,0),seasonal = list(order = c(0,1,0), period = 12)) 
Acf(am1$residuals,lag.max=12, main='Residual ACF')
Pacf(am1$residuals,lag.max=12, main='Residual PACF')

am2 <- arima(train, order=c(0,1,1),seasonal = list(order = c(0,1,0), period = 12)) 
Acf(am2$residuals,lag.max=12, main='Residual ACF')
Pacf(am2$residuals,lag.max=12, main='Residual PACF')

apred <- forecast(am2, h=4, level=0)
apred <- apred$mean[2:4]
accuracy(test, apred)

apred <- forecast(am2, h=4, level=0)
plot(binders.ts, main = "Binders Quantity Forecast", xlim = c(2014, 2018.3), ylab = 'Quantity')
lines(apred$mean, lwd = 2, col = "blue", lty=2)

train <- window(binders.ts, start = c(2014,1), end = c(2017,11))
am2 <- arima(train, order=c(0,1,1),seasonal = list(order = c(0,1,0), period = 12))
apred <- forecast(am2, h=4, level=0)
lines(apred$mean, lwd = 2, col = "orange", lty=2)

#Chairs Time Series
chairs.ts <- ts(chairs$Quantity, start = c(2014,1), end = c(2017,12), frequency = 12)
plot(chairs.ts, main='Monthly Quantity')

train <- window(chairs.ts, start = c(2014,1), end = c(2017,08))
test <- window(chairs.ts, start = c(2017,10), end = c(2017,12))

#Holt Winters ANN (Optimal, Best)
hwm <- ets(train, model = "AAA")
Acf(hwm$residuals, lag.max = 12, main='Holt Winters Residuals ACF')
hwpred <- forecast(hwm, h=4, level=0)
hwpred <- hwpred$mean[2:4]
accuracy(test, hwpred)

hwpred <- forecast(hwm, h=4, level=0)
plot(chairs.ts, main = "Chair Quantity Forecast", xlim = c(2014, 2018.3), ylab = 'Quantity')
lines(hwpred$mean, lwd = 2, col = "blue", lty=2)

train <- window(chairs.ts, start = c(2014,1), end = c(2017,11))
hwm <- ets(train, model = "AAA")
hwpred <- forecast(hwm, h=4, level=0)
lines(hwpred$mean, lwd = 2, col = "orange", lty=2)

#Copiers Time Series
copiers.ts <- ts(copiers$Quantity, start = c(2014,1), end = c(2017,4), frequency = 4)
plot(copiers.ts, main='Monthly Quantity')

train <- window(copiers.ts, start = c(2014,1), end = c(2017,2))
test <- window(copiers.ts, start = c(2017,4), end = c(2017,4))

#Forecasting (Random Walk)
Acf(train, main='Train ACF')