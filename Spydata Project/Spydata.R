library(quantmod)
library(fpp)
library(backtest)


spy <- getSymbols("SPY", from="2010-10-01", src="yahoo", auto.assign=F)
head(spy)

spy <- spy[1:1600,]
adj <- spy[,6]


plot(spy[,6], main = "Adhusted Closing Price")


logr <- periodReturn(adj,period = "daily", type = "log", leading = TRUE)
logr <- 1+logr
head(logr)


par(mfrow = c(2,1))
acf.logr = acf(logr, main='ACF Plot', lag.max=100)
pacf.logr = pacf(logr, main='PACF Plot', lag.max=100)
print(adf.test(logr))


m1 <- auto.arima(logr, seasonal = FALSE)
m1
summary(m1)


x <- logr
length(x)

artest1 <- arima(logr, order = c(1,0,0))
artest2 <- arima(logr, order = c(2,0,0))
artest3 <- arima(logr, order = c(3,0,0))



accuracy(forecast(artest1))
accuracy(forecast(artest2))
accuracy(forecast(artest3))

m2 <- artest2 # as it provides highest forecasting accuracy

mtest1 <- arima(logr, order = c(0,0,1))
mtest2 <- arima(logr, order = c(0,0,2))
mtest3 <- arima(logr, order = c(0,0,3))
mtest1;mtest2;mtest3

accuracy(forecast(mtest1))
accuracy(forecast(mtest2))
accuracy(forecast(mtest3))

m3 <- mtest3 # based on lower AIC value

source("backtest.R")
ts <- ts(logr, start = start(logr), end = end(logr), frequency = 1)
b1 <- backtest(m1 = m1, ts, orig = 1600, h = 1)
b2 <- backtest(m1 = m2, ts, orig = 1600, h = 1)
b3 <- backtest(m1 = m3, ts, orig = 1600, h = 1)

b1e <- backtest(m1 = m1, ts, orig = 1961, h = 362)
b2e <- backtest(m1 = m2, ts, orig = 1961, h = 362)
b3e <- backtest(m1 = m3, ts, orig = 1961, h = 362)

sum(b1e$mabso)
sum(b1e$mabso)
sum(b1e$mabso)

MAPEb1e <- (sum(b1e$mabso)/length(b1e$mabso))
accu1e <- 100-MAPEb1e

MAPEb2e <- (sum(b2e$mabso)/length(b2e$mabso))
accu2e <- 100-MAPEb2e

MAPEb3e <- (sum(b3e$mabso)/length(b3e$mabso))
accu3e <- 100-MAPEb3e

accu1e
accu2e
accu3e

# as the SMAE is quite similar and calculating MAPE for three proposed model 
# we found the probability of the prediction is right to 99% for all these model
# so we conclude, for this particular problem, ARIMA, AR and MA all performed 
# equally accurately.