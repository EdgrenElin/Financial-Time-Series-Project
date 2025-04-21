library(zoo)
library(forecast)
library(patchwork)
library(ggplot2)
library(Metrics)
library(dplyr)
library(imputeTS)
library(tseries)

data <- read.csv("spiff_data.csv")  #getting time series data

data <- ts(data, frequency = 1)
plot.ts(data)

data[c(36,1194,2836,3430,4118),-c(1,2)] <- NA
for (i in c(36,1194,2836,3430,4118)) {
  data[c(i-1,i,i+1),] <- na.approx(data[c(i-1,i,i+1),])   #replacing outliers by mean
}

par(mfrow = c(4,2))

ts_plots <- list()
for (i in 3:9) {
  ts_plots[[i-2]] <- autoplot(data[,i])
}

(ts_plots[[1]] + ts_plots[[2]]) / (ts_plots[[3]] + ts_plots[[4]]) / (ts_plots[[5]] + ts_plots[[6]]) / (ts_plots[[7]])

par(mfrow = c(1,1))
arim <- auto.arima(log(data[1:198,3]))
arim
plot(arim)
plot.ts(log(data[1:198,3]))
lines(fitted(arim), col = "red")
lines(fitted(arim) + 1.96*sqrt(arim$sigma2), col = "blue")
lines(fitted(arim) - 1.96*sqrt(arim$sigma2), col = "blue")
testpred <- forecast(arim, h = 50)
testpred$x <- exp(testpred$x)
testpred$mean <- exp(testpred$mean)
testpred$upper <- exp(testpred$upper)
testpred$lower <- exp(testpred$lower)
plot(data[,3])
plot(testpred, type = "l")

checkresiduals(arim)

kalman <- na_kalman(log(data[,3]), smooth = TRUE)

var(diff(log(data[1:198,3])))
mean(diff(log(data[1:198,3])))

var(log(data[1:198,3]))
plot.ts(kalman, xlim = c(150,300))

plot.ts(kalman)
plot.ts(ts(log(data[1:198,3])))
arim2 <- auto.arima(log(data[1:398,4]))
arim2
plot.ts(log(data[1:398,4]))
lines(fitted(arim2), col = "red")
lines(fitted(arim2) + 1.96*sqrt(arim2$sigma2), col = "blue")
lines(fitted(arim2) - 1.96*sqrt(arim2$sigma2), col = "blue")

pred2 <- forecast(arim2, h = 50)
pred2$x <- exp(pred2$x)
pred2$mean <- exp(pred2$mean)
pred2$upper <- exp(pred2$upper)
pred2$lower <- exp(pred2$lower)
plot(pred2)


arim3 <- auto.arima(log(data[1:598,5]))
arim3

plot.ts(log(data[1:598,5]))
lines(fitted(arim3), col = "red")
lines(fitted(arim3) + 1.96*sqrt(arim2$sigma2), col = "blue")
lines(fitted(arim3) - 1.96*sqrt(arim2$sigma2), col = "blue")

pred3 <- forecast(arim3, h = 50)
pred3$x <- exp(pred3$x)
pred3$mean <- exp(pred3$mean)
pred3$upper <- exp(pred3$upper)
pred3$lower <- exp(pred3$lower)
plot(pred3)


arim4 <- auto.arima(log(data[1:1198,8]))
arim4

plot.ts(log(data[1:1198,8]))
lines(fitted(arim4), col = "red")
lines(fitted(arim4) + 1.96*sqrt(arim2$sigma2), col = "blue")
lines(fitted(arim4) - 1.96*sqrt(arim2$sigma2), col = "blue")

pred4 <- forecast(arim4, h = 50)
pred4$x <- exp(pred4$x)
pred4$mean <- exp(pred4$mean)
pred4$upper <- exp(pred4$upper)
pred4$lower <- exp(pred4$lower)
plot(pred4)


n6 <- as.ts(diff(data[1:798,6]), frequency = 10)
decompose(n6)
plot.ts(n6)
acf(n6, lag.max = 200)
adf.test(n6)

pacf(n6, lag.max = 200)

arim5 <- auto.arima(n6)
arim5



par(mfrow = c(1,1))
testobj <- data[,6]
testobj <- ts(data[1:798,6], frequency = 25)
plot(decompose(testobj))
acf(diff(data[1:798,6]), lag.max = 100)
pacf(diff(data[1:798,6]), lag.max = 200)

stl1 <- stl(testobj, s.window = "periodic")
plot(stl1)
