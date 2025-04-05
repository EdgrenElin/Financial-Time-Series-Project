library(zoo)
library(forecast)
library(patchwork)
library(ggplot2)
library(Metrics)
library(dplyr)

data <- read.csv("spiff_data.csv")  #getting time series data

data <- ts(data)
plot.ts(data)
 
data[c(36,1194,2836,3430,4118),-c(1,2)] <- NA
for (i in c(36,1194,2836,3430,4118)) {
  data[c(i-1,i,i+1),] <- na.approx(data[c(i-1,i,i+1),])   #replacing outliers by mean
}

gurkor <- data[,3]    #Specifically looking at gurkor

which(is.na(gurkor))    #Looking for missing values in gurkor

pre_gurkor <- gurkor[1:198]   #Creating object consisting of values before the NA
plot.ts(pre_gurkor)   

#Looking at the plot, there does not seem to be any seasonality
#furthermore, it looks quite linear

acf(pre_gurkor)     #Looking at acf, doesn't seem to be any seasonality

#Since it looks linear, we look at the differences between each time step
diffpre_gurkor <- diff(pre_gurkor)
plot.ts(diffpre_gurkor)


#New plot looks pretty stationary and the variance seems to be about constant and the mean looks to be zero
#this suggests it might be a white noise process.

acf(diffpre_gurkor)  

#acf looking veeeery niiiice. Each lag looks to be inside the 95% confidence intervals.
#Hence, looks like white noise is reasonable => we fit an ARIMA(0,1,0) since we differenced once.


ii <- 1:(198 - 20)

train_gurk <- pre_gurkor[ii]   #creating a trining set
test_gurk <- pre_gurkor[-ii]   #creating test set

fit <- Arima(train_gurk, order = c(0,1,0))   #Creating arima model, (0,1,0) is just a random walk

plot.ts(train_gurk)
lines(fit$x, col = "red", lty = 2)

checkresiduals(fit)     # everything looks really good, low acf everywhere, residuals looks aproximately normal distributed. But mean is a little shifted to the right

fit2 <- Arima(train_gurk, order = c(0,1,0), include.drift = TRUE) 

checkresiduals(fit2)    #Residuals are way better with drift included

#This model looks pretty good. There are ones which give smaller AIC 

optarim <- auto.arima(train_gurk)
optarim
plot(optarim$x)

checkresiduals(optarim)

#Comparing the models

optarim     
fit2

pred_optarim <- forecast(optarim,20)
plot(pred_optarim)
lines(179:198, test_gurk, col = "orange")
lines(train_gurk, col = "red")

pred_fit2 <- forecast(fit2,20)
plot(pred_fit2)
lines(179:198, test_gurk, col = "orange")
lines(train_gurk, col = "red")

mae_fit2 <- mae(pred_fit2$mean, test_gurk)
mae_optarim <- mae(pred_optarim$mean, test_gurk)

mae_fit2
mae_optarim

#Will probably want to do a cross validation using models with different parameters but this is preliminary

#We see that the test statistics for optarim and fit2 are really close, with optarim being a little better
#Now refit on entire data and forecast 25 points, we specifically do ARIMA(0,1,0) with drift

fullfit <- Arima(pre_gurkor, order = c(0,1,0), include.drift = TRUE)

checkresiduals(fullfit)   #very nice 

forcastfull <- forecast(fullfit, h=25)
autoplot(forcastfull)

newdata <- data
newdata[199:223,3] <- forcastfull$mean

autoplot(as.ts(newdata[1:223,3]))
#tested some time series linear regression as well, this gives pred intervals which is very nice
gurk_x <- as.ts(1:198)
testmod <- tslm(as.ts(pre_gurkor)~gurk_x)
testmod

plot.ts(pre_gurkor)
abline(testmod)
fc <- forecast(testmod, newdata = data.frame(gurk_x = 199:223))
plot(fc)
