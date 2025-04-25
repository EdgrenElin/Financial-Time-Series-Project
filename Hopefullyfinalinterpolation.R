library(zoo)
library(forecast)
library(patchwork)
library(ggplot2)
library(Metrics)
library(dplyr)
library(tseries)
library(patchwork)



data <- read.csv("spiff_data.csv")  #getting time series data

#Taking logs of data
data <- ts(data)
data[,-c(1,2)] <- log(data[,-c(1,2)])

plot.ts(data)

data[c(36,1194,2836,3430,4118),-c(1,2)] <- NA
for (i in c(36,1194,2836,3430,4118)) {
  data[c(i-1,i,i+1),] <- na.approx(data[c(i-1,i,i+1),])   #replacing outliers by mean
}

plot.ts(data)

interpdata <- list()

for (ts in 3:9) {
  miss <- which(is.na(data[,ts]))
  interpdata[[ts-2]] <- ts(data[1:(miss[1]-1),ts])
}

names(interpdata) <- c("gurk","guit","sling","stock","sugar","water","tranq")
logplotlabs <- c("log-gurk","log-guit","log-sling","log-stock","log-sugar","log-water","log-tranq")
logretlabs <- c("logret-gurk","logret-guit","logret-sling","logret-stock","logret-sugar","logret-water","logret-tranq")


for (ts in 1:7) {
  tmp <- interpdata[[ts]]
  logvalues <- autoplot(tmp, ylab = logplotlabs[ts])
  logretvalues <- autoplot(diff(tmp), ylab = logretlabs[ts])
  acfplot <- ggAcf(diff(tmp), type = "correlation", lag.max = 200)
  pacfplot <- ggAcf(diff(tmp), type = "partial", lag.max = 200)
  ljungbox
  print(logvalues / logretvalues / acfplot / pacfplot)
}

#only looking to fit arima to the points before the missing 50 inside the data

#Gurkor has no significant lags in both acf and pacf => possibly ARIMA(0,1,0). Will also test 1,1,0 & 0,1,1 & 1,1,1

#guitars only have a few significant lags later on => possibly ARIMA(0,1,0), will try same models as with gurkor

#Slingshots very similar to guitars, which we have seen from correlations. But, here the first lag is almost significant
#hence, will try same models as above, but also try 2,1,0  & 0,1,2 & 2,1,1 & 1,1,2 & 2,1,2

#for stock, will try same arima as gurkor and guitars

#will do the same for sugar

#will do same for water, there is a big spike in the acf but this is probably due to some already imputed values in the data

#tranquility seems to have both p and q to be significant up to lag 2 and then cuts off. Hence will try the usual and 2,1,2 & 3,1,2 & 2,1,3 & 3,1,3

#------------------------------------------------------------
#We need to do some kind of validation, will divide data into 5 folds.

#                                 train on fold 1, test on fold 2
#                                 train on 1 & 2, test on 3
#                                 train on 1:3, test on 4
#                                 train on 1:4, test on 5

#Then we refit the model that seemed to be the best on the entire set for forecasting 

K <- 5
train1 <- list()
train2 <- list()
train3 <- list()
train4 <- list()

test1 <- list()
test2 <- list()
test3 <- list()
test4 <- list()

#List of lists containing the train/test sets for all data. first index is # folds trained on while second is which dataset

#gurkor = 1, guitars = 2, slingshots = 3, stock = 4, sugar = 5, water = 6, tranquility = 7

train <- list(train1,train2,train3,train4)
test <- list(test1,test2,test3,test4)
for (i in 1:4) {
  l <- i/K
  for (ts in 1:7) {
    trainindex <- 1:(l*length(interpdata[[ts]]))
    train[[i]][[ts]] <- interpdata[[ts]][trainindex]
    
    testindex <- (length(trainindex) + 1):(length(trainindex) + 0.2*length(interpdata[[ts]]))
    test[[i]][[ts]] <- interpdata[[ts]][testindex]
  }
}

#------------------------------------------------------------
#Fitting models for gurkor

autoplot(interpdata[[1]])   #not seeing any 

selfgurkmaes <- list()
autogurkmaes <- list()

for (k in c(0.5,0.6,0.7,0.8,0.9)) {

gurktrain <- interpdata[[1]][1:(k*length(interpdata[[1]]))]
gurktest <- interpdata[[1]][-(1:(k*length(interpdata[[1]])))]

gurkarim <- auto.arima(gurktrain, trace = TRUE)  #Note that multiple models have similar AIC
                                                  #also of interest, model usually changes with training size
gurkarim
checkresiduals(gurkarim)
gurkarimself <- Arima(gurktrain, order = c(0,1,0), include.drift = TRUE)
gurkarimself
checkresiduals(gurkarimself)

forgurkarim <- forecast(gurkarim, h = length(gurktest))
#plot(forgurkarim)
#lines((k*length(interpdata[[1]])):length(interpdata[[1]]), gurktest, col="red")

forgurkarimself <- forecast(gurkarimself, h = length(gurktest))
#plot(forgurkarimself)
#lines((k*length(interpdata[[1]])):length(interpdata[[1]]), gurktest, col="red")

autogurkmaes <- append(autogurkmaes, mae(gurktest, forgurkarim$mean))
selfgurkmaes <- append(selfgurkmaes,mae(gurktest, forgurkarimself$mean))

}

selfgurkmaes
autogurkmaes

selfaveragemae <- 0
autoaveragemae <- 0
for (mae in selfgurkmaes) {
  selfaveragemae <- selfaveragemae + mae[[1]]
}

for (mae in autogurkmaes){
  autoaveragemae <- autoaveragemae + mae[[1]]
}

selfaveragemae <- selfaveragemae/length(selfgurkmaes)
autoaveragemae <- autoaveragemae/length(autogurkmaes)

selfaveragemae
autoaveragemae   #Using auto.arima seems to induce some overfitting, model ARIMA(0,1,0) seems pretty good
#---------------------------------------------------------
#Frequency 12?

sugar <- ts(interpdata[[5]])
plot(sugar)
acf(diff(diff(sugar, lag = 12), lag.max = 50))
pacf(diff(diff(sugar, lag = 12)), lag.max = 50)
test <- ts(data[,7], frequency = 50)
plot(test)

testingarima <- auto.arima(sugar, trace = TRUE, seasonal = TRUE)
plot(ts(testingarima$fitted,frequency = 50))
lines(sugar, col = "red")
forcastintime <- forecast(testingarima, h = 50)
autoplot(forcastintime)

stocks <- ts(interpdata[[4]])
plot.ts(stocks[430:500])
acf(diff(diff(stocks, lag = 12)), lag.max = 100)
pacf(diff(diff(stocks, lag = 24)), lag.max = 100)

acf(diff(diff(interpdata[[1]], lag = 12)), lag.max=100)
pacf(diff(diff(interpdata[[1]], lag = 12)), lag.max=100)
#scrap code underneath
#----------------------------------------------------
#The "best" fitted model by auto.arima seems to fit the data too well for this test/training split
#now fitting for bigger train data

gurktrain <- ts(interpdata[[1]][1:(0.95*length(interpdata[[1]]))])
gurktest <- ts(interpdata[[1]][-(1:(0.95*length(interpdata[[1]])))])

gurkarim <- auto.arima(gurktrain, trace = TRUE)  #waaay more models, all still similar AIC
gurkarim
checkresiduals(gurkarim)
gurkarimself <- Arima(gurktrain, order = c(0,1,0))
gurkarimself
checkresiduals(gurkarimself)

plot(forecast(gurkarim, h = 10))
lines((0.95*length(interpdata[[1]])):length(interpdata[[1]]), gurktest, col="red")

plot(forecast(gurkarimself, h = 10))
lines((0.95*length(interpdata[[1]])):length(interpdata[[1]]), gurktest, col="red")

#Again, arima(0,1,0) seems pretty good for longer forecasting
#testing with smaller set

gurktrain <- ts(interpdata[[1]][1:(0.7*length(interpdata[[1]]))])
gurktest <- ts(interpdata[[1]][-(1:(0.7*length(interpdata[[1]])))])

gurkarim <- auto.arima(gurktrain, trace = TRUE)  #waaay more models, all still similar AIC
gurkarim
checkresiduals(gurkarim)
gurkarimself <- Arima(gurktrain, order = c(0,1,0))
gurkarimself
checkresiduals(gurkarimself)

plot(forecast(gurkarim, h = 50))
lines((0.7*length(interpdata[[1]])):length(interpdata[[1]]), gurktest, col="red")

plot(forecast(gurkarimself, h = 50))
lines((0.7*length(interpdata[[1]])):length(interpdata[[1]]), gurktest, col="red")
#This illustrates that the ARIMA(0,1,0) model without drift would have predicted worse for the smaller data

gurkarim <- auto.arima(ts(interpdata[[1]],frequency = 28), trace = TRUE, seasonal = TRUE)                #We need to determine the frequency/season length somehow, I have not been able to see it
gurkarim
checkresiduals(gurkarim)
plot(forecast(gurkarim, h = 50))
#Looking at the end of tranquility plot
autoplot(ts(interpdata[[7]], frequency = 28))
#season is possibly around 25-30 days
autoplot(diff(ts(interpdata[[7]]), lag = 25))
adf.test(diff(ts(interpdata[[7]]), lag = 25))

acf(diff(ts(interpdata[[7]])), lag.max = 5000)
pacf(diff(ts(interpdata[[7]])), lag.max = 5000)

adf.test(diff(ts(interpdata[[7]])))

#testing the models on test set
traininterptranq <- interpdata[[7]][(1:(0.8*length(interpdata[[7]])))]
testinterptranq <- interpdata[[7]][-(1:(0.8*length(interpdata[[7]])))]

tranqmod <- Arima(traininterptranq, order = c(2,1,2))


autotranqmod <- auto.arima(traininterptranq)
autotranqmod
tranqmod
#Our chosen model is very close to the auto model, but the auto model is simpler which is better

checkresiduals(autotranqmod)
checkresiduals(tranqmod)

forecasting <- forecast(autotranqmod, h = 280)
plot(forecasting)
lines((0.8*length(interpdata[[7]])):length(interpdata[[7]]), testinterptranq, col = "red") 

forecasting2 <- forecast(tranqmod, h = 280)
plot(forecasting2)






#basically no different in long term. Making the test set smaller:

traininterptranq <- interpdata[[7]][(1:(0.95*length(interpdata[[7]])))]
testinterptranq <- interpdata[[7]][-(1:(0.95*length(interpdata[[7]])))]

tranqmod <- Arima(traininterptranq, order = c(2,1,2))


autotranqmod <- auto.arima(traininterptranq)
autotranqmod
tranqmod
#Our chosen model is very close to the auto model, but the auto model is simpler which is better

checkresiduals(autotranqmod)
checkresiduals(tranqmod)

forecasting <- forecast(autotranqmod, h = 70)
plot(forecasting)
lines((0.95*length(interpdata[[7]])):length(interpdata[[7]]), testinterptranq, col = "red") 

forecasting2 <- forecast(tranqmod, h = 70)
plot(forecasting2)

#still basically the same

seasontesting <- ts(interpdata[[7]], frequency = 100)
acf(seasontesting)

testarim <- auto.arima(seasontesting, seasonal = TRUE)
testarim
checkresiduals(testarim)
#test <- rollapply(interpdata[[7]],width = 20, FUN = sd)
#autoplot(test)
#tranq <- ts(interpdata[[7]] frequency = )
