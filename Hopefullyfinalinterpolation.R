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
  print(logvalues / logretvalues / acfplot / pacfplot)
}

#only looking to fit arima to the points before the missing 50 inside the data
allorders <- list()
#Gurkor has no significant lags in both acf and pacf => possibly ARIMA(0,1,0). Will also test 1,1,0 & 0,1,1 & 1,1,1
gurkorders <- list()
gurkorders[[1]] <- c(0,1,0)
gurkorders[[2]] <- c(1,1,0)
gurkorders[[3]] <- c(0,1,1)
gurkorders[[4]] <- c(1,1,1)
allorders[[1]] <- gurkorders
#guitars only have a few significant lags later on => possibly ARIMA(0,1,0), will try same models as with gurkor
guitorders <- list()
guitorders[[1]] <- c(0,1,0)
guitorders[[2]] <- c(1,1,0)
guitorders[[3]] <- c(0,1,1)
guitorders[[4]] <- c(1,1,1)
allorders[[2]] <- guitorders
#Slingshots very similar to guitars, which we have seen from correlations. But, here the first lag is almost significant
#hence, will try same models as above, but also try 2,1,0  & 0,1,2 & 2,1,1 & 1,1,2 & 2,1,2
slingorders <- list()
slingorders[[1]] <- c(0,1,0)
slingorders[[2]] <- c(1,1,0)
slingorders[[3]] <- c(0,1,1)
slingorders[[4]] <- c(1,1,1)
slingorders[[5]] <- c(2,1,0)
slingorders[[6]] <- c(0,1,2)
slingorders[[7]] <- c(2,1,1)
slingorders[[8]] <- c(1,1,2)
slingorders[[9]] <- c(2,1,2)
allorders[[3]] <- slingorders
#for stock, will try same arima as gurkor and guitars
stockorders <- list()
stockorders[[1]] <- c(0,1,0)
stockorders[[2]] <- c(1,1,0)
stockorders[[3]] <- c(0,1,1)
stockorders[[4]] <- c(1,1,1)
allorders[[4]] <- stockorders
#will do the same for sugar
sugarorders <- list()
sugarorders[[1]] <- c(0,1,0)
sugarorders[[2]] <- c(1,1,0)
sugarorders[[3]] <- c(0,1,1)
sugarorders[[4]] <- c(1,1,1)
allorders[[5]] <- sugarorders
#will do same for water, there is a big spike in the acf but this is probably due to some already imputed values in the data
waterorders <- list()
waterorders[[1]] <- c(0,1,0)
waterorders[[2]] <- c(1,1,0)
waterorders[[3]] <- c(0,1,1)
waterorders[[4]] <- c(1,1,1)
allorders[[6]] <- waterorders
#tranquility seems to have both p and q to be significant up to lag 2 and then cuts off. Hence will try the usual and 2,1,2 & 3,1,2 & 2,1,3 & 3,1,3
tranqorders <- list()
tranqorders[[1]] <- c(0,1,0)
tranqorders[[2]] <- c(1,1,0)
tranqorders[[3]] <- c(0,1,1)
tranqorders[[4]] <- c(1,1,1)
tranqorders[[5]] <- c(2,1,0)
tranqorders[[6]] <- c(0,1,2)
tranqorders[[7]] <- c(2,1,1)
tranqorders[[8]] <- c(1,1,2)
tranqorders[[9]] <- c(2,1,2)
tranqorders[[10]] <- c(3,1,2)
tranqorders[[11]] <- c(2,1,3)
tranqorders[[12]] <- c(3,1,3)
allorders[[7]] <- tranqorders
#------------------------------------------------------------
#We need to do some kind of validation, will divide data into 5 folds.

#                                 train on fold 1, test on fold 2
#                                 train on 1 & 2, test on 3
#                                 train on 1:3, test on 4
#                                 train on 1:4, test on 5

#Then we refit the model that seemed to be the best on the entire set for forecasting 


#List of lists containing the train/test sets for all data. first index is # folds trained on while second is which dataset

#gurkor = 1, guitars = 2, slingshots = 3, stock = 4, sugar = 5, water = 6, tranquility = 7

K <- 5
train <- list()
test <- list()
for (i in 1:(K-1)) {
  train[[i]] <- list()
  test[[i]] <- list()
  l <- i/K
  for (ts in 1:7) {
    trainindex <- 1:round(l*length(interpdata[[ts]]))

    train[[i]][[ts]] <- interpdata[[ts]][trainindex]
    
    testindex <- (length(trainindex) + 1):round(length(trainindex) + (1/K)*length(interpdata[[ts]]))
    test[[i]][[ts]] <- interpdata[[ts]][testindex]
  }
}
#------------------------------------------------------------
#Fitting all the models

#Initializing vectors
allmodels <- list()     #list contains all models fitted
forecasts <- list()     #list of forecasts to compare with test data
msevalues <- list()     #list of mse for every model
maevalues <- list()     #list of mae for every model
for (i in 1:(K-1)) {
  allmodels[[i]] <- list()  #needed to make lists of lists
  forecasts[[i]] <- list()
  msevalues[[i]] <- list()
  maevalues[[i]] <- list()
  for (ts in 1:7) {
    allmodels[[i]][[ts]] <- list()
    forecasts[[i]][[ts]] <- list()
    msevalues[[i]][[ts]] <- list()
    maevalues[[i]][[ts]] <- list()
    for (ord in 1:length(allorders[[ts]])) {  
      tempmodel <- Arima(train[[i]][[ts]], order = allorders[[ts]][[ord]])        #Fitting the arima model
      allmodels[[i]][[ts]][[ord]]  <- tempmodel                               
      forecasts[[i]][[ts]][[ord]] <- forecast(tempmodel, h = length(test[[i]][[ts]]))    #Creating forecast
      msevalues[[i]][[ts]][[ord]] <- mse(test[[i]][[ts]], forecasts[[i]][[ts]][[ord]]$mean)   #Calculating MSE
      maevalues[[i]][[ts]][[ord]] <- mae(test[[i]][[ts]], forecasts[[i]][[ts]][[ord]]$mean)
    }
  }
}

avgmsevalues <- list()
avgmaevalues <- list()
for (ts in 1:7) {
  avgmsevalues[[ts]] <- list()
  avgmaevalues[[ts]] <- list()
  for (ord in 1:length(allorders[[ts]])) {
    avgmse <- 0
    avgmae <- 0
    for (i in 1:(K-1)) {
      avgmse <- avgmse + msevalues[[i]][[ts]][[ord]]
      avgmae <- avgmae + maevalues[[i]][[ts]][[ord]]
    }
    
    avgmsevalues[[ts]][[ord]] <- avgmse
    avgmaevalues[[ts]][[ord]] <- avgmae
  }
}


#total numbers of models fitted is 164

#models [[1]][[5]][[4]], [[2]][[4]][[4]], [[3]][[3]][[7]], [[4]][[3]][[7]], [[4]][[3]][[8]], [[4]][[3]][[9]], [[4]][[7]][[10]], [[4]][[7]][[12]]
#all have NaN standard errors in some parameters, i.e could not compute the standard error
#Probably because of overfitting
#All forecasts are basically just the mean 
#------------------------------------------------------------
#Plotting the mse values
par(mfrow = c(2,1))
for (ts in 1:7) {
  plot(1:length(avgmsevalues[[ts]]), unlist(avgmsevalues[[ts]]), type = "l", xlab = "models", ylab = "average MSE", main = paste0("avgmse for dataset ", ts))
  plot(1:length(avgmaevalues[[ts]]), unlist(avgmaevalues[[ts]]), type = "l", xlab = "models", ylab = "average MAE", main = paste0("avgmae for dataset ", ts))
}

#Just need to choose models and refit + forecast to get interpolation
