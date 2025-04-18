library(zoo)
library(forecast)
library(patchwork)
library(ggplot2)
library(Metrics)
library(dplyr)
library(tseries)
library(patchwork)



data <- read.csv("spiff_data.csv")  #getting time series data

data <- ts(data)
plot.ts(data)

data[c(36,1194,2836,3430,4118),-c(1,2)] <- NA
for (i in c(36,1194,2836,3430,4118)) {
  data[c(i-1,i,i+1),] <- na.approx(data[c(i-1,i,i+1),])   #replacing outliers by mean
}

logdata <- data
logdata[,-c(1,2)] <- log(logdata[,-c(1,2)])

logretnames <- c("gurkor logret","guitars logret","slingshots logret","stocks logret","sugar logret","water logret","tranquility logret")
upperconf <- list()
lowerconf <- list()
interp_val <- list()

for (ts in 3:9) {
  par(mfrow = c(3,1))
  #Below chunk transforms data into log returns
  time_s <- data[,c(1,2,ts)]
  time_s[,3] <- log(time_s[,3]) 
  time_s[-1,3] <- diff(time_s[,3])
  time_s <- time_s[-1,]
  time_s <- as.ts(time_s)
  #fitting models to values before NA gaps
  miss <- which(is.na(time_s[which(time_s[,1] < 5000),3]))
  prets <- as.ts(time_s[1:(miss[1]-1),])
  pretx <- prets[,2]
  linmod <- tslm(prets[,3]~pretx)
  acf(prets[,-c(1,2)])
  #plotting data with fitted lines
  plot.ts(prets[,3], ylab = logretnames[ts-2], xlab = "time starting at day 2", main ="Logreturns and fitted linear model")
  abline(linmod, col="blue")
  
  #creating interpolated values with confidence intervals
  pred <- forecast(linmod, newdata = data.frame(pretx = miss))
  plot(forecast(linmod,newdata = data.frame(pretx = miss)), main = "Interpolated/predicted values")
  upper <- pred$upper[,1]
  lower <- pred$lower[,1]
  interp <- pred$mean
  
  upperconf[[ts-2]] <- upper
  lowerconf[[ts-2]] <- lower
  interp_val[[ts-2]] <- interp
  
  upperconf[[ts-2]][1] <- logdata[miss[1],ts] + upperconf[[ts-2]][1]
  lowerconf[[ts-2]][1] <- logdata[miss[1],ts] + lowerconf[[ts-2]][1]
  
  upperconf[[ts-2]] <- cumsum(upperconf[[ts-2]])
  lowerconf[[ts-2]] <- cumsum(lowerconf[[ts-2]])
  
  for (i in miss[-1]) {
    logdata[i,ts] <- interp_val[[ts-2]][i - miss[1]] + logdata[i-1,ts]
  }
  upperconf
}

par(mfrow = c(1,1))
plot(logdata[,2],logdata[,3], type = "l", xlim = c(0,300), ylim = c(1.5,2.2))
lines(logdata[199:248,2],upperconf[[1]][-length(upperconf[[1]])], type = "l", col = "red")
lines(logdata[199:248,2],lowerconf[[1]][-length(lowerconf[[1]])], type = "l", col = "red")

for (ts in 3:9) {
  
  
  
}

#remains to change back from logreturns to actual time series values



#autoplot(forecast(linmod,newdata = data.frame(pretx = miss)))
#plot(forecast(linmod,newdata = data.frame(pretx = miss)))
#plot(time_s[,3], xlim = c(1300,1600))
#lines(miss, upperconf[[7]])
#lines(miss, lowerconf[[7]])
#lines(miss, interp_val[[7]])
