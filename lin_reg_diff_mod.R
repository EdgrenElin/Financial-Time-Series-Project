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

logretnames <- c("gurkor logret","guitars logret","slingshots logret","stocks logret","sugar logret","water logret","tranquility logret")
upperconf <- list()
lowerconf <- list()
interp_val <- list()

for (ts in 3:9) {
  par(mfrow = c(2,1))
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
  
  #plotting data with fitted lines
  plot.ts(prets[,3], ylab = logretnames[ts-2], xlab = "time starting at day 2", main ="Logreturns and fitted linear model")
  abline(linmod, col="blue")
  
  #creating interpolated values with confidence intervals
  pred <- forecast(linmod, newdata = data.frame(pretx = miss))
  plot(forecast(linmod,newdata = data.frame(pretx = miss)), main = "Interpolated/predicted values")
  upper <- pred$upper[,2]
  lower <- pred$lower[,2]
  interp <- pred$mean
  
  upperconf[[ts-2]] <- upper
  lowerconf[[ts-2]] <- lower
  interp_val[[ts-2]] <- interp
}

#remains to change back from logreturns to actual time series values



#autoplot(forecast(linmod,newdata = data.frame(pretx = miss)))
#plot(forecast(linmod,newdata = data.frame(pretx = miss)))
#plot(time_s[,3], xlim = c(1300,1600))
#lines(miss, upperconf[[7]])
#lines(miss, lowerconf[[7]])
#lines(miss, interp_val[[7]])
