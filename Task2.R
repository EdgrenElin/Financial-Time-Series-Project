#might need to use install under packages for some of the packages
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(patchwork)

data <- read.csv("spiff_data.csv")

#data


#win.graph()
#id <- identify(data$day,data$guitars)   #looks like 36 1194 2836 3430 4118 outliers
#id <- c(36,1194,2836,3430,4118) 
#data[id,-c(1,2)] <- NA

#Plotting each time series
gurk <- ggplot(data[,c(2,3)], aes(x=day, y=gurkor)) +
  geom_line() + ylim(6,15)

guit <- ggplot(data[,c(2,4)], aes(x=day, y=guitars)) +
  geom_line() +ylim(2.5,11)

sling <- ggplot(data[,c(2,5)], aes(x=day, y=slingshots)) +
  geom_line() +ylim(2,6.7)

stock <- ggplot(data[,c(2,6)], aes(x=day, y=stocks)) +
  geom_line() + ylim(4,14)

sugar <- ggplot(data[,c(2,7)], aes(x=day, y=sugar)) +
  geom_line() +ylim(1.5,4.5)

water <- ggplot(data[,c(2,8)], aes(x=day, y=water)) +
  geom_line() + ylim(3.5,9)

tranq <- ggplot(data[,c(2,9)], aes(x=day, y=tranquillity)) +
  geom_line() +ylim(6,22)

(gurk+guit+sling+stock+sugar+water+tranq) 

#Plotting the empty data 
par(mfrow=c(1,1))
gurk1 <- ggplot(data[,c(2,3)], aes(x=day, y=gurkor)) +
  geom_line() +xlim(150,300) + ylim(6.2,6.8)

guit1 <- ggplot(data[,c(2,4)], aes(x=day, y=guitars)) +
  geom_line() +xlim(350,500) + ylim(3.075,3.55)

sling1 <- ggplot(data[,c(2,5)], aes(x=day, y=slingshots)) +
  geom_line() +xlim(550,700) +ylim(2.2,2.75)

stock1 <- ggplot(data[,c(2,6)], aes(x=day, y=stocks)) +
  geom_line() +xlim(750,900) + ylim(10.8,13.5)

sugar1 <- ggplot(data[,c(2,7)], aes(x=day, y=sugar)) +
  geom_line() +xlim(950,1100) + ylim(3,4)

water1 <- ggplot(data[,c(2,8)], aes(x=day, y=water)) +
  geom_line() + xlim(1150,1300) +ylim(4.6,5)

tranq1 <- ggplot(data[,c(2,9)], aes(x=day, y=tranquillity)) +
  geom_line() +xlim(1350,1500) + ylim(8,9.5)

(gurk1+guit1+sling1+stock1+sugar1+water1+tranq1) 

#Creating linear interpolation of the missing points, there are exactly 50 in every asset
linint <- data

#function for finding the sequence to interpolate
firstseqNA <- function(dataset) {
  isna <- which(is.na(dataset))
  return(isna[which(isna < 5000)]) 
  #Brute force since by observation, no NA between day 1450ish to 5200 ish
}

#Getting sequences
guseqNA <- firstseqNA(linint$gurkor)

guiseqNA <- firstseqNA(linint$guitars)

slseqNA <- firstseqNA(linint$slingshots)

stoseqNA <- firstseqNA(linint$stocks)

suseqNA <- firstseqNA(linint$sugar)

waseqNA <- firstseqNA(linint$water)

trseqNA <- firstseqNA(linint$tranquillity)

#guseqNA
#guiseqNA
#slseqNA
#stoseqNA
#suseqNA
#waseqNA
#trseqNA

NAdays <- matrix(c(guseqNA,guiseqNA,slseqNA,stoseqNA,suseqNA,waseqNA,trseqNA),nrow = 50, ncol = 7)

#Function creating linear interpolation
interpLin <- function(dataset, navalues){
  k <- (dataset[tail(navalues[-1],n=1) + 1] - dataset[navalues[1] - 1]) / length(navalues)
  x0 <- dataset[navalues[1]-1]
  return(k*(navalues - navalues[1]) + x0)
}

#Creating the linear interpolations
for (i in 1:7) {
  linint[,i + 2][NAdays[,i]] <- interpLin(linint[,i+2],NAdays[,i])
}

#Plotting the interpolations
gurklin <- ggplot(linint[,c(2,3)], aes(x=day, y=gurkor)) +
  geom_line() +xlim(150,300) + ylim(6.2,6.8)

guitlin <- ggplot(linint[,c(2,4)], aes(x=day, y=guitars)) +
  geom_line() +xlim(350,500) + ylim(3.075,3.55)

slinglin<- ggplot(linint[,c(2,5)], aes(x=day, y=slingshots)) +
  geom_line() +xlim(550,700) +ylim(2.2,2.75)

stocklin <- ggplot(linint[,c(2,6)], aes(x=day, y=stocks)) +
  geom_line() +xlim(750,900) + ylim(10.8,13.5)

sugarlin <- ggplot(linint[,c(2,7)], aes(x=day, y=sugar)) +
  geom_line() +xlim(950,1100) + ylim(3,4)

waterlin <- ggplot(linint[,c(2,8)], aes(x=day, y=water)) +
  geom_line() + xlim(1150,1300) +ylim(4.6,5)

tranqlin <- ggplot(linint[,c(2,9)], aes(x=day, y=tranquillity)) +
  geom_line() +xlim(1350,1500) + ylim(8,9.5)

(gurklin+guitlin+slinglin+stocklin+sugarlin+waterlin+tranqlin)

pairs(newdata)

