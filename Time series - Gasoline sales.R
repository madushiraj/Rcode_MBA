gas = read.csv("Gasoline Sales.csv", header = TRUE) 
head(gas)
plot(gas)
plot(gas$Week, gas$GallonsSold, type="l")
plot(gas$Week, gas$PriceGallon, type="l")
cor(gas$GallonsSold, gas$PriceGallon) #Correlation
z = gas$GallonsSold
x = gas$Week
y = gas$PriceGallon 
lm.gas = lm(z~x + y)  ## build a linear model , gas price and sales correlated 
summary(lm.gas)
newWeek = c(11, 12, 13)
newPrice = c(4.08, 3.95, 4.02) #Try some random values
newData = data.frame(x = newWeek, y = newPrice)
salesPre = predict(lm.gas, newData, level=0.95, interval = "confidence")
salesPre
## can use double exponential smoothing since data has a trend component 
tsPrice = ts(gas$PriceGallon, gas$Week) #Build time series
tsPriceNoSea = HoltWinters(tsPrice, gamma=FALSE) #No seasonal component 
plot(tsPriceNoSea, cex.main = 0.75)
install.packages('forecast')
install.packages('forecast', dependencies = TRUE)
library(forecast)
tsPriceForecast = forecast(tsPriceNoSea, 3) 
plot(tsPriceForecast)
newDataForecast = data.frame(x = newWeek, y = tsPriceForecast$mean[1:3]) 
salesPreForecast = predict(lm.gas, newDataForecast, level=0.95, interval = "confidence") 
salesPreForecast