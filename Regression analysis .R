getwd()
brainData = read.csv("Brain Size Data.csv", header = TRUE)
str(brainData)
head(brainData)
install.packages('FNN')
brainData = read.csv("Brain Size Data.csv", header = TRUE)
newHeight = 65.5
library(FNN)
preWeight3 = knn.reg(train = brainData$Height, test = newHeight,
                     y = brainData$Weight, k = 3) 
preWeight3
preWeight5 = knn.reg(train = brainData$Height, test = newHeight, y = brainData$Weight, k = 5)
preWeight5
## Simple Linear regression Model 
x = c(0, 10, 20, 30, 40, 50) 
y = c(4, 21, 43, 59, 82, 96) 
plot(x, y)
cor(x, y) ## since data is correlated we can use regression to fit into y=mx+c equation 
lm.data = lm(y~x) #Build linear model. ~ separates dependent from independent variable 
lm.data 
plot(x, y) ## same as earlier graph 
abline(lm.data) ## straight line 
coef(lm.data) ## analysing parameters , orginal data = residuals +fitted , following commands are for analysing data 
resid(lm.data) #Errors for each data point
summary(lm.data) ## checking R squared value for a good fit, if its near 1 its good. 
newX = c(18, 32, 48)  ## predicting Y values for new X values 
y.pre = predict(lm.data, data.frame(x = newX), level=0.95, interval = "confidence") 
y.pre
plot(x, y, col="Blue")
points(newX, y.pre[,1], pch = 16) #Plot points as solid circles abline(lm.data)
abline(lm.data)
## multiple linear regresion model 
oil = read.table("Heating Oil.txt", header = TRUE) 
head(oil)
summary(oil)
cor(oil)
lm.oil = lm(oil$Oil~oil$Temp + oil$Insulation) 
summary(lm.oil)
newTemp = c(38, 64, 48)
newInsu = c(2, 6, 5)
newData = data.frame(Temp = newTemp, Insulation = newInsu)
oil.pre = predict(lm.oil, newData, level=0.95, interval = "confidence")
oil.pre ## gives 15 row output due to R mapping error , to eliminate this go with below mapping 
z = oil$Oil
x = oil$Temp
y = oil$Insulation
lm.oil = lm(z~x + y)
newData = data.frame(x = newTemp, y=newInsu)
oil.pre = predict(lm.oil, newData, level=0.95, interval = "confidence")
oil.pre

