#libraries 
library()
install.packages("naivebayes")
install.packages("ggplot")
library(naivebayes)
library(dplyr)
library(ggplot2)

data <-read.csv(file.choose(),header = T)   # open the file, file location can be given once you run the code
str(data)         # looking at the structure of data set
xtabs(~admit+rank,data=data)    #contingency table or cross tabulation between two categorical variables 
data$rank <-as.factor(data$rank)  # factor admit and rank variables 
data$admit <-as.factor(data$admit)
str(data)
#visualization 
pairs.default(data[-1])
par(mfrow=c(1,2))
boxplot(data$admit)
boxplot(data$gre)
library(ggplot2)
ggplot(data)
## Create box plots for ease of comparison
ggplot(data, aes(x=admit, y=gpa, fill=admit))+
geom_boxplot()+
ggtitle("Box Plot")
## create density grpahs 
ggplot(data, aes(x=gre,fill=admit))+
  geom_density(alpha=0.7)+
  ggtitle("desnity Plot")
##Data partition 
set.seed(1234)
ind <-sample(2,nrow(data),replace = T, prob = c(0.8,0.2)) ## create two samples 
train <-data[ind==1,] ## assign data to train data
test <-data[ind==2,]## assign data to test data
#Naive Bayes Model
model <-naive_bayes(admit~.,data = train)
model
plot(model) ## you will get three graphs 
p<-predict(model,train,type ='prob')
head(cbind(p,train)) # merge probability table of predictions with train data set
##Confusion Matrix - train data 
p1 <-predict(model,train)
(tab1<- table(p1,train$admit))
1-sum(diag(tab1))/ sum(tab1)
##Confusion Matrix - test data 
p2 <-predict(model,test)
(tab2<- table(p2,test$admit))
1-sum(diag(tab2))/ sum(tab2)











