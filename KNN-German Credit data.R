#Load the dataset on the "loan" variable
loan <- read.csv("credit_data.csv")
head(loan)

str(loan)

#Clean data
#in order to predict creditability we don't need all the data ,selecting only the ones that would be needed.
loan.subset <- loan[c('Creditability','Age..years.','Sex...Marital.Status','Occupation','Account.Balance','Credit.Amount','Length.of.current.employment','Purpose')]

head(loan.subset) #preview new subset of data

#Normalization
#some number are one digit and others three digits, normalize for better differentiation
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

#since we are going to predict 1 st column : credibility, removing it and creating a data frame 
loan.subset.n <- as.data.frame(lapply(loan.subset[,2:8], normalize))
head(loan.subset.n) #all the values are now normalised & comparable 

#Now we create testing & training dataset
set.seed(123)# this will get us a random data sample otherwise will get different samples each time we run code and it will confuse results 
#and then we randomly select 70% of our data for training 
dat.d <- sample(1:nrow(loan.subset.n),size=nrow(loan.subset.n)*0.7,replace = FALSE) #random selection of 70% data.
train.loan <- loan.subset[dat.d,] # 70% of data is to train the model 
test.loan <- loan.subset[-dat.d,] # remaining 30% test data

#creating a different data set for the target variable 
train.loan_labels <- loan.subset[dat.d,1]# values of the first column will be assigned 
test.loan_labels <-loan.subset[-dat.d,1]

#Building a Machine Learning model
#now to train the model we have to install a package
#Install class package which contains KNN algorithm
install.packages('class')
# Load class package
library(class)

#Find the number of observation (to find the optimal K value is to calculate the square root of the total number of observations in the data set)
NROW(train.loan_labels) 

#Now we have to initate the value of K.to identify the optimum value we find the sqrt
#of total no. observations, one good practise is to get squaroot value of number of data point as the K value 
sqrt(NROW(train.loan_labels))

#will start with K as 26 which is nearest integer of above taken squar root value .
knn.26 <- knn(train=train.loan, test=test.loan, cl=train.loan_labels, k=26)
knn.27 <- knn(train=train.loan, test=test.loan, cl=train.loan_labels, k=27)
#sdepending on the K value accurace level changes and need to find optimal K vlaue for the model 

#Model Evaluation
#Calculate the proportion of correct classification for k = 26, 27
ACC.26 <- 100 * sum(test.loan_labels == knn.26)/NROW(test.loan_labels)
ACC.27 <- 100 * sum(test.loan_labels == knn.27)/NROW(test.loan_labels)

ACC.26 #accuracy 68.66%
ACC.27 #Accuracy is 69% which has reduced compared to k=26. Accuracy value is higher

# Check prediction against actual value in tabular form for k=26
table(knn.26 ,test.loan_labels)
#Table states that out of 17 observations, for 8 observations we were correctly predicted and 7 were incorrect 
# out of 295 observation 87 were wrong and 198 were correct. here stating bad creditors as credible seen at only 7 instances
# which is a fair low error rate for 286 observations. but since that error has biggest impact need to reduce it more
# stating good creditor as bad hs low impact compared to bad creditor as good

knn.26 #view 

# Check prediction against actual value in tabular form for k=27
table(knn.27 ,test.loan_labels)
knn.27




#Another way to calculate the accuracy
#Use the confusion matrix to calculate the accuracy
install.packages('caret')
library(caret)

confusionMatrix(table(knn.26 ,test.loan_labels))
confusionMatrix(table(knn.27 ,test.loan_labels))

#Optimization
#calculates the accuracy of the KNN model for 'K' values ranging from 1 to 28
i=1
k.optm=1
for (i in 1:28){
  knn.mod <- knn(train=train.loan, test=test.loan, cl=train.loan_labels, k=i)
  k.optm[i] <- 100 * sum(test.loan_labels == knn.mod)/NROW(test.loan_labels)
  k=i
  cat(k,'=',k.optm[i],'')
}

#Accuracy plot
plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level")

#For 'K' value of 25 we get the maximum accuracy
knn.25 <- knn(train=train.loan, test=test.loan, cl=train.loan_labels, k=25)
