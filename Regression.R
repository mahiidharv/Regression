rm(list=ls(all=TRUE))
##################### Create a directory##################

setwd("G:\\insofe\\20161126_Batch23_CSE7202c_Regression\\Lab Session")
data<-read.csv(file = "CustomerData.csv",header = T,sep = ",")
str(data)
data$CustomerID<-NULL
data$City<-as.factor(x = data$City)
require(corrplot)
data_numeric=subset(x = data,select = -c(FavoriteChannelOfTransaction,FavoriteGame,City))
cor(data_numeric)
corrplot(cor(data_numeric),method = 'number')
corrplot(cor(data_numeric),method = 'ellipse')
rows=seq(1,nrow(data_numeric),1)
set.seed(123)
trainRows=sample(rows,(70*nrow(data_numeric))/100)
train = data_numeric[trainRows,]
test = data_numeric[-trainRows,]
Customer_model1<-lm(train$TotalRevenueGenerated~.,data=train)
plot(Customer_model1)
summary(Customer_model1)
predicted_train<-predict(Customer_model1,train)
require(DMwR)
regr.eval(trues = train$TotalRevenueGenerated,preds =predicted_train)
testpred<-predict(Customer_model1,test)
regr.eval(trues=test$TotalRevenueGenerated,preds = testpred)

