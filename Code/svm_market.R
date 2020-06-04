install.packages("e1071")
library(e1071)
library(caret)
data<-read.csv("F://dwdm project novelty//bank-full1.csv",header=TRUE)
head(data)

t<-sample(1:45211,40000)
r<-setdiff(1:45211,t)
train<-data[t,c(1,2,3,4,5,6,7,8,9,10,11,12,13,16,17)]
head(train)
test<-data[r,c(1,2,3,4,5,6,7,8,9,10,11,12,13,16)]
w<-as.factor(train_label<-data[t,17])
x<-as.factor(test_label<-data[y,17])

train.svm<-svm(y~.,train,kernel="polynomial",cost=0.01,scale=TRUE,degree=3,gamma=1)
summary(train.svm)
#plot(train.svm,train)
test.svm<-predict(train.svm,test)
confusionMatrix(table(predict=test.svm,truth=data[r,17]))
#Acurracy : 0.99
