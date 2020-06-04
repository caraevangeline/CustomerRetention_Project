install.packages("naivebayes")
library(naivebayes)
library(caret)

data<-read.csv("F://dwdm project novelty//bank-full1.csv",header=TRUE)

head(data)
class(data)
data
sapply(data,function(x) sum(is.na(x)))
sapply(data, function(x) length(unique(x)))

t<-sample(1:45211,40000)
y<-setdiff(1:45211,t)
train<-data[t,1:17]
head(train)
test<-data[y,-17]
w<-as.factor(train_label<-data[t,17])
x<-as.factor(test_label<-data[y,17])
model<-naive_bayes(train,w)
p<-predict(model,newdata=test,type="class")
confusionMatrix(table(p,test_label))
??confusionMatrix
#Accuracy : 0.8718