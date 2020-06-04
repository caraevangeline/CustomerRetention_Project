#--------------------------------------
#LOGISTIC REGRESSION
#--------------------------------------
bank_full <- read.csv('F://dwdm project novelty//bank-full1.csv',header=T,na.strings=c(""))
head(bank_full) 
str(bank_full) 
summary(bank_full) 
is.na(bank_full) 
require(Amelia)
missmap(bank_full,main="Missing Data - Bank Subscription", col=c("red","grey"),legend=FALSE)
summary(bank_full)
library(caret)
set.seed(1234567)
train1<-createDataPartition(bank_full$y,p=0.88,list=FALSE)
train<-bank_full[train1,]
test<-bank_full[-train1,]
train.logistic=glm(y~age+job+marital+education+default+housing+balance+loan+contact+day+month+duration+campaign+pdays+previous+poutcome,binomial(link="logit"),train)
summary(train.logistic) 
test.prob<-predict.glm(train.logistic,test[,1:16],type="response")
test.prediction<-cut(test.prob,c(-Inf,0.5,Inf),labels=c("No","Yes"))
test.prediction
summary(test.prediction)
table(test.prediction,test$y)
confusionMatrix(table(test.prediction,test$y))
      #Accuracy  :   0.906


#------------------------------------------------------
#C5.0
#------------------------------------------------------
library(C50)
C5.0Control(subset=TRUE,bands=2,winnow=TRUE,noGlobalPruning=FALSE,CF=0.25,minCases=2,label="C5.0 Outcome")
train.c50=C5.0(train[,1:15],train$y,trials=10,rules=FALSE,control=C5.0Control(),costs=NULL)
C5imp(train.c50,metric="usage",pct=TRUE)
summary(train.c50) 
test.c50=predict(object=train.c50,newdata=test[,1:16],trials=1,type="class")
table(test.c50,test$y)
confusionMatrix(table(test.c50,test$y))
      #Accuracy  :   0.8969764


#-----------------------------------------------------
#Neural Network
#-----------------------------------------------------
install.packages("neuralnet")
library(neuralnet)
str(train)
str(test)
str(bank_full)
bank_full_transform<-bank_full
bank_full_transform$marital=factor(bank_full_transform$marital,levels=c("single","married","divorced"),labels=c(1,2,3))
bank_full_transform$job=factor(bank_full_transform$job,levels=c("admin","blue-collar","entrepreneur","housemaid","management","retired","self-employed","services","student","technician","unemployed","unknown"),labels=c(1,2,3,4,5,6,7,8,9,10,11,12))
bank_full_transform$education=factor(bank_full_transform$education,levels=c("primary","secondary","tertiary","unknown"),labels=c(1,2,3,4))
bank_full_transform$housing=factor(bank_full_transform$housing,levels=c("no","yes"),labels=c(1,2))
bank_full_transform$loan=factor(bank_full_transform$loan,levels=c("no","yes"),labels=c(1,2))
bank_full_transform$y=factor(bank_full_transform$y,levels=c("no","yes"),labels=c(1,2))
bank_full_transform$contact=factor(bank_full_transform$contact,levels=c("cellular","telephone","unknown"),labels=c(1,2,3))
bank_full_transform$poutcome=factor(bank_full_transform$poutcome,levels=c("failure","other","success","unknown"),labels=c(1,2,3,4))
bank_full_transform$month=factor(bank_full_transform$month,levels=c("apr","aug","dec","feb","jan","jul","jun","mar","may","nov","oct","sep"),labels=c(1,2,3,4,5,6,7,8,9,10,11,12))
str(bank_full_transform)
bank_full_transform$default=factor(bank_full_transform$default,levels=c("no","yes"),labels=c(1,2))


bank_full_transform$y<-as.numeric(as.character(bank_full_transform$y))
bank_full_transform$job<-as.numeric(as.character(bank_full_transform$job))
bank_full_transform$marital<-as.numeric(as.character(bank_full_transform$marital))
bank_full_transform$education<-as.numeric(as.character(bank_full_transform$education))
bank_full_transform$loan<-as.numeric(as.character(bank_full_transform$loan))
bank_full_transform$housing<-as.numeric(as.character(bank_full_transform$housing))
bank_full_transform$contact<-as.numeric(as.character(bank_full_transform$contact))
bank_full_transform$month<-as.numeric(as.character(bank_full_transform$month))
bank_full_transform$poutcome<-as.numeric(as.character(bank_full_transform$poutcome))
bank_full_transform$age<-as.numeric(as.character(bank_full_transform$age))
bank_full_transform$balance<-as.numeric(as.character(bank_full_transform$balance))
bank_full_transform$day<-as.numeric(as.character(bank_full_transform$day))
bank_full_transform$duration<-as.numeric(as.character(bank_full_transform$duration))
bank_full_transform$campaign<-as.numeric(as.character(bank_full_transform$campaign))
bank_full_transform$pdays<-as.numeric(as.character(bank_full_transform$pdays))
bank_full_transform$previous<-as.numeric(as.character(bank_full_transform$previous))
bank_full_transform$default<-as.numeric(as.character(bank_full_transform$default))
str(bank_full_transform)

library(caret)
set.seed(1234567)
train2<-createDataPartition(bank_full_transform$y,p=0.7,list=FALSE)
trainnew<-bank_full_transform[train2,]
testnew<-bank_full_transform[-train2,]
str(trainnew)
str(testnew)
trainnew.nnbp<-neuralnet(y~age+balance+day+duration+campaign+pdays+previous+marital+education+housing+loan+month+poutcome+contact,data=bank_full_transform,hidden=5,threshold=0.01,err.fct="sse",linear.output=FALSE,likelihood=TRUE,stepmax=1e+05,rep=1,startweights=NULL,learningrate.limit=list(0.1,1.5),learningrate.factor=list(minus=0.5,plus=1.5),learningrate=0.5,lifesign="minimal",lifesign.step=1000,algorithm="backprop",act.fct="logistic",exclude=NULL,constant.weights=NULL)


summary(trainnew.nnbp)
gwplot(trainnew.nnbp,selected.covariate="balance")
plot(trainnew.nnbp,rep="best")
prediction(trainnew.nnbp)
print(trainnew.nnbp)
columns=c("age","job","marital","education","balance","housing","loan","contact","day","month","duration","campaign","pdays","poutcome")
testnew2<-subset(testnew,select=columns)
testnew.nnbp<-compute(trainnew.nnbp,testnew2,rep=1)
table(testnew$y,testnew.nnbp$net.result)
cbind(testnew$y,testnew.nnbp$net.result)
print(testnew.nnbp) 
