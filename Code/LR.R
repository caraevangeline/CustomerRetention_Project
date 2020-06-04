training.data.raw <- read.csv('F://dwdm project novelty//bank1.csv',header=T,na.strings=c(""))
sapply(training.data.raw,function(x) sum(is.na(x)))
sapply(training.data.raw, function(x) length(unique(x)))
install.packages("Amelia")
library(Amelia)
missmap(training.data.raw, main = "Missing values vs observed")
data <- subset(training.data.raw,select=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21))
?factor
t<-sample(1:41188,35000)
e<-setdiff(1:41188,t)
is.factor(data$age)
is.factor(data$loan)
contrasts(data$loan)
train <- data[t,]
summary(train)
test <- data[e,]
test
?predict
?glm
class(train)
m<-glm.fit(train[,-21],test[,21])
sapply(data, function(x) length(levels(x)))
model <- glm(y ~.,family=binomial(link='logit'),data=train)
summary(model)
fitted.results <- predict(model,newdata=test[,-21],type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
data$month
misClasificError <- mean(fitted.results != test$Survived)
print(paste('Accuracy',1-misClasificError))
