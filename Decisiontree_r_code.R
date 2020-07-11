data("iris")
View(iris)
install.packages("C50")
library(C50)
install.packages("caret")
library(caret)
# Data partion for model building and testing
inTraininglocal <- createDataPartition(iris$Species,p=.75, list=F)
training <- iris[inTraininglocal,]
View(training)
testing <- iris[-inTraininglocal,]
View(testing)
#model building
model <- C5.0(training$Species~.,data = training,trails = 1000)
?C5.0
# Generating the model summary
testing[,-5]
summary(model)
pred <- predict.C5.0(model,training[,-5])
pred
pred1 <- predict.C5.0(model,testing[,-5])
pred1
a <- table(training$Species,pred)
b <- table(testing$Species,pred1)
a
b
sum(diag(b)/sum(b))
plot(model)

###Bagging####
acc<-c()
for(i in 1:100)
{
  print(i)
  inTraininglocal<-createDataPartition(iris$Species,p=.85,list=F)
  training1<-iris[inTraininglocal,]
  testing<-iris[-inTraininglocal,]

  fittree<-C5.0(training1$Species~.,data=training1)
  pred<-predict.C5.0(fittree,testing[,-5])
  a<-table(testing$Species,pred)
  a
  acc<-c(acc,sum(diag(a))/sum(a))
  acc

}
summary(acc)
acc
