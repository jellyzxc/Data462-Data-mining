
setwd("D:/tasks/462dataming/Assignment3")
library(tidyverse)
library(readr)  
library(dplyr)
trainningData <- read.csv("BankTrain.csv",header = TRUE )%>%select(y,x1,x3)
testingData <- read.csv("BankTest.csv",header = TRUE )%>%select(y,x1,x3)
glimpse(trainningData) 
#convert y to facotor 
trainningData$y=as.factor(trainningData$y)
testingData$y=as.factor(testingData$y)

trainningData<-trainningData%>%mutate(ForgedOrNot= case_when(y == 1 ~ "Forged",y == 0 ~ "Genuine"))

ggplot(trainningData) +
  geom_point( aes(x=x3, y=x1,colour=ForgedOrNot),size=3)+
  xlab ("x3") + 
  ylab("x1") +
  ggtitle("training data of banknotes divided into  two classes:forged and genuine ")+
  theme(plot.title = element_text(hjust = 0.5)) 


##################----support vector classifier
library (e1071) 
#By default, tune() performs ten-fold cross-validation on a set of models of interest
set.seed(1)
tune.out=tune(svm,y~x1+x3,data=trainningData,kernel ="linear",
              ranges =list(cost=c(0.001,0.01,0.1,1,10,100,1000)))
summary(tune.out)

bestm=tune.out$best.model
summary(bestm)

plot(bestm,trainningData[,-4])  #remove the mutated column:ForgedOrNot
 

ypred=predict(bestm,testingData)
table(predict=ypred,truth=testingData$y)

##############----Svm
set.seed(1)
tune1.out = tune(svm,
                y~x1+x3,
                data=trainningData, kernel="radial",
                ranges=list(cost=c(0.001,0.01,0.1,1,10,100,1000),
                            gamma=c(0.5,1,2,3,4)))
summary(tune1.out)
bestmodel1 = tune1.out$best.model
summary(bestmodel1)

plot(bestmodel1,trainningData[,-4]) #remove the mutated column:ForgedOrNot


ypred1 = predict(bestmodel1,testingData)
table(predict=ypred1,truth=testingData$y)

