setwd("D:/tasks/462dataming/Assignment3")
library(ISLR)
library(MASS)
library(tree)
library(randomForest)

TestData <- read.csv("carseatsTest.csv")
TrainData  <- read.csv("carseatsTrain.csv")
str(TrainData)

#a)
# Grow the regression tree
tree.Carseats=tree(Sales~.,TrainData)
summary(tree.Carseats)

#plot thte tree
plot(tree.Carseats)
text(tree.Carseats,pretty=0)

#predict
yhat.train = predict(tree.Carseats,newdata=TrainData)
TrainingError = mean((yhat.train-TrainData$Sales)^2)

 
yhat.test = predict(tree.Carseats,newdata=TestData)
TestingError =mean((yhat.test-TestData$Sales)^2)

TrainingError
TestingError
 

#b)  
set.seed(1)
cv.carseats=cv.tree(tree.Carseats)
plot(cv.carseats$size,cv.carseats$dev,type="b")


# Prune the tree
prune.Carseats=prune.tree(tree.Carseats,best=17)   
plot(prune.Carseats)
text(prune.Carseats,pretty=0)

# Estimate the error of the tree
yhat1.train = predict(prune.Carseats,newdata=TrainData)
TrainingError1 = mean((yhat1.train-TrainData$Sales)^2)

yhat1.test = predict(prune.Carseats,newdata=TestData)
TestingError1 =mean((yhat1.test-TestData$Sales)^2)

TrainingError1
TestingError1



#c)
set.seed(1)
str(TrainData)
# Generate bagged trees  that bagging is simply a special case of a random forest with m = p.
bag.Carseats=randomForest(Sales~.,data=TrainData,mtry=9,importance=TRUE)

yhat.bag.train = predict(bag.Carseats,newdata=TrainData)
TrainingError_bag = mean((yhat.bag.train-TrainData$Sales)^2)

yhat.bag.test = predict(bag.Carseats,newdata=TestData)
TestError_bag = mean((yhat.bag.test-TestData$Sales)^2)

TrainingError_bag
TestError_bag

set.seed(1)
# Try a random forest p=9 regeression m=p/3=3
rf.Carseats=randomForest(Sales~.,data=TrainData, mtry=3,importance=TRUE)
yhat.rf.train = predict(rf.Carseats,newdata=TrainData)
TrainingError_rf = mean((yhat.rf.train-TrainData$Sales)^2)

yhat.rf.test = predict(rf.Carseats,newdata=TestData)
TestError_rf = mean((yhat.rf.test-TestData$Sales)^2)

TrainingError_rf
TestError_rf


#d)
#n.trees:
#Integer specifying the total number of trees to fit. This is equivalent to the number of iterations and
#the number of basis functions in the additive expansion. Default is 100.

#interaction.depth	
# Integer specifying the maximum depth of each tree (i.e., the highest level of variable interactions allowed). 
# A value of 1 implies an additive model, a value of 2 implies a model with up to 2-way interactions, etc.
# Default is 1.

# shrinkage	
# a shrinkage parameter applied to each tree in the expansion. 
# Also known as the learning rate or step-size reduction; 0.001 to 0.1 usually work, 
# but a smaller learning rate typically requires more trees. Default is 0.1.
library(gbm)
library(tidyverse)
result=tibble(B=0, D=0, Lamda=0, TrainingError=0, TestError=0)
set.seed(1)
B=seq(1000,10000, by=1000)
D=c(1,2,3,4)
Lamda=c(0.001, 0.01,0.1, 0.2)
for( b in B) {
  for(d in D){
    for(a in Lamda){
      
      boost.Carseats=gbm(Sales~.,data=TrainData,distribution="gaussian",n.trees=b,
                         interaction.depth=d,shrinkage =a)
      yhat.boost_train=predict(boost.Carseats,newdata=TrainData,n.trees=b,
                               interaction.depth=d,shrinkage =a)
      TrainError_boost= mean((yhat.boost_train-TrainData$Sales)^2)
      yhat.boost_test=predict(boost.Carseats,newdata=TestData,n.trees=b,
                         interaction.depth=d,shrinkage =a)
      TestError_boost= mean((yhat.boost_test-TestData$Sales)^2)
      result<-add_row(result, B = b, D = d, Lamda=a, TrainingError=TrainError_boost,TestError=TestError_boost)
     
      }
    }
}

 
boost.Carseats=gbm(Sales~.,data=TrainData,distribution="gaussian",n.trees=9000,
                   interaction.depth=2,shrinkage =0.001)
summary(boost.Carseats)
 



