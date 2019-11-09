setwd("C:/Users/asus/Desktop/tasks/462dataming/XiaocuiZhang")
library(data.table)
library(readr)
library(dplyr)
TrainData<- read.csv("AutoTrain.csv")
TestData<- read.csv("AutoTest.csv")
names(TrainData)
#predict Y = mpg using X = weight.
Y_train=TrainData$mpg
X_train=TrainData$weight
x_pred=TestData$weight
Y_true=TestData$mpg

#testif Y_train/X_train/x_pred/Y contains  na or null...  
#if return  integer(0) ,means data ok  
#which(is.na(Y_train))  #which(is.nan(Y_train))      
#which(Y_train=="NULL")#which(Y_train=="null")

k = c(2,5,10,20,30,50,100) 
MSE_train = numeric(length(k))
MSE_test = numeric(length(k))
#kNN <- function(k,x.train,y.train,x.pred) 
for(i in 1:length(k)) {
  haty_P= kNN(k[i], X_train,Y_train,x_pred)      # TEST predicted values
  haty_T = kNN(k[i], X_train,Y_train,X_train)    #TRAIN predicted values
  MSE_train[i]=mean((Y_train-haty_T)^2)
  MSE_test[i]=mean((Y_true - haty_P)^2)
}

SummaryTable=data.table(K=k, MSE_train=MSE_train,MSE_test=MSE_test)  

kbest=SummaryTable%>%filter(MSE_test==min(MSE_test))%>%select(K) 
KNN_model=kNN(kbest[[1]],X_train,Y_train,c(X_train,x_pred))

ymax=max(Y_train,Y_true)
ymin=min(Y_train,Y_true) 
xmax=max(X_train,x_pred)
xmin=min(X_train,x_pred) 
 
#kNN model and all the data ---regression line
tt <- data.frame( X=c(X_train,x_pred), Y=KNN_model) 
KNNModelData<-tt[order(tt[,1]),]  
plot(KNNModelData$X,KNNModelData$Y, data=KNNModelData,
     pch=15, col="black", cex=0.5,lwd = 2,lty=1, type="p",
     ylim=c(ymin-1,ymax+1),xlim=c(xmin-1,xmax+1),
     xlab = "Weight",ylab ="MPG", main = "The relationship between weight and MPG")
points(X_train, Y_train,lwd =1,col = "blue", type="p", pch=1)
points(x_pred, Y_true, lwd =1, col = "#CD2626",type="p",pch=1)
legend("topright",
       legend = c("KNN_model","Train Data","Test Data"),
       col = c("black","blue","#CD2626"),
       pch=c(15,1,1),
       text.col = "black",
       horiz = FALSE)
 
plot(1/k,MSE_train, 
     col="black", lwd = 2,lty=2, type="l",xlab = "1/K",ylab ="MSE",main = "relationship between 1/K and MSE")
points(1/k, MSE_test,lwd =2,lty=2,col = "blue", type="l")
points(c(1/30,1/30),c(1,20),lwd =1,col = "red", type="l")
legend("topright",
       legend = c("MSE_train","MSE_test","k=30"),
       col = c("black","blue","red"),
       pch=c("-","-","-"),
       text.col = "black",
       horiz = FALSE)
 
 
