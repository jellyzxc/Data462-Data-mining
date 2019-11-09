library(cluster)

setwd("D:/tasks/462dataming/Assignment3")
clusterData <- read.csv("A3data2.csv")
str(clusterData)

plot(clusterData[,1:2], col=(clusterData[,3]),
     main="ture clusters", pch=20, cex=1)


set.seed(1)
#K-Means Clustering   Cluster the data (k=3)
km.out=kmeans(clusterData[,1:2],3,nstart=50)
km.out$cluster
plot(clusterData[,1:2], col=(km.out$cluster+1),
     main="K-Means Clustering Results with K=3", pch=20, cex=1)


#Hierarchically cluster the data using the 2 linkage methods

distance=dist(clusterData[,1:2], method = "euclidean", diag = TRUE, upper = TRUE)



hc.complete=hclust(distance, method="complete")
plot(hc.complete,main="Complete Linkage", xlab="", sub="", labels=FALSE)
plot(clusterData[,1:2], col=cutree(hc.complete, 3),
     main="Hierarchical Clustering Results",
     sub="with complete linkage and Euclidean distance",pch=20, cex=1)


hc.single=hclust(distance, method="single")
plot(hc.single, main="Single Linkage", xlab="", sub="", labels=FALSE)
plot(clusterData[,1:2], col=cutree(hc.single, 3),
     main="Hierarchical Clustering Results",
     sub="with Single linkage and Euclidean distance",pch=20, cex=1)

par(mfrow=c(2,2))
plot(clusterData[,1:2], col=(clusterData[,3]),
     main="True clusters", pch=20, cex=1)
plot(clusterData[,1:2], col=(km.out$cluster+1),
     main="K-Means Clustering Results with K=3", pch=20, cex=1)
plot(clusterData[,1:2], col=cutree(hc.complete, 3),
     main="Hierarchical Clustering Results--complete linkage",pch=20, cex=1)
plot(clusterData[,1:2], col=cutree(hc.single, 3),
     main="Hierarchical Clustering Results-Single linkage ",pch=20, cex=1)


#accuracy
 
#knn
table(km.out$cluster,clusterData[,3])
#hc.complete
table(cutree(hc.complete,3),clusterData[,3])
#hc.single
table(cutree(hc.single, 3),clusterData[,3])



#############################
#Rescale the  data 
scaledData=scale(clusterData[,1:2], center=T,scale=T)
#K-Means Clustering   Cluster the data (k=3)
set.seed(1)
km1.out=kmeans(scaledData,3,nstart=50)
plot(scaledData, col=(km1.out$cluster+1),
     main="K-Means Clustering Results with K=3", pch=20, cex=1)

#Hierarchically cluster the data using the 2 linkage methods

distance1=dist(scaledData, method = "euclidean", diag = TRUE, upper = TRUE)

hc1.complete=hclust(distance1, method="complete")
plot(hc1.complete,main="Complete Linkage", xlab="", sub="", labels=FALSE)
plot(scaledData, col=cutree(hc1.complete, 3),
     main="Hierarchical Clustering Results",
     sub="with complete linkage and Euclidean distance",pch=20, cex=1)


hc1.single=hclust(distance1, method="single")
plot(hc1.single, main="Single Linkage", xlab="", sub="", labels=FALSE)
plot(scaledData, col=cutree(hc1.single, 3),
     main="Hierarchical Clustering Results",
     sub="with Single linkage and Euclidean distance",pch=20, cex=1)


dev.off()
par(mfrow=c(2,2))
plot(scaledData, col=(clusterData[,3]),
     main="True clusters", pch=20, cex=1)
plot(scaledData, col=(km1.out$cluster+1),
     main="K-Means Clustering Results with K=3", pch=20, cex=1)
plot(scaledData, col=cutree(hc1.complete, 3),
     main="Hierarchical Clustering Results--complete linkage",pch=20, cex=1)
plot(scaledData, col=cutree(hc1.single, 3),
     main="Hierarchical Clustering Results-Single linkage ",pch=20, cex=1)


#accuracy
#knn
table(km1.out$cluster,clusterData[,3])
#hc.complete
table(cutree(hc1.complete,3),clusterData[,3])
#hc.single
table(cutree(hc1.single, 3),clusterData[,3])
nrow(scaledData)



par(mfrow=c(2,2))
plot(clusterData[,1:2], col=(clusterData[,3]),
     main="True clusters", pch=20, cex=1)
plot(clusterData[,1:2], col=(km1.out$cluster+1),
     main="K-Means Clustering Results with K=3", pch=20, cex=1)
plot(clusterData[,1:2], col=cutree(hc1.complete, 3),
     main="Hierarchical Clustering Results--complete linkage",pch=20, cex=1)
plot(clusterData[,1:2], col=cutree(hc1.single, 3),
     main="Hierarchical Clustering Results-Single linkage ",pch=20, cex=1)


 
