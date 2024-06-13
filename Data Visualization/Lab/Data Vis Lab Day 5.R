#Nate Kaduk
#Data Visualization

#Lab 8: Clustering college data

library(readxl)


clusterData <- read.csv("C:/Users/natek/Downloads/ipeds2014.csv")
cleanedData<-na.omit(clusterData) #Clean data by removing system misisng
head(cleanedData)

#Get summary of cleaned data
summary(cleanedData)

#Find clusters in the data based upon different numbers of clusters.
clust3 <- kmeans(cleanedData[,c("gradrate4yr","ugradenroll", "admitrate")], 3)
clust4<- kmeans(cleanedData[,c("gradrate4yr","ugradenroll", "admitrate")], 4)
clust5<- kmeans(cleanedData[,c("gradrate4yr","ugradenroll", "admitrate")], 5)

#See what is provided for a cluster
print(clust3)

#Add clusters to the cleaned data
cleanedData$cluster3 <- clust3$cluster
cleanedData$cluster4 <- clust4$cluster
cleanedData$cluster5 <- clust5$cluster

#Output the explained variance of clusters 
clust3$betweenss/clust3$totss #86.5%
clust4$betweenss/clust4$totss #92.8%
clust5$betweenss/clust5$totss #95.0%

library(cluster)
library(factoextra)
library(gridExtra)
#First clustering method
clusplot(cleanedData, clust3$cluster, color=T, labels=4)
clusplot(cleanedData, clust4$cluster, color=T, labels=4)
clusplot(cleanedData, clust5$cluster, color=T, labels=4)



p3 <- fviz_cluster(clust3, geom="point", data=clusNum)+ggtitle("fit=1")
p4 <- fviz_cluster(clust4, geom="point", data=clusNum)+ggtitle("fit=2")
p5 <- fviz_cluster(clust5, geom="point", data=clusNum)+ggtitle("fit=3")

grid.arrange(p3, p4, p5, nrow=2)


#Remove the string variables for institution name and state abbreviation
clusNum<- cleanedData[,-match(c("instnm", "stabbr"), names(cleanedData))]
head(clusNum)

nrow(clusNum)
#1 Applys the function over rows/observations
#2 Applys function over columns/attributes
sum(apply(clusNum,2,var))

#Calculate within sum of squares for each numeric variable.
wss = (nrow(clusNum)-1)*sum(apply(clusNum, 2, var))
wss

#Use kmeans to find within sum of squares for a 3-k clustering scheme
kRes = kmeans(clusNum, centers=3)
head(kRes)
kRes$withinss

#Calculate several within sum of squares to make an elbow plot.
#Finding sum off all of the wss for each cluster
for(i in 2:18) wss[i]<-sum(kmeans(clusNum, centers=i)$withinss)
wss
plot(1:18, wss, type="b", xlab="Number of Clusters", ylab="Within sum of squares")
#for (i in 2:18) wss[i]

#As five clusters is where the within sum of squares start to flat-line the most, the clustering process
#is performed in five clusters.
clustTest<-kmeans(clusNum,5)
clustTest
