#Nate Kaduk
#Data Visualization HW 12


suburbData<-read.csv("C:/Users/natek/Downloads/suburbs_ranking_2016.csv")
head(suburbData)

smallerData<- suburbData[c("SidewalkPct", "PropTax","Pov","Police","CommServ", "Top20")]
head(smallerData)

#Part 1: 
library(vtable)
testData<-summary(smallerData)
st(smallerData)
library(ggplot2)

ggplot(smallerData, aes(smallerData$SidewalkPct))+geom_histogram(fill="red")

hist(smallerData$SidewalkPct, xlab = "Percentage of Roads with Sidewalks", main="Suburb Sidewalk Percentage", c="lightblue")
breaksPT<-seq(0, 4, by=0.5)
hist(smallerData$PropTax, breaks=breaksPT, xlab = "Average Annual Property Taxes (Thousands of dollars)", main="Suburb Annual Property Taxes", c="orange",xaxt="n")
axis(side=1, at =seq(0,4, by=0.5))


hist(suburbData$Pov, breaks = 5, xlab = "Poverty Rate (Percentage of Total Suburb Population)", main="Suburb Poverty Rates", c="red")

hist(suburbData$Police, xlab = "Number of Police in Suburb (per 1000 people)", main="Suburb Police Numbers", c="pink",xaxt="n")
axis(side=1, at =seq(0,24, by=2))

breaksCS <- seq(0, 0.016, by = 0.002)
hist(smallerData$CommServ, breaks=breaksCS, xlab = "Community Services (per 1000 people)", main="Suburb Community Services", c="darkgreen",xaxt="n")
axis(side=1, at =seq(0,0.016,0.002))

#Part 2
library(doBy)
NumberOfObservations <- function(x)return(length(x))

summaryBy(SidewalkPct+PropTax+StuTeachRatio+ACT+Attend~Top20, FUN=c(mean,sd, NumberOfObservations), data=suburbData)

st(smallerData,group='Top20')

smallerData1 <- smallerData[smallerData['Top20']==1,]
smallerData0 <- smallerData[smallerData['Top20']==0,]

#Histograms for top 20 schools
hist(smallerData1$SidewalkPct, xlab = "Percentage of Roads with Sidewalks", main="Suburb Sidewalk Percentage (Top 20 Suburbs)", c="lightblue")

breaksPT<-seq(0, 4, by=0.5)
hist(smallerData1$PropTax, breaks=breaksPT, xlab = "Average Annual Property Taxes (Thousands of dollars)", main="Suburb Annual Property Taxes (Top 20 Suburbs)", c="orange",xaxt="n")
axis(side=1, at =seq(0,4, by=0.5))

breaksPov <- seq(0, 10, by=1)
hist(smallerData1$Pov, breaks = breaksPov, xlab = "Poverty Rate (Percentage of Total Suburb Population)", main="Suburb Poverty Rates (Top 20 Suburbs)", c="red", xaxt="n")
axis(side=1, at=seq(0,10,by=1))

breaksPolice <- seq(0,6, by=1)
hist(smallerData1$Police, breaks=breaksPolice, xlab = "Number of Police in Suburb (per 1000 people)", main="Suburb Police Numbers (Top 20 Suburbs)", c="pink",xaxt="n")
axis(side=1, at =seq(0,6, by=1))

breaksCS <- seq(0, 0.016, by = 0.002)
hist(smallerData1$CommServ, breaks=breaksCS, xlab = "Community Services (per 1000 people)", main="Suburb Community Services (Top 20 Suburbs)", c="darkgreen",xaxt="n")
axis(side=1, at =seq(0,0.016,0.002))


#Histograms for non-top 20 schools

hist(smallerData0$SidewalkPct, xlab = "Percentage of Roads with Sidewalks", main="Suburb Sidewalk Percentage (Non-Top 20 Suburbs)", c="lightblue")

breaksPT<-seq(0, 4, by=0.5)
hist(smallerData0$PropTax, breaks=breaksPT, xlab = "Average Annual Property Taxes (Thousands of dollars)", main="Suburb Annual Property Taxes (Non-Top 20 Suburbs)", c="orange",xaxt="n")
axis(side=1, at =seq(0,4, by=0.5))


hist(smallerData0$Pov, breaks = 5, xlab = "Poverty Rate (Percentage of Total Suburb Population)", main="Suburb Poverty Rates (Non-Top 20 Suburbs)", c="red")

breaksPolice <- seq(0, 24, by=6)
hist(smallerData0$Police, breaks=breaksPolice,xlab = "Number of Police in Suburb (per 1000 people)", main="Suburb Police Numbers (Non-Top 20 Suburbs)", c="pink",xaxt="n")
axis(side=1, at =seq(0,24, by=6))

breaksCS <- seq(0, 0.016, by = 0.002)
hist(smallerData0$CommServ, breaks=breaksCS, xlab = "Community Services (per 1000 people)", main="Suburb Community Services (Non-Top 20 Suburbs)", c="darkgreen",xaxt="n")
axis(side=1, at =seq(0,0.016,0.002))


#Part 3
#a
library(party)

excludingData <- suburbData[,-match(c("OverallRank", "SchoolRank","SafetyRank","community", "suburbid"), names(suburbData))]
head(excludingData)

model.ctree <- ctree(as.factor(Top20)~., data=excludingData)
print(model.ctree)
plot(model.ctree)

#b

library(randomForest)
modelRF <- randomForest(as.factor(Top20)~., data = excludingData, importance=T, ntree=)
importance(modelRF)
varImpPlot(modelRF)
#c

part3Data = excludingData[,-match(c("MedSaleVal", "Perform"),names(excludingData))]
part3Model <- ctree(as.factor(Top20)~., data=part3Data)

print(part3Model)
plot(part3Model)

attach(suburbData)
subTable <- table(suburbData$Top20, suburbData$StateStand)
subTable

library(psych)
describe(c(suburbData$ACT,suburbData$Top20))

#Part 4
clusterableData <- suburbData[,7:24]
head(clusterableData)
set.seed(1234)

#Cluster data
clusterKMeans <- kmeans(clusterableData, 2)
#Find number of suburbs in each group
length(clusterKMeans$cluster[clusterKMeans$cluster==1])
length(clusterKMeans$cluster[clusterKMeans$cluster==2])

suburbData$twoclusters<- clusterKMeans$cluster
clusterableData <- suburbData[,7:25]


st(clusterableData, group="twoclusters")

library(ggplot2)


ggplot(suburbData, aes(x=Perform, y=MedSaleVal)) +geom_point(aes(color=as.factor(twoclusters))) #+geom_mark_ellipse(data = suburbData, aes(color = as.factor(twoclusters)))

kMeans5 <- kmeans(clusterableData, 5)
suburbData$fiveclusters <- kMeans5$cluster
clusterableData <- suburbData[,7:26]

st(clusterableData[,-match(c('twoclusters'), names(clusterableData))], group="fiveclusters")

ggplot(suburbData, aes(x=Perform, y=MedSaleVal, color=as.factor(fiveclusters))) +geom_point()

