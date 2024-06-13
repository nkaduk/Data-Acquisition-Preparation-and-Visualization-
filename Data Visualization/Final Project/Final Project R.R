
finalData <- read.csv("C:/Users/natek/Downloads/clean (7).csv")
head(finalData)

#Data for random forest 
forestData <- finalData[c('COUNTYFIP', 'AGE', 'INCOME','RACE','SEX','EDUCATION')]

library(randomForest)
modelRF <- randomForest(as.factor(COUNTYFIP)~., data = forestData, importance=T, ntree=)
importance(modelRF)
varImpPlot(modelRF, main = "Importance in Determining County")


blackLoc <- finalData$RACE=='Black'
otherLoc <- finalData$RACE=='Other'
whiteLoc<- finalData$RACE=='White'

finalData$RACENUM[whiteLoc] <- 1
finalData$RACENUM[blackLoc] <- 3
finalData$RACENUM[otherLoc] <- 2

lessHSLOC<-finalData$EDUCATION=='Less Than High School'
HSLOC <- finalData$EDUCATION=='High School'
BALOC <- finalData$EDUCATION=='Bachelor\'s Degree'
MALOC <- finalData$EDUCATION=="Master\'s and Above"


finalData$ENUM[lessHSLOC]<-1
finalData$ENUM[HSLOC]<-2
finalData$ENUM[BALOC]<-3
finalData$ENUM[MALOC]<-4

femaleLoc <- finalData$SEX =="Female"
maleLoc <- finalData$SEX =="Male"

finalData$SEXNUM[femaleLoc] = 1
finalData$SEXNUM[maleLoc] = 2

library(party)
forestData <- finalData[c('COUNTYFIP', 'SEXNUM','ENUM', 'AGE', 'INCOME','RACE','SEX','EDUCATION', 'RACENUM')]
forestDataAB <- forestData[forestData$COUNTYFIP== 15| forestData$COUNTYFIP ==17,]

 
model.ctree <- ctree(as.factor(COUNTYFIP)~., data=forestDataAB[,-match(c('RACE','EDUCATION', 'SEX'), names(forestData))])
print(model.ctree)
plot(model.ctree)


forestDataAC <- forestData[forestData$COUNTYFIP== 3| forestData$COUNTYFIP ==5,]


model.ctree <- ctree(as.factor(COUNTYFIP)~., data=forestDataAC[,-match(c('RACE','EDUCATION', 'SEX'), names(forestData))])
print(model.ctree)
plot(model.ctree)


