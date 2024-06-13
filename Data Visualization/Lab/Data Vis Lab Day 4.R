#Nate Kaduk
#Data Visualization


#Lab 7: Statistic analysis, specifically decision tree, on titanic data
library(readxl)

filename <-"C:/Users/natek/Downloads/titanic3.csv"

titanic <- read.csv(filename)
head(titanic)
colnames(titanic)

primaryVar <- c("age", "sibsp", "pclass", "sex", "parch","survived")
titanic <- na.omit(titanic[,primaryVar])
head(titanic)

#titanic$female<-revalue(titanic$sex, replace = c(male=0, female=1))
#Map character values in the variable sex to numeric values in the new variable female.
library(plyr)
titanic$female <- mapvalues(titanic$sex, from=c("male","female"), to=c(0,1))

titanic$female <- ifelse(titanic$female=="1",1, ifelse(titanic$female == "0", 0, NA))

typeof(titanic["female"][,1][0])

set.seed(1234)

library(plyr)


head(titanic)

titanic$randu <- runif(nrow(titanic),0,1) #Create uniform variable for each row
titanic.train <- titanic[titanic$randu<0.7,]
titanic.test <- titanic[titanic$randu>=0.7,]

model.OLS <- lm(survived ~age + sibsp+pclass+female+randu, data=titanic.train)
summary(model.OLS)

pred.OLS <- predict(model.OLS, type="response")
summary(pred.OLS)

#Predict using decision trees
library(party)


model.ctree <- ctree(as.factor(survived)~age+sibsp+pclass+female+randu, data=titanic.train)
controls = ctree_control(minbucket = 1)

print(model.ctree)
plot(model.ctree)

pred.ctree <- predict(model.ctree, newdata=titanic.test, type="response")
survivalTable <- table(pred.ctree, titanic.test$survived)
survivalTable
prop.table(survivalTable)


library(randomForest)
modelRF <- randomForest(as.factor(survived)~pclass+age+female+sibsp, data = titanic.train, importance=T, ntree=)
importance(modelRF)
varImpPlot(modelRF)
