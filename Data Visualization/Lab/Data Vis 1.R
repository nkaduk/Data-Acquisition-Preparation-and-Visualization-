#Nate Kaduk
#Data Visualization

#Lab 1: Find mean and other summary statistics using different R libraries and functions

#Read Data
afqtData <- read.csv("C:/Users/natek/Downloads/NLSY97_basicsforclass (5).csv")

#See what this data has to offer
head(afqtData)

#Get afqtScore into a single vector
afqtScore <- afqtData["afqtscore"][,1]

#Make sure everything is good by checking the data type
typeof(afqtScore)

#Find mean but remove missing values
mean(afqtScore, na.rm=TRUE)

#Use the psych library to find the mean, and other summary statistics, a different way
library(psych)

describe(afqtScore)

summary(afqtData$afqtscore)
summary(afqtScore, digits=4)
summary(afqtScore)

quantile(afqtData$afqtscore, na.rm=TRUE)

#Use yet another library to get summary statistics
library(pastecs)
stat.desc(afqtData$afqtscore)


#Make histograms for the data
hist(afqtScore)
hist(afqtScore, freq = T, breaks=20) #Count
hist(afqtScore, freq = F, breaks=20) #Probability

hist(afqtScore, freq = T, breaks=10, xlab="AFQT Score", ylab="Count of People with AFQT Score", border = "Black", col="Red", main = "AQFT Score Histogram")


#Lab 2: Repeat Lab 1 on a different variable, namely rfaminc or family income in $10,000

#summary Statistics for rfaminc
summary(afqtData$rfaminc)
describe(afqtData$rfaminc)
stat.desc(afqtData$rfaminc)

#Histogram for rfaminc
hist(afqtData$rfaminc, breaks=7, col="Light Blue", xlab = "Family Income in $10,000s", ylab="Count", main="Family Income Distribution")


#Lab 3: Make basic table of count/percentages of father/mother years of education

library(party)

parentTable <- table(afqtData$fatheryrs, afqtData$motheryrs)
head(parentTable)

#Father margin table
margin.table(parentTable, 1)
#Mother margin table
margin.table(parentTable,2)

#Find occurrences, in percentages, of both mother and father education 
prop.table(parentTable) #Cell percentages
prop.table(parentTable, 1) #Row percentages
prop.table(parentTable, 2) #Column percentages

