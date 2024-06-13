#Nate Kaduk
#Data Visualization

afqtData <- read.csv("C:/Users/natek/Downloads/NLSY97_basicsforclass (5).csv")

head(afqtData)

#Lab 4: Make string version of father/mother education rather than having to rely on years alone
breaks<- c(-Inf, 11, 12, 15, Inf) #Cut off points for education levels
names <- c("LTHS", "HS", "Some College", "BA")

afqtData$fatherED <- cut(afqtData$fatheryrs, breaks=breaks, labels=names)
summary(afqtData$fatherED)

afqtData$motherED <- cut(afqtData$motheryrs, breaks=breaks, labels=names)
summary(afqtData$motherED)

head(afqtData)

myTable <- table(afqtData$motherED, afqtData$fatherED)
myTable

summary(myTable)

#Write table to a file
#write.table(myTable, file = "C:/Users/natek/Downloads/testTable.txt", sep = "\t")

#Lab 5, part a: Plot height and weight of entries
#Convert the variables inches and feet into one single variable in inches 
afqtData$heightin97[is.na(afqtData$heightin97)]=0 #Convet system missing into zero
afqtData$height97 = afqtData$heightft97*12+afqtData$heightin97

par(mfrow=c(1,1))
plot(afqtData$height97, afqtData$weight97)
plot(afqtData$height97, afqtData$weight97, xlab="Height (Inches)", ylab="Weight (Pounds)", main="Height vs Weight", col="blue")
abline(lm(afqtData$weight97~afqtData$height97), col = "red")



#Lab 6: Combining Data through various methods and then summarizing on the variable ba.

#Method 1
aggregate(afqtData, list(BA=afqtData$ba), mean, na.rm=T)


#Method 2
hasBA = subset(afqtData, ba>=1)
summary(hasBA$afqtscore)

noBA = subset(afqtData, ba <=0)
summary(noBA$afqtscore)

library(psych)
describeBy(afqtData$afqtscore, group=afqtData$ba)

library(dplyr)
afqtData %>%
  group_by(ba) %>%
  summarise_at(vars(afqtscore), list(name=mean), na.rm=T)

