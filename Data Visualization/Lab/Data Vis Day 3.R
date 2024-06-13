#Nate Kaduk
#Data Visualization

#Extra Lab 1: Merge movies spreadsheets, adjust for inflation, and find top 10 grossing movies


#install.packages("readxl")

library(readxl)
filename <- "C:/Users/natek/Downloads/movies - R version.xlsx"

movies <- read_excel(filename, sheet = "movies")
cpi <- read_excel(filename, sheet="cpi")


head(movies)
head(cpi)

colnames(movies)
colnames(cpi)

mergedMovies <- merge(movies, cpi, "Year")
head(mergedMovies)
#View(mergedMovies)


#mergedMovies$realDollars <- movies["gross_million"]*mergedMovies["Inflator"]

mergedMovies$testDollars <- (mergedMovies$gross_million*mergedMovies$Inflator)
head(mergedMovies)
testVar <- mergedMovies$gross_million

sortedMovies <- mergedMovies[order(mergedMovies$gross_million, decreasing=T),]
sortedMovies$Movie[1:10]

sortedMoviesReal <-mergedMovies[order(mergedMovies$testDollars, decreasing=T),]
sortedMoviesReal$Movie[1:10]

#Extra Lab 2: Get summaries by different groups using different methods
fruit<- read_excel("C:/Users/natek/Downloads/messy_data_collapse_R.xlsx", sheet="Sheet1")
library(doBy)
head(fruit)

#attach(fruit)
summaryBy(units+price~region, data=fruit)
summaryBy(units+price~fruit, data=fruit)

#Collapse Method 1
#Fruit region statistics
summaryFruit1 <- summaryBy(units+price~fruit+region, FUN=c(mean, sd), data=fruit)
summaryFruit1

#Collapse Method 2
#Collapse based on user-defined function
halfmean <- function(x)return(mean(x)/2)
summaryFruit2 <- summaryBy(price+units~fruit+region, FUN=halfmean, data=fruit)
summaryFruit2

#Collapse Method 3
#Fruit-price statistics then region-units statistics
library(collapse)
summaryFruit3 <- collap(fruit, units+price~fruit+region, FUN=list(mean,sd))
summaryFruit3

