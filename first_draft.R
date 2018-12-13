library("readr")
library("caret")
library("corrplot")

# set working directory first!!!
existingProducts <- read.csv("./data/existingproductattributes2017.csv")
newProducts <- read.csv("./data/newproductattributes2017.csv")

dataSet <- existingProducts[,!names(existingProducts) %in% c("BestSellersRank")]
tempSet <- dummyVars(" ~ .", data = dataSet)
dataSet <- data.frame(predict(tempSet, newdata = existingProducts))

corrMatrix <- cor(dataSet)
corrplot(corrMatrix)