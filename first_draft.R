library("readr")
library("caret")
library("corrplot")

# set working directory first!!!
existingProducts <- read.csv("./data/existingproductattributes2017.csv")
newProducts <- read.csv("./data/newproductattributes2017.csv")

dataSet <- existingProducts[,!names(existingProducts) %in% c("BestSellersRank", "x5StarReviews")]
dataSet <- data.frame(
  predict(
    tempSet <- dummyVars(" ~ .", data = dataSet),
    newdata = existingProducts
  )
)

corrplot(
  cor(dataSet),
  method="color",
  type="upper"
)

# eliminate outliers
# fill up NA
# eliminate duplicate extended warranties
# normalize variables

corrplot(
  
)