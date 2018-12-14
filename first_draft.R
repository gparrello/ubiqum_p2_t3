library("readr")
library("caret")
library("corrplot")
library("rbokeh")

# set working directory first!!!
existingProducts <- read.csv("./data/existingproductattributes2017.csv")
newProducts <- read.csv("./data/newproductattributes2017.csv")

# fill up NA?
dataSet <- subset(
  existingProducts,
  Volume < 7000,  # eliminate outliers
  select = -c(  # select features
    BestSellersRank,
    # x5StarReviews,
    # ProductDepth,
    # ProductWidth,
    # ProductHeight,
    # ShippingWeight,
    # ProfitMargin,
    ProductNum
  )
)

# normalize variables
numericIndices <- sapply(
  subset(
    dataSet,
    select = -Volume
  ),
  is.numeric
)
dataSet[numericIndices] <- lapply(dataSet[numericIndices], scale)

# build dummies
dataSet <- data.frame(
  predict(
    dummyVars(" ~ .", data = dataSet),
    newdata = dataSet
  )
)


corrplot(
  cor(dataSet),
  method="color",
  type="lower"
)

# eliminate duplicate extended warranties



inTraining <- createDataPartition(
  dataSet$Volume,
  p = .75,
  list = FALSE
)
trainingSet <- dataSet[inTraining,]
testingSet <- dataSet[-inTraining,]

cvFolds = 10
cvRepeats = 1
fitControl <- trainControl(
  method = "repeatedcv",
  number = cvFolds,
  repeats = cvRepeats,
  search = "random"
)


tuneLength = 1
formula = as.formula("Volume ~ .")
rfModelFile <- "./model/rfModel.rba"
if(!file.exists(rfModelFile)){
  rfModel <- train(
    formula,
    data = trainingSet,
    method = "rf",
    trControl = fitControl,
    tuneLength = tuneLength,
    importance = T
  )
} else {
  rfModel <- load(rfModelFile)
}
varImp(rfModel)

lmModelFile <- "./model/lmModel.rba"
if(!file.exists(lmModelFile)){
linearModel <- train(
  formula,
  data = trainingSet,
  method = "lm",
  trControl = fitControl,
  tuneLength = tuneLength
)
} else {
  lmModel <- load(lmModelFile)
}

figure() %>%
  ly_points(x4StarReviews, Volume, data = trainingSet)
