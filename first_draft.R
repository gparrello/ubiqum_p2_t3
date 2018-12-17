library("readr")
library("caret")
library("corrplot")
library("rbokeh")


plotHistFunc <- function(workSet){
  # To plot a grid of histograms and bar charts
  plot_list <- vector(mode = 'list')
  for (i in names(workSet)){
    x <- workSet[,i]
    hist_bins <- diff(range(x)) / (2 * IQR(x) / length(x)^(1/3))
    plot_list[[i]] <- figure(xlab = NULL) %>%
      ly_hist(x, freq=TRUE, breaks=hist_bins) %>%
      # ly_density(x) %>%
      x_axis(number_formatter = "numeral", format = "0.") %>%
      theme_axis("x", major_label_orientation = 45)
  }
  plot_num <- length(plot_list)
  total_columns = 2
  p <- grid_plot(
    plot_list,
    ncol = total_columns,
    width = plot_width
  )
  return(p)
}


# fill up NA?
selectAttributes <- function(dataSet){
  outputSet <- subset(
    dataSet,
    select = -c(  # select features
      BestSellersRank,
      x5StarReviews,
      x3StarReviews,
      x1StarReviews,
      ProductDepth,
      ProductWidth,
      ProductHeight,
      ShippingWeight,
      ProfitMargin,
      ProductNum,
      NegativeServiceReview,
      Recommendproduct,
      Price
    )
  )
  return(outputSet)
}

normalizeThis <- function(dataSet){
  # normalize variables
  numericIndices <- sapply(
    subset(
      dataSet,
      select = -Volume
    ),
    is.numeric
  )
  dataSet[numericIndices] <- lapply(dataSet[numericIndices], scale)
  return(dataSet)
}

# set working directory first!!!
existingProducts <- read.csv("./data/existingproductattributes2017.csv")
newProducts <- read.csv("./data/newproductattributes2017.csv")

# select attributes
dataSet <- selectAttributes(existingProducts)
# eliminate outliers
dataSet <- subset(
  dataSet,
  Volume < 7000
)
# normalize values
dataSet <- normalizeThis(dataSet)

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

set.seed(8888)
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

formula <- as.formula("Volume ~ .")
tuneLength <- 1
models <- list(
  "rf",
  "svmLinear",
  "svmRadial",
  "lm"
)
trainedModels <- vector(mode="list")

for(model in models){
  print(paste("Trying training with", model, "method."))
  modelFile <- paste("./models/", model, "_model.rba", sep="")
  if(!file.exists(modelFile)){
    print("Training new model")
    trainedModel <- train(
      formula,
      data = trainingSet,
      method = model,
      trControl = fitControl,
      tuneLength = tuneLength#,
      #importance = T
    )
    save(trainedModel, file = modelFile)
  } else {
    print("Loading from file")
    load(modelFile) # will create variable trainedModel
  }
  trainedModels[[model]] <- trainedModel
}
