#remotes::install_github("joachim-gassen/tidycovid19")
library(tidycovid19)
library(tidyverse)
library(ggrepel)
library(gghighlight)
library(zoo)
library(randomForest)
library(caret)
library(ggplot2)
library(sets)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
set.seed(1234)

getData <- function(country, countryCode){
  dateStart = "2020-02-15"
  dateEnd = "2020-11-06"
  
  df <- download_jhu_csse_covid19_data(cached = TRUE, silent = TRUE)
  dp <- download_google_cmr_data(cached = TRUE, silent = TRUE)
  
  data <- na.omit(merge(df, dp, by = c("iso3c", "date")))
  
  mobility <- data[ which(data$date >= dateStart & data$date < dateEnd), ]
  
  mobility$confirmed = c(0, diff(mobility$confirmed, lag = 1))
  for (i in nrow(mobility)) {
    if (i > 0 && mobility[i,]$iso3c != mobility[i-1,]$iso3c) {
      mobility[i,]$confirmed = 0;
    }
  }
  
  retail_recreation = mobility$retail_recreation
  grocery_pharmacy = mobility$grocery_pharmacy 
  parks = mobility$parks 
  transit_stations = mobility$transit_stations 
  workplaces = mobility$workplaces 
  residential = mobility$residential 
  confirmed = mobility$confirmed
  
  data <- data.frame(retail_recreation, 
                     grocery_pharmacy, 
                     parks, 
                     transit_stations, 
                     workplaces, 
                     residential, 
                     confirmed)
  
  write.csv(data, paste('alldata_', country, '.csv', sep=""))
  return(data)
}

globTree <- NULL
getParams <- function(trainSet){
  
  customRF <- list(type = "Regression", library = "randomForest", loop = NULL)
  customRF$parameters <- data.frame(parameter = c("mtry", "ntree", "maxnodes"), class = rep("numeric", 3), label = c("mtry", "ntree", "maxnodes"))
  customRF$grid <- function(x, y, len = NULL, search = "grid") {}
  customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
    randomForest(x, y, mtry = param$mtry, ntree=param$ntree, maxnodes=param$maxnodes, ...)
  }
  customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL) {
    predict(modelFit, newdata)
  }
  customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL) {
    predict(modelFit, newdata, type = "prob")
  }
  customRF$sort <- function(x) x[order(x[,1]),]
  customRF$levels <- function(x) x$classes
  
  trControl <- trainControl(method = "cv",
                            number = 2,
                            search = "grid",
                            verboseIter = TRUE)
  
  tuneGrid <- expand.grid(.mtry=c(1:6),
                          .maxnodes=c(seq(30,70,5)),
                          .ntree=c(seq(200, 1000, 100)))
  
  tree <- train(confirmed ~ .,
                data = trainSet,
                method = customRF,
                metric = "RMSE",
                tuneGrid = tuneGrid,
                trControl = trControl,
                importance = TRUE,
                nodesize = 5
  )
  globTree <<- tree
  return(tree)
}

getModel <- function(tuneGrid, trControl, trainSet, best_maxnodes, best_ntree){
  trained_rf <- train(confirmed~.-X,
                      trainSet,
                      method = "rf",
                      metric = "RMSE",
                      tuneGrid = tuneGrid,
                      trControl = trControl,
                      importance = TRUE,
                      nodesize = 5,
                      ntree = best_ntree,
                      maxnodes = best_maxnodes)
  saveRDS(trained_rf, paste('rf_', country, '.rds', sep=""))
  return(trained_rf)
}

getAnaliza <- function(testSet, prediction){
  # plot real vs pred
  plot(testSet$confirmed, col="blue")
  lines(prediction, col="red")
  # analiza reziduala
  reziduali <- glm(confirmed ~ . -X, data = testSet)
  print(summary(reziduali))
  # P-O dijagram
  ggplot() + geom_point(aes(prediction, testSet$confirmed))
}

# input params
country <- "all"
countryCode <- "HRV"

# load data
filename <- paste('alldata_', country, '.csv', sep="")
if(isTRUE(file.exists(filename))){
  data <- as.data.frame(read.csv(filename, header = TRUE, sep = ','))
}else{
  data <- getData(country, countryCode)
}

# train and test (80-20)
train <- sample(nrow(data), 0.1*nrow(data), replace = FALSE)
trainSet <- data[train,]

filename <- paste('alldata_', 'Croatia', '.csv', sep="")
testSet <- as.data.frame(read.csv(filename, header = TRUE, sep = ','))

# train model
filename <- paste('rf_', country, '.rds', sep="")
if(isTRUE(file.exists(filename))){
  trained_rf <- readRDS(filename)
}else{
  trained_rf <- getParams(trainSet)
  saveRDS(trained_rf, paste('rf_', country, '.rds', sep=""))
}

# predict
prediction <- predict(trained_rf, testSet)

# analiza
getAnaliza(testSet, prediction)
