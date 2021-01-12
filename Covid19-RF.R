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
  
  new_cases <- df[ which(df$country==country & df$date >= dateStart & df$date < dateEnd), ]
  mobility <- dp[ which(dp$date >= dateStart & dp$date < dateEnd & dp$iso3c == countryCode), ]
  
  retail_recreation = mobility$retail_recreation
  grocery_pharmacy = mobility$grocery_pharmacy 
  parks = mobility$parks 
  transit_stations = mobility$transit_stations 
  workplaces = mobility$workplaces 
  residential = mobility$residential 
  confirmed = new_cases$confirmed
  
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

getParams <- function(trainSet){
  # best mtry param
  trControl <- trainControl(method = "cv",
                            number = 10,
                            search = "grid")
  tuneGrid <- expand.grid(.mtry = c(1: 5))
  rf_mtry <- train(confirmed ~ . -X,
                   data = trainSet,
                   method = "rf",
                   metric = "RMSE",
                   tuneGrid = tuneGrid,
                   trControl = trControl,
                   importance = TRUE,
                   nodesize = 5,
                   ntree = 300)
  best_mtry <- rf_mtry$bestTune$mtry

  # best maxnodes param
  store_maxnode <- list()
  tuneGrid <- expand.grid(.mtry = best_mtry)
  for (maxnodes in c(20: 30)) {
    set.seed(1234)
    rf_maxnode <- train(confirmed~.-X,
                        data = trainSet,
                        method = "rf",
                        metric = "RMSE",
                        tuneGrid = tuneGrid,
                        trControl = trControl,
                        importance = TRUE,
                        nodesize = 5,
                        maxnodes = maxnodes,
                        ntree = 300)
    key <- toString(maxnodes)
    store_maxnode[[key]] <- rf_maxnode
  }
  results_node <- resamples(store_maxnode)
  #print(summary(results_node))
  res <- summary(results_node)
  tmp <- as.data.frame(res[["statistics"]][["Rsquared"]])
  best_maxnodes <- as.numeric(rownames(tmp[match(max(tmp$Max.),tmp$Max.),]))

  # best ntree param
  store_maxtrees <- list()
  for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
    rf_maxtrees <- train(confirmed~.-X,
                         data = trainSet,
                         method = "rf",
                         metric = "RMSE",
                         tuneGrid = tuneGrid,
                         trControl = trControl,
                         importance = TRUE,
                         nodesize = 5,
                         maxnodes = best_maxnodes,
                         ntree = ntree)
    key <- toString(ntree)
    store_maxtrees[[key]] <- rf_maxtrees
  }
  results_tree <- resamples(store_maxtrees)
  #print(summary(results_tree))
  res <- summary(results_tree)
  tmp <- as.data.frame(res[["statistics"]][["Rsquared"]])
  best_ntree <- as.numeric(rownames(tmp[match(max(tmp$Max.),tmp$Max.),]))
  
  
  returnList <- list("mtry" = best_mtry, "grid" = tuneGrid, "control" = trControl, "maxnodes" = best_maxnodes, "ntree" = best_ntree)
  return(returnList)
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
country <- "Croatia"
countryCode <- "HRV"

# load data
filename <- paste('alldata_', country, '.csv', sep="")
if(isTRUE(file.exists(filename))){
  data <- as.data.frame(read.csv(filename, header = TRUE, sep = ','))
}else{
  data <- getData(country, countryCode)
}

# train and test (80-20)
train <- sample(nrow(data), 0.8*nrow(data), replace = FALSE)
trainSet <- data[train,]
testSet <- data[-train,]

# train model
filename <- paste('rf_', country, '.rds', sep="")
if(isTRUE(file.exists(filename))){
  trained_rf <- readRDS(filename)
}else{
  # best mtry, maxnodes and ntree
  params <- getParams(trainSet)
  best_mtry <- params$mtry
  tuneGrid <- params$grid
  trControl <- params$control
  best_maxnodes <- params$maxnodes
  best_ntree <- params$ntree
  
  # train
  trained_rf <- getModel(tuneGrid, trControl, trainSet, best_maxnodes, best_ntree)
}

# predict
prediction <- predict(trained_rf, testSet)

# analiza
getAnaliza(testSet, prediction)
