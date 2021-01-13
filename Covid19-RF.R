#remotes::install_github("joachim-gassen/tidycovid19")
library(tidycovid19)
library(tidyverse)
library(ggrepel)
library(gghighlight)
library(zoo)
library(foreach)
library(parallel)
library(doParallel)
library(randomForest)
library(caret)
library(ggplot2)
library(sets)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
set.seed(1234)
dataglo <- NULL
deleteglo <- NULL
getData <- function(countryCode){
  dateStart = "2020-02-15"
  dateEnd = "2020-11-06"
  
  df <- download_jhu_csse_covid19_data(cached = TRUE, silent = TRUE)
  dp <- download_google_cmr_data(cached = TRUE, silent = TRUE)
  pop <- download_wbank_data(cached = TRUE, silent = TRUE)
  pop <- pop[, c("iso3c", "population", "pop_density")]
  
  if(countryCode == 'ALL'){
    data <- na.omit(merge(df, pop, by = c("iso3c")))
    data <- na.omit(merge(data, dp, by = c("iso3c", "date")))
    
    mobility <- data[ which(data$date >= dateStart & data$date < dateEnd), ]
    mobility$confirmed = c(0, diff(mobility$confirmed, lag = 1)) / mobility$population
    
    toDelete = c()
    for (i in 1:nrow(mobility)) {
      if (i > 1 && mobility[i,]$iso3c != mobility[i-1,]$iso3c) {
        mobility[i,]$confirmed = 0;
      }else if(i > 1 && (mobility[i,]$date - 1) != mobility[i-1,]$date){
        toDelete <- append(toDelete, i)
      }
    }
    if(!is.null(toDelete)){
      mobility = mobility[-toDelete,]
    }
    
    #mobility = mobility[1:200,] # SJETI SE OVO OBRISATI IDIOTE POREMECENI
  }else{
    pop_country <- pop [which(pop$iso3c == countryCode), ]
    df_country <- df[ which(df$iso3c == countryCode), ]
    dp_country <- dp[ which(dp$iso3c == countryCode), ]
    data <- na.omit(merge(df_country, pop_country, by = c("iso3c")))
    data <- na.omit(merge(data, dp_country, by = c("iso3c", "date")))
    mobility <- data[ which(data$date >= dateStart & data$date < dateEnd), ]
    mobility$confirmed = c(0, diff(mobility$confirmed, lag = 1)) / mobility$population
    toDelete = c()
    for (i in 1:nrow(mobility)) {
      if(i > 1 && (as.Date(mobility[i,]$date) - 1) != as.Date(mobility[i-1,]$date)){
        toDelete <- append(toDelete, i)
      }
    }
    deleteglo <<- toDelete
    dataglo<<-mobility
    if(!is.null(toDelete)){
      mobility = mobility[-toDelete,]
    } 
  }
  
  retail_recreation = mobility$retail_recreation
  grocery_pharmacy = mobility$grocery_pharmacy 
  parks = mobility$parks 
  transit_stations = mobility$transit_stations 
  workplaces = mobility$workplaces 
  residential = mobility$residential 
  confirmed = mobility$confirmed
  density = mobility$pop_density
  
  data <- data.frame(retail_recreation, 
                     grocery_pharmacy, 
                     parks, 
                     transit_stations, 
                     workplaces, 
                     residential, 
                     density,
                     confirmed)
  
  write.csv(data, paste('alldata_', countryCode, '.csv', sep=""), row.names = FALSE)
  return(data)
}

# copy pasted from the caret source file with some extra params
customParRF <- list(label = "Parallel Random Forest with more parameters",
     library = c("e1071", "randomForest", "foreach", "import"),
     loop = NULL,
     type = c("Classification", "Regression"),
     parameters = data.frame(parameter = c("mtry", "ntree", "maxnodes"), class = rep("numeric", 3), label = c("mtry", "ntree", "maxnodes")),
     grid = function(x, y, len = NULL, search = "grid"){
       if(search == "grid") {
         out <- data.frame(mtry = caret::var_seq(p = ncol(x), 
                                                 classification = is.factor(y), 
                                                 len = len))
       } else {
         out <- data.frame(mtry = unique(sample(1:ncol(x), size = len, replace = TRUE)))
       }
       out
     },
     fit = function(x, y, wts, param, lev, last, classProbs, ...) {
       workers <- foreach::getDoParWorkers()
       import::from(foreach, `%dopar%`)
       theDots <- list(...)
       theDots$ntree <- if(is.null(theDots$ntree)) 
         formals(randomForest:::randomForest.default)$ntree else 
           theDots$ntree
       
       theDots$x <- x
       theDots$y <- y
       theDots$mtry <- param$mtry
       theDots$ntree <- ceiling(param$ntree/workers)
       theDots$maxnodes = param$maxnodes
       iter_seeds <- sample.int(10000, size = workers)
       out <- foreach::foreach(ntree = 1:workers, .combine = randomForest::combine) %dopar% {
         set.seed(iter_seeds[workers])
         do.call(randomForest::randomForest, theDots)
       }
       if(!inherits(out, "randomForest")) 
         out <- do.call("randomForest::combine", out)
       out$call["x"] <- "x"
       out$call["y"] <- "y"
       out
     },
     predict = function(modelFit, newdata, submodels = NULL) 
       predict(modelFit, newdata),
     prob = function(modelFit, newdata, submodels = NULL)
       predict(modelFit, newdata, type = "prob"),
     predictors = function(x, ...) {
       ## After doing some testing, it looks like randomForest
       ## will only try to split on plain main effects (instead
       ## of interactions or terms like I(x^2).
       varIndex <- as.numeric(names(table(x$forest$bestvar)))
       varIndex <- varIndex[varIndex > 0]
       varsUsed <- names(x$forest$ncat)[varIndex]
       varsUsed
     },
     varImp = function(object, ...){
       varImp <- randomForest::importance(object, ...)
       if(object$type == "regression")
         varImp <- data.frame(Overall = varImp[,"%IncMSE"])
       else {
         retainNames <- levels(object$y)
         if(all(retainNames %in% colnames(varImp))) {
           varImp <- varImp[, retainNames]
         } else {
           varImp <- data.frame(Overall = varImp[,1])
         }
       }
       
       out <- as.data.frame(varImp, stringsAsFactors = TRUE)
       if(dim(out)[2] == 2) {
         tmp <- apply(out, 1, mean)
         out[,1] <- out[,2] <- tmp  
       }
       out
     },
     levels = function(x) x$classes,
     tags = c("Random Forest", "Ensemble Model", "Bagging", "Implicit Feature Selection"),
     sort = function(x) x[order(x[,1]),],
     oob = function(x) {
       out <- switch(x$type,
                     regression =   c(sqrt(max(x$mse[length(x$mse)], 0)), x$rsq[length(x$rsq)]),
                     classification =  c(1 - x$err.rate[x$ntree, "OOB"],
                                         e1071::classAgreement(x$confusion[,-dim(x$confusion)[2]])[["kappa"]]))
       names(out) <- if(x$type == "regression") c("RMSE", "Rsquared") else c("Accuracy", "Kappa")
       out
     })

getParams <- function(trainSet){
  # customRF <- list(type = "Regression", 
  #                  library = "randomForest",
  #                  loop = NULL,
  #                  parameters = data.frame(parameter = c("mtry", "ntree", "maxnodes"), class = rep("numeric", 3), label = c("mtry", "ntree", "maxnodes")),
  #                  grid = function(x, y, len = NULL, search = "grid") {},
  #                  customRF$fit = function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  #                    #RandomForest(x, y, mtry = param$mtry, ntree=param$ntree, maxnodes=param$maxnodes, ...)
  #                  },
  #                  predict = function(modelFit, newdata, preProc = NULL, submodels = NULL) {
  #                    predict(modelFit, newdata)
  #                  },
  #                  prob = function(modelFit, newdata, preProc = NULL, submodels = NULL) {
  #                    predict(modelFit, newdata, type = "prob")
  #                  },
  #                  sort = function(x) x[order(x[,1]),],
  #                  levels = function(x) x$classes
  # )
  
  trControl <- trainControl(method = "cv",
                            number = 4,
                            search = "grid",
                            verboseIter = TRUE,
                            allowParallel = TRUE)
  
  tuneGrid <- expand.grid(.mtry=c(1:7),
                          .maxnodes=c(seq(30,100,10)),
                          .ntree=c(seq(200, 2000, 200)))
  
  registerDoParallel(cores=8)
  
  tree <- train(confirmed ~ .,
                data = trainSet,
                method = customParRF,
                metric = "RMSE",
                tuneGrid = tuneGrid,
                trControl = trControl,
                importance = TRUE,
                nodesize = 5
  )
  
  stopImplicitCluster()
  
  return(tree)
}

getAnaliza <- function(testSet, prediction){
  # plot real vs pred
  plot(testSet$confirmed, col="blue", xlab = "Date", ylab = "Daily new cases", main = "Actuall vs Predicted")
  lines(prediction, col="red")
  # analiza reziduala
  reziduali <- glm(confirmed ~ ., data = testSet)
  print(summary(reziduali))
  # P-O dijagram
  ggplot() + geom_point(aes(prediction, testSet$confirmed)) + labs(x = "Prediction", y = "Daily new cases", title = "P-O dijagram")
}

# input params
countryCode <- "ALL"

# load data
filename <- paste('alldata_', countryCode, '.csv', sep="")
if(isTRUE(file.exists(filename))){
  data <- as.data.frame(read.csv(filename, header = TRUE, sep = ','))
}else{
  data <- getData(countryCode)
}

# train and test (80-20)
train <- sample(nrow(data), 0.8*nrow(data), replace = FALSE)
trainSet <- data[train,]
testSet <- data[-train,]

# train model
filename <- paste('rf_', countryCode, '.rds', sep="")
if(isTRUE(file.exists(filename))){
  trained_rf <- readRDS(filename)
}else{
  trained_rf <- getParams(trainSet)
  saveRDS(trained_rf, paste('rf_', countryCode, '.rds', sep=""))
}

# predict
prediction <- predict(trained_rf, testSet)

# analiza
getAnaliza(testSet, prediction)

