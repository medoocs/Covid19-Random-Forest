options(warn=-1) #kekw

for (p in c("remotes", "tidyverse", "parallel", "doParallel", "caret", "ggplot2", "dplyr", "smoother")) {
  if (!(p %in% rownames(installed.packages()))) {
    install.packages(p)
  }
  library(p, character.only=TRUE)
}

if (!("tidycovid19" %in% rownames(installed.packages()))) {
  library(remotes)
  remotes::install_github("joachim-gassen/tidycovid19")
}
library(tidycovid19)

dateStart <- "2020-02-15"
#dateEnd <- "2020-11-06"
dateEnd <- "2021-01-06" # stavili smo do prvog mjeseca da bi dobili vise podataka

tryCatch({
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}, error = function(e) {
  knitr::opts_knit$set(root.dir = dirname(rstudioapi::getActiveDocumentContext()$path))
})
set.seed(1234)

countryCode <- "HRV"

processCountry <- function(data) {
  data$confirmed = c(0, diff(data$confirmed, lag = 1)) / data$population
  
  toDelete = c()
  for (i in 2:nrow(data)) {
    if ((as.Date(data[i,]$date) - 1) != as.Date(data[i-1,]$date)) {
      toDelete <- append(toDelete, i)
    }
  }
  if (!is.null(toDelete)) {
    data = data[-toDelete,]
  }
  
  data$confirmed = smth.gaussian(data$confirmed)
  return(na.omit(data))
}

fetchData <- function(countryCode) {
  filename <- paste('alldata_', countryCode, '.csv', sep="")
  if (isTRUE(file.exists(filename))) {
    return(as.data.frame(read.csv(filename, header = TRUE, sep = ',')))
  }
  
  df <- download_jhu_csse_covid19_data(cached = TRUE, silent = TRUE)
  dp <- download_google_cmr_data(cached = TRUE, silent = TRUE)
  pop <- download_wbank_data(cached = TRUE, silent = TRUE)
  pop <- pop[, c("iso3c", "population", "pop_density")]
  
  if (countryCode == 'ALL') {
    data <- na.omit(merge(df, pop, by = c("iso3c")))
    data <- na.omit(merge(data, dp, by = c("iso3c", "date")))
  } else {
    pop_country <- pop [which(pop$iso3c == countryCode), ]
    df_country <- df[ which(df$iso3c == countryCode), ]
    dp_country <- dp[ which(dp$iso3c == countryCode), ]
    data <- na.omit(merge(df_country, pop_country, by = c("iso3c")))
    data <- na.omit(merge(data, dp_country, by = c("iso3c", "date")))
  }
  
  write.csv(data, paste('alldata_', countryCode, '.csv', sep=""), row.names = FALSE)
  
  return(data)
}

getData <- function(countryCode) {
  data <- fetchData(countryCode)
  
  #population <<- data$population[1]
  
  mobility <- data[ which(data$date >= dateStart & data$date < dateEnd), ]
  
  grouped <- group_by(mobility, iso3c)
  grouped <- do(grouped, processCountry(.))
  mobility <- ungroup(grouped)
  
  data <- mobility[, c("retail_recreation", "grocery_pharmacy", "parks", 
                       "transit_stations", "workplaces", "residential",
                       "pop_density", "confirmed")]
  
  return(data)
}

data <- getData(countryCode)
summary(data)

getParams <- function(trainSet) {
  trControl <- trainControl(method = "cv",
                            number = 5,
                            search = "grid",
                            verboseIter = TRUE,
                            allowParallel = TRUE)
  
  tuneGrid <- expand.grid(.mtry=c(1:ncol(trainSet)))
  
  registerDoParallel(cores=detectCores()-2)
  
  tree <- train(confirmed ~ .,
                data = trainSet,
                method = "parRF",
                metric = "RMSE",
                ntree = 100,
                maxnodes = 30,
                tuneGrid = tuneGrid,
                trControl = trControl,
                importance = TRUE,
                nodesize = 5
  )
  
  stopImplicitCluster()
  
  return(tree)
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
summary(trained_rf)

getAnaliza <- function(testSet, prediction) {
  # plot real vs pred
  plot(testSet$confirmed, type="l", lwd=3, col="blue", xlab = "Date", ylab = "Daily new cases", main = "Actual vs Predicted")
  #lines(testSet$confirmed, col="blue")
  lines(prediction, col="red", lwd=3)
  legend("topleft", legend=c("actual (smooth)", "predicted"), col=c("blue", "red"), lty=1, cex=0.8)
  # P-O dijagram
  ggplot() + geom_point(aes(prediction, testSet$confirmed)) + labs(x = "Prediction", y = "Daily new cases", title = "P-O dijagram")
}
prediction <- predict(trained_rf, testSet)
getAnaliza(testSet, prediction)

reziduali <- glm(confirmed ~ ., data = testSet)
summary(reziduali)