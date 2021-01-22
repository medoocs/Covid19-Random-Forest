options(warn=-1) #kekw

for (p in c("remotes", "tidyverse", "parallel", "doParallel", "caret", "ggplot2", "dplyr", "smoother", "countrycode", "shiny")) {
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

#countries <- codelist$iso3c
#names(countries) <- codelist$country.name.en

#countries <- Filter(Negate(is.na), countries)
#countries <- countries[sort(names(countries))]
#countries <- append(c("Global Data" = "ALL"), countries)

# selectInput("countryCode", "Izbor drzave",
#             choices=countries,
#             selected="HRV")

countryCode <- "HRV" #reactive({input$countryCode})

# dateRangeInput('dateRange',
#                label = 'Izbor datuma: dd.mm.yyyy.',
#                format = 'dd.mm.yyyy.',
#                start = as.Date("2020-02-15"), end = as.Date("2020-11-06")
# )

dateStart <- as.Date("2020-02-15") #reactive({input$dateRange[[1]]})
dateEnd <- as.Date("2021-01-06")   #reactive({input$dateRange[[2]]})

#checkboxInput("retrainFlag", "Ponovno u\u010Denje")
#retrainFlag <- reactive({input$retrainFlag})

processCountry <- function(data) {
  data$confirmed = c(0, diff(data$confirmed, lag = 1))
  if (countryCode == "ALL") {
    data$confirmed <- data$confirmed / data$population
  }
  
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
  
  mobility <- data[ which(data$date >= dateStart & data$date < dateEnd), ]
  
  grouped <- group_by(mobility, iso3c)
  grouped <- do(grouped, processCountry(.))
  mobility <- ungroup(grouped)
  
  data <- mobility[, c("retail_recreation", "grocery_pharmacy", "parks", 
                       "transit_stations", "workplaces", "residential",
                       "pop_density", "confirmed")]
  
  return(data)
}

getParams <- function(trainSet, filename) {
  if (isTRUE(file.exists(filename))) {
    return(readRDS(filename))
  }
  
  trControl <- trainControl(method = "cv",
                            number = 5,
                            search = "grid",
                            verboseIter = FALSE,
                            allowParallel = TRUE)
  
  tuneGrid <- expand.grid(.mtry=c(1:ncol(trainSet)))
  
  registerDoParallel(cores=detectCores()-2)
  
  tree <- caret::train(confirmed ~ .,
                       data = trainSet,
                       method = "parRF",
                       metric = "RMSE",
                       ntree = 100,
                       maxnodes = 30,
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                       nodesize = 5)
  
  stopImplicitCluster()
  
  saveRDS(tree, filename)
  
  return(tree)
}

plotLines <- function(testSet, prediction) {
  # plot real vs pred
  plot(testSet$confirmed, type="l", lwd=3, col="#ff904c", xlab = "Index", ylab = "Daily new cases", main = paste("Actual vs Predicted (", countryCode, ")", sep=""))
  lines(prediction, col="#39a767", lwd=3)
  lines(smth.gaussian(prediction, window=7, tails=TRUE, alpha=3.0), col="#6e4f7b", lwd=3)
  legend("topleft", legend=c("actual (smooth)", "predicted", "predicted (smooth)"), col=c("#ff904c", "#39a767", "#6e4f7b"), lty=1, cex=0.8, lwd=3)
}

plotPO <- function(testSet, prediction) {
  # P-O dijagram
  ggplot() + geom_point(aes(prediction, testSet$confirmed)) + labs(x = "Prediction", y = "Daily new cases", title = "P-O dijagram")
}

data <- getData(countryCode)
summary(data)

set.seed(1234)
# train and test (80-20)
train <- sample(nrow(data), 0.8*nrow(data), replace = FALSE)

trainSet <- data[train,]
testSet <- data[-train,]

# train model
filename <- paste('rf_', countryCode, '.rds', sep="")

trained_rf <- getParams(trainSet, filename)
summary(trained_rf)

prediction <- predict(trained_rf, testSet)

plotLines(testSet, prediction)
plotPO(testSet, prediction)

reziduali <- glm(confirmed ~ ., data = testSet)
summary(reziduali)
