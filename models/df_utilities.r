
# void function, returns list of datasets
getDf <- function(){
  # fetch datasets from folder
  cDir <- rstudioapi::getActiveDocumentContext()$path
  datasetDir <- sub("(.*/)models/.*", "\\1", cDir)
  
  years <- c("2011", "2012", "2013", "2014", "2015")
  
  df_train <- list()
  df_validate <- list()
  df_test <- list()
  
  # parse datasets into respective training, validation, test sets
  for(i in seq_along(years)){
    if(years[i] == "2011" || years[i] == "2012"){
      df_train[[i]] <- read.csv(file=paste0(datasetDir, "dataset/", "gt_", years[i], ".csv"))
    } else if (years[i] == "2013"){
      df_validate <- read.csv(file=paste0(datasetDir, "dataset/", "gt_", years[i], ".csv"))
    } else {
      df_test[[i]] <- read.csv(file=paste0(datasetDir, "dataset/", "gt_", years[i], ".csv"))
    }
  }
  
  # merge datasets
  df_train = do.call(what=rbind, args=df_train)
  df_train = data.frame(apply(df_train[1:9],2,scale), df_train[10:11])
  
  df_validate = data.frame(apply(df_validate[1:9],2,scale), df_validate[10:11])
  
  df_test = do.call(what=rbind, args=df_test)
  df_test = data.frame(apply(df_test[1:9],2,scale), df_test[10:11])
  
  dfs = list(df_train, df_validate, df_test)
  return (dfs)
}

# convenience functions for metrics evaluation
mse <- function(y, yhat){
  differences <- y-yhat
  return (mean(differences^2))
}

rmse <- function(y, yhat){
  differences <- y-yhat
  return (sqrt(mean(differences)^2))
}

rsquare <- function(y, yhat){
  return (cor(y, yhat)^2)
}

mae <- function(y, yhat){
  differences <- y-yhat
  return (mean(abs(differences)))
}