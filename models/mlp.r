library(Rcpp)
library(RSNNS)
library(caret)

evaluation_mlp <- function(df_train, df_validate, df_test){
  # grid search parameters for MLP
  mlp_grid <- expand.grid(
    size = list(c(7), c(7,6,7), c(7,5,5)),
    learnFuncParams = list(c(0.02, 0.01), c(0.015, 0.01)),
    maxit = c(100, 200, 300, 400),
    r2 = 0,
    mae = 0
  )
  
  # grid search
  for(i in 1:nrow(mlp_grid)){
    set.seed(321)
    
    mlp_tune <- mlp(
      df_validate[,1:9],
      df_validate[,10],
      size = unlist(mlp_grid$size[i], use.names=FALSE),
      learnFuncParams= unlist(mlp_grid$learnFuncParams[i], use.names=FALSE),
      maxit = mlp_grid$maxit[i]
    )
    
    pred <- predict(mlp_tune, df_validate[,1:9])
    mlp_grid$r2[i] <- rsquare(pred, df_validate[,10])[1]
    mlp_grid$mae[i] <- mae(pred, df_validate[,10])
  }
  
  system.time(
    co_mlp_model <- mlp(df_train[,1:9], df_train[,10],
                        size = c(7,6,7),
                        learnFuncParams = c(0.02, 0.01),
                        maxit=200
    )
  )
  system.time(
    nox_mlp_model <- mlp(df_train[,1:9], df_train[,11],
                         size = c(7,6,7),
                         learnFuncParams = c(0.02, 0.01),
                         maxit=200
    )
  )
  
  # training set predictions
  co_mlp_pred <- predict(co_mlp_model, df_train[,1:9])
  nox_mlp_pred <- predict(nox_mlp_model, df_train[,1:9])
  
  # test set predictions
  co_mlp_pred_t <- predict(co_mlp_model, df_test[,1:9])
  nox_mlp_pred_t <- predict(nox_mlp_model, df_test[,1:9])
  
  message("Training set performance metrics: ")
  # performance metrics for training set
  message("CO R^2: ", rsquare(co_mlp_pred, df_train[,10])[1])
  message("CO MAE: ", mae(co_mlp_pred, df_train[,10]))
  
  message("NOX R^2: ", rsquare(nox_mlp_pred, df_train[,11])[1])
  message("NOX MAE: ", mae(nox_mlp_pred, df_train[,11]))
  
  
  message("Test set performance metrics: ")
  # performance metrics for test set
  message("CO R^2: ", rsquare(co_mlp_pred_t, df_test[,10])[1])
  message("CO MAE: ", mae(co_mlp_pred_t, df_test[,10]))
  
  message("NOX R^2: ", rsquare(nox_mlp_pred_t, df_test[,11])[1])
  message("NOX MAE: ", mae(nox_mlp_pred_t, df_test[,11]))
  
}
