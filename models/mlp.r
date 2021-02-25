library(Rcpp)
library(RSNNS)
library(caret)

evaluation_mlp <- function(df_train, df_validate, df_test){
  # removing AP, AH for CO hyperparameter tuning
  co_df_train <- df_train[, c(-2,-3,-11)]
  co_df_validate <- df_validate[, c(-2,-3,-11)]
  co_df_test <- df_test[, c(-2,-3,-11)]
  
  # removing AFDP, AH for NOX hyperparameter tuning
  nox_df_train <- df_train[, c(-3,-4,-10)]
  nox_df_validate <- df_train[, c(-3,-4,-10)]
  nox_df_test <- df_test[, c(-3,-4,-10)]
  
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
      co_df_validate[,1:7],
      co_df_validate[,8],
      size = unlist(mlp_grid$size[i], use.names=FALSE),
      learnFuncParams= unlist(mlp_grid$learnFuncParams[i], use.names=FALSE),
      maxit = mlp_grid$maxit[i]
    )

    pred <- predict(mlp_tune, co_df_validate[,1:7])
    mlp_grid$r2[i] <- rsquare(pred, co_df_validate[,8])[1]
    mlp_grid$mae[i] <- mae(pred, co_df_validate[,8])
  }
  
  for(i in 1:nrow(mlp_grid)){
    set.seed(321)
    
    mlp_tune <- mlp(
      nox_df_validate[,1:7],
      nox_df_validate[,8],
      size = unlist(mlp_grid$size[i], use.names=FALSE),
      learnFuncParams= unlist(mlp_grid$learnFuncParams[i], use.names=FALSE),
      maxit = mlp_grid$maxit[i]
    )
    
    pred <- predict(mlp_tune, nox_df_validate[,1:7])
    mlp_grid$r2[i] <- rsquare(pred, nox_df_validate[,8])[1]
    mlp_grid$mae[i] <- mae(pred, nox_df_validate[,8])
  }
  
  system.time(
    co_mlp_model <- mlp(co_df_train[,1:7], co_df_train[,8],
                        size = c(7,5,5),
                        learnFuncParams = c(0.02, 0.01),
                        maxit=400
    )
  )
  system.time(
    nox_mlp_model <- mlp(nox_df_train[,1:7], nox_df_train[,8],
                         size = c(7,5,5),
                         learnFuncParams = c(0.015, 0.01),
                         maxit=200
    )
  )
  
  # training set predictions
  co_mlp_pred <- predict(co_mlp_model, co_df_train[,1:7])
  nox_mlp_pred <- predict(nox_mlp_model, nox_df_train[,1:7])
  
  # test set predictions
  co_mlp_pred_t <- predict(co_mlp_model, co_df_test[,1:7])
  nox_mlp_pred_t <- predict(nox_mlp_model, nox_df_test[,1:7])
  
  message("Training set performance metrics: ")
  # performance metrics for training set
  message("CO R^2: ", rsquare(co_mlp_pred, co_df_train[,8])[1])
  message("CO MAE: ", mae(co_mlp_pred, co_df_train[,8]))
  
  message("NOX R^2: ", rsquare(nox_mlp_pred, nox_df_train[8])[1])
  message("NOX MAE: ", mae(nox_mlp_pred, nox_df_train[,8]))
  
  
  message("Test set performance metrics: ")
  # performance metrics for test set
  message("CO R^2: ", rsquare(co_mlp_pred_t, co_df_test[,8])[1])
  message("CO MAE: ", mae(co_mlp_pred_t, co_df_test[,8]))
  
  message("NOX R^2: ", rsquare(nox_mlp_pred_t, nox_df_test[,8])[1])
  message("NOX MAE: ", mae(nox_mlp_pred_t, nox_df_test[,8]))
  
}
