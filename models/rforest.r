library (randomForest)

evaluation_rf <- function(df_train, df_validate, df_test){
  # removing AP, AH for CO hyperparameter tuning
  co_df_train <- df_train[, c(-2,-3,-11)]
  co_df_validate <- df_validate[, c(-2,-3,-11)]
  co_df_test <- df_test[, c(-2,-3,-11)]
  
  # removing AFDP, AH for NOX hyperparameter tuning
  nox_df_train <- df_train[, c(-3,-4,-10)]
  nox_df_validate <- df_train[, c(-3,-4,-10)]
  nox_df_test <- df_test[, c(-3,-4,-10)]
  
  # tuning parameters for randomForest
  system.time(
    co_tuned_rf <- tuneRF(
      x = co_df_validate[,1:7],
      y = co_df_validate[,8],
      sampsize = nrow(co_df_validate[,1:8]),
      ntreeTry = 300,
      mtryStart = 5,
      stepFactor = 1.5,
      improve = 0.01,
      trace= TRUE,
    )
  )
  
  system.time(
    nox_tuned_rf <- tuneRF(
      x = nox_df_validate[,1:7],
      y = nox_df_validate[,8],
      sampsize = nrow(nox_df_validate[,1:8]),
      ntreeTry = 300,
      mtryStart = 5,
      stepFactor = 1.5,
      improve = 0.01,
      trace= TRUE,
    )
  )
  
  # training model with best parameters
  co_rf <- randomForest(CO ~ ., data = co_df_train, mtry = 4,ntree = 300)
  nox_rf <- randomForest(NOX ~ .,data = nox_df_train,mtry = 4,ntree = 300)
  
  # predictions for training set
  co_rf_pred <- predict(co_rf, co_df_train[,1:7])
  nox_rf_pred <- predict(nox_rf, nox_df_train[,1:7])
  
  # predictions for test set
  co_rf_pred_t <- predict(co_rf, co_df_test[,1:7])
  nox_rf_pred_t <- predict(nox_rf, nox_df_test[,1:7])
  
  message("Training set performance metrics: ")
  # performance metrics on training set
  message("CO R^2: ", rsquare(co_rf_pred, co_df_train[, 8]))
  message("CO MAE: ", mae(co_rf_pred, co_df_train[, 8]))
  
  message("NOX R^2: ", rsquare(nox_rf_pred, nox_df_train[, 8]))
  message("NOX MAE: ", mae(nox_rf_pred, nox_df_train[, 8]))
  
  message("Test set performance metrics: ")
  # performance metrics on test set
  message("CO R^2: ", rsquare(co_rf_pred_t, co_df_test[, 8]))
  message("CO MAE: ", mae(co_rf_pred_t, co_df_test[, 8]))
  
  message("NOX R^2: ", rsquare(nox_rf_pred_t, nox_df_test[, 8]))
  message("NOX MAE: ", mae(nox_rf_pred_t, nox_df_test[, 8]))
}