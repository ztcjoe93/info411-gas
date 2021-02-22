library (randomForest)

evaluation_rf <- function(df_train, df_validate, df_test){
  # tuning parameters for randomForest
  system.time(
    tuned_rf <- tuneRF(
      x = df_validate[,1:9],
      y = df_validate[,10],
      sampsize = nrow(df_validate[,1:9]),
      ntreeTry = 300,
      mtryStart = 5,
      stepFactor = 1.5,
      improve = 0.01,
      trace= TRUE,
    )
  )
  
  # training model with best parameters
  co_rf <- randomForest(CO ~ ., data = df_train[,-11], mtry = 7,ntree = 300)
  nox_rf <- randomForest(NOX ~ .,data = df_train[,-10],mtry = 7,ntree = 300)
  
  # predictions for training set
  co_rf_pred <- predict(co_rf, df_train[,-11])
  nox_rf_pred <- predict(nox_rf, df_train[,-10])
  
  # predictions for test set
  co_rf_pred_t <- predict(co_rf, df_test[,-11])
  nox_rf_pred_t <- predict(nox_rf, df_test[,-10])
  
  message("Training set performance metrics: ")
  # performance metrics on training set
  message("CO R^2: ", rsquare(co_rf_pred, df_train[, 10]))
  message("CO MAE: ", mae(co_rf_pred, df_train[, 10]))
  
  message("NOX R^2: ", rsquare(nox_rf_pred, df_train[, 11]))
  message("NOX MAE: ", mae(nox_rf_pred, df_train[, 11]))
  
  message("Test set performance metrics: ")
  # performance metrics on test set
  message("CO R^2: ", rsquare(co_rf_pred_t, df_test[, 10]))
  message("CO MAE: ", mae(co_rf_pred_t, df_test[, 10]))
  
  message("NOX R^2: ", rsquare(nox_rf_pred_t, df_test[, 11]))
  message("NOX MAE: ", mae(nox_rf_pred_t, df_test[, 11]))
}