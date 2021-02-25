
evaluation_lr <- function(df_train, df_validate, df_test){

  #There are no tuning parameters for regression
  #References: https://stackoverflow.com/questions/47822694/logistic-regression-tuning-parameter-grid-in-r-caret-package/48218280#48218280
  #            https://datascience.stackexchange.com/questions/31614/which-parameters-are-hyper-parameters-in-a-linear-regression
  
  co_droplist = c("AH", "AP", "NOX")
  nox_droplist = c("AH", "AFDP", "CO")
  
  # train model with best parameters
  lr_co <- lm(CO ~ ., data = df_train[,!(names(df_train) %in% co_droplist)])
  lr_nox <- lm(NOX ~ .,data = df_train[,!(names(df_train) %in% nox_droplist)]) 
  
  # predictions for training set
  lr_co_pred <- predict(lr_co, df_train[,!(names(df_train) %in% co_droplist)])
  lr_nox_pred <- predict(lr_nox, df_train[,!(names(df_train) %in% nox_droplist)])
  
  # predictions for test set
  lr_co_pred_t <- predict(lr_co, df_test[,!(names(df_test) %in% co_droplist)])
  lr_nox_pred_t <- predict(lr_nox, df_test[,!(names(df_test) %in% nox_droplist)])
  
  
  message("Training set performance metrics: ")
  # performance metrics on training set
  message("CO R^2: ", rsquare(lr_co_pred, df_train[, 10]))
  message("CO MAE: ", mae(lr_co_pred, df_train[, 10]))
  
  message("NOX R^2: ", rsquare(lr_nox_pred, df_train[, 11]))
  message("NOX MAE: ", mae(lr_nox_pred, df_train[, 11]))
  
  
  message("Test set performance metrics: ")
  # performance metrics on test set
  message("CO R^2: ", rsquare(lr_co_pred_t, df_test[, 10]))
  message("CO MAE: ", mae(lr_co_pred_t, df_test[, 10]))
  
  message("NOX R^2: ", rsquare(lr_nox_pred_t, df_test[, 11]))
  message("NOX MAE: ", mae(lr_nox_pred_t, df_test[, 11]))
}
