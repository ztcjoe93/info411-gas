# relevant SVR package
library(e1071)
library(caret)

evaluation_svm <- function(df_train, df_validate, df_test){
  # gridsearch for best params
  system.time(
    svm_grid <- tune(svm,
                     train.x = df_train[,1:9],
                     train.y = df_train[,10],
                     validation.x = df_validate[,1:9],
                     validation.y = df_validate[,10],
                     ranges = list(gamma = 0.0222222 * c(0.01, 0.1, 1, 10), cost = c(0.1, 1, 10),
                                   type = c("eps-regression", "nu-regression"), kernel = c("radial", "sigmoid", "linear")),
                     tunecontrol = tune.control(sampling="fix")
    )
  )
  
  # train model with best parameters
  svm_model_co <- svm(CO ~ ., data=df_train[,-11],type="nu-regression",cost=10,nu=0.5,kernel="radial")
  svm_model_nox <- svm(NOX ~., data=df_train[,-10],type="nu-regression",cost=10,nu=0.5,kernel="radial")
  
  # training set predictions
  svm_co_pred <- predict(svm_model_co, df_train[,-11])
  svm_nox_pred <- predict(svm_model_nox, df_train[,-10])
  
  # test set predictions
  svm_co_pred_t <- predict(svm_model_co, df_test[, -11])
  svm_nox_pred_t <- predict(svm_model_nox, df_test[, -10])
  
  message("Training set performance metrics: ")
  # performance metrics on training set
  message("CO R^2: ", rsquare(svm_co_pred, df_train[, 10]))
  message("CO MAE: ", mae(svm_co_pred, df_train[, 10]))
  
  message("NOX R^2: ", rsquare(svm_nox_pred, df_train[, 11]))
  message("NOX MAE: ", mae(svm_nox_pred, df_train[, 11]))
  
  message("Test set performance metrics: ")
  # performance metrics on test set
  message("CO R^2: ", rsquare(svm_co_pred_t, df_test[, 10]))
  message("CO MAE: ", mae(svm_co_pred_t, df_test[, 10]))
  
  message("NOX R^2: ", rsquare(svm_nox_pred_t, df_test[, 11]))
  message("NOX MAE: ", mae(svm_nox_pred_t, df_test[, 11]))
}