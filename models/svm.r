# relevant SVR package
library(e1071)
library(caret)

evaluation_svm <- function(df_train, df_validate, df_test){
  # removing AP, AH for CO hyperparameter tuning
  co_df_train <- df_train[, c(-2,-3,-11)]
  co_df_validate <- df_validate[, c(-2,-3,-11)]
  co_df_test <- df_test[, c(-2,-3,-11)]
  
  # removing AFDP, AH for NOX hyperparameter tuning
  nox_df_train <- df_train[, c(-3,-4,-10)]
  nox_df_validate <- df_train[, c(-3,-4,-10)]
  nox_df_test <- df_test[, c(-3,-4,-10)]
  
  # gridsearch for best params
  system.time(
    co_svm_grid <- tune(svm,
                     train.x = co_df_train[,1:7],
                     train.y = co_df_train[,8],
                     validation.x = co_df_validate[,1:7],
                     validation.y = co_df_validate[,8],
                     ranges = list(gamma = 0.0222222 * c(0.01, 0.1, 1, 10), cost = c(0.1, 1, 10),
                                   type = c("eps-regression", "nu-regression"), kernel = c("radial", "sigmoid", "linear")),
                     tunecontrol = tune.control(sampling="fix")
    )
  )
  
  system.time(
    nox_svm_grid <- tune(svm,
                        train.x = nox_df_train[, 1:7],
                        train.y = nox_df_train[,8],
                        validation.x = nox_df_validate[,1:7],
                        validation.y = nox_df_validate[,8],
                        ranges = list(gamma = 0.0222222 * c(0.01, 0.1, 1, 10), cost = c(0.1, 1, 10),
                                      type = c("eps-regression", "nu-regression"), kernel = c("radial", "sigmoid", "linear")),
                        tunecontrol = tune.control(sampling="fix")
                        )
  )
  
  # train model with best parameters
  svm_model_co <- svm(CO ~ ., data=co_df_train,type="eps-regression",gamma=0.222222,cost=10,nu=0.5,kernel="radial")
  svm_model_nox <- svm(NOX ~., data=nox_df_train,type="nu-regression",gamma=0.222222, cost=10,nu=0.5,kernel="radial")
  
  # training set predictions
  svm_co_pred <- predict(svm_model_co, co_df_train[,1:7])
  svm_nox_pred <- predict(svm_model_nox, nox_df_train[,1:7])
  
  # test set predictions
  svm_co_pred_t <- predict(svm_model_co, co_df_test[, 1:7])
  svm_nox_pred_t <- predict(svm_model_nox, nox_df_test[, 1:7])
  
  message("Training set performance metrics: ")
  # performance metrics on training set
  message("CO R^2: ", rsquare(svm_co_pred, co_df_train[, 8]))
  message("CO MAE: ", mae(svm_co_pred, co_df_train[, 8]))
  
  message("NOX R^2: ", rsquare(svm_nox_pred, nox_df_train[, 8]))
  message("NOX MAE: ", mae(svm_nox_pred, nox_df_train[, 8]))
  
  message("Test set performance metrics: ")
  # performance metrics on test set
  message("CO R^2: ", rsquare(svm_co_pred_t, co_df_test[, 8]))
  message("CO MAE: ", mae(svm_co_pred_t, co_df_test[, 8]))
  
  message("NOX R^2: ", rsquare(svm_nox_pred_t, nox_df_test[, 8]))
  message("NOX MAE: ", mae(svm_nox_pred_t, nox_df_test[, 8]))
}