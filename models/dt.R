library(rpart)

evaluation_dt <- function(df_train, df_validate, df_test){
  
  #tuning decision tree for CO
  co_droplist = c("AH", "AP", "NOX")
  
  system.time(
    dt_grid <- tune.rpart(
      formula = CO ~ .,
      data = df_validate[,!(names(df_validate) %in% co_droplist)],
      cp = c(0.1, 0.01, 0.001)
    )
  )
  dt_grid
  
  system.time(
    dt_grid <- tune.rpart(
      formula = CO ~ .,
      data = df_validate[,!(names(df_validate) %in% co_droplist)],
      maxdepth = c(5, 10, 15) 
    )
  )
  dt_grid
  
  system.time(
    dt_grid <- tune.rpart(
      formula = CO ~ .,
      data = df_validate[,!(names(df_validate) %in% co_droplist)],
      minsplit = c(3, 5, 10, 20)
      
    )
  )
  dt_grid
  ###
  
  #tuning decision tree for NOX
  nox_droplist = c("AH", "AFDP", "CO")
  
  system.time(
    dt_grid <- tune.rpart(
      formula = NOX ~ .,
      data = df_validate[,!(names(df_validate) %in% nox_droplist)],
      cp = c(0.1, 0.01, 0.001)
    )
  )
  dt_grid
  
  system.time(
    dt_grid <- tune.rpart(
      formula = NOX ~ .,
      data = df_validate[,!(names(df_validate) %in% nox_droplist)],
      maxdepth = c(5, 10, 15)
    )
  )
  dt_grid
  
  system.time(
    dt_grid <- tune.rpart(
      formula = NOX ~ .,
      data = df_validate[,!(names(df_validate) %in% nox_droplist)],
      minsplit = c(3, 5, 10, 20)
    )
  )
  dt_grid
  ###
  
  
  # train model with best parameters
  dt_co <- rpart(CO ~ ., data = df_train[,!(names(df_train) %in% co_droplist)], 
                 cp = 0.001, maxdepth = 5, minsplit = 20, method = "anova")
  
  dt_nox <- rpart(NOX ~ .,data = df_train[,!(names(df_train) %in% nox_droplist)], 
                  cp = 0.001, maxdepth = 10, minsplit = 3, method = "anova")
  
  # predictions for training set
  dt_co_pred <- predict(dt_co, df_train[,!(names(df_train) %in% co_droplist)])
  dt_nox_pred <- predict(dt_nox, df_train[,!(names(df_train) %in% nox_droplist)])
  
  # predictions for test set
  dt_co_pred_t <- predict(dt_co, df_test[,!(names(df_test) %in% co_droplist)])
  dt_nox_pred_t <- predict(dt_nox, df_test[,!(names(df_test) %in% nox_droplist)])
  
  
  message("Training set performance metrics: ")
  # performance metrics on training set
  message("CO R^2: ", rsquare(dt_co_pred, df_train[, 10]))
  message("CO MAE: ", mae(dt_co_pred, df_train[, 10]))
  
  message("NOX R^2: ", rsquare(dt_nox_pred, df_train[, 11]))
  message("NOX MAE: ", mae(dt_nox_pred, df_train[, 11]))
  
  
  message("Test set performance metrics: ")
  # performance metrics on test set
  message("CO R^2: ", rsquare(dt_co_pred_t, df_test[, 10]))
  message("CO MAE: ", mae(dt_co_pred_t, df_test[, 10]))
  
  message("NOX R^2: ", rsquare(dt_nox_pred_t, df_test[, 11]))
  message("NOX MAE: ", mae(dt_nox_pred_t, df_test[, 11]))
}