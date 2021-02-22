library(gbm)

# GBM parameters etc.
# https://www.rdocumentation.org/packages/gbm/versions/2.1.8/topics/gbm

evaluation_gbm <- function(df_train, df_validate, df_test){
  # settings GBM parameters in search grid
  gbm_grid <- expand.grid(
    shrinkage = c(0.1, 0.01, 0.001), # learning rate
    interaction.depth = c(3,5,7), # max depth of each tree
    n.trees = c(200, 400, 600),
    n.minobsinnode = c(5,10,15), # minimum no. of obs
    bag.fraction = c(0.5, 0.75, 1),
    r2 = 0,
    mae = 0
  )
  
  # tuning for best parameters
  for(i in 1:nrow(gbm_grid)){
    set.seed(321)
    
    gbm.tune <- gbm(
      formula = CO ~ .,
      distribution = "gaussian",
      data = df_validate[,-11],
      n.trees = gbm_grid$n.trees[i],
      interaction.depth = gbm_grid$interaction.depth[i],
      shrinkage = gbm_grid$interaction.depth[i],
      n.minobsinnode = gbm_grid$n.minobsinnode[i],
      bag.fraction = gbm_grid$bag.fraction[i],
      verbose = FALSE
    )
    
    pred_val <- predict(gbm.tune, df_validate[,-11])
    gbm_grid$r2[i] <- rsquare(pred_val, df_validate[,10])
    gbm_grid$mae[i] <- mae(pred_val, df_validate[,10])
  }
  
  # training models
  system.time(
    co_gbm <- gbm(
      formula = CO ~ ., distribution = "gaussian", data = df_train[, -11],
      n.trees = 200, interaction.depth = 3, shrinkage = 0.001,
      n.minobsinnode = 15, bag.fraction = 1,
      verbose = FALSE
    )
  )
  
  system.time(
    nox_gbm <- gbm(
      formula = NOX ~ ., distribution = "gaussian", data = df_train[, -10],
      n.trees = 200, interaction.depth = 3, shrinkage = 0.001,
      n.minobsinnode = 15, bag.fraction = 1,
      verbose = FALSE
    )
  )
  
  # training set predictions
  co_gbm_pred <- predict(co_gbm, df_train[,1:9])
  nox_gbm_pred <- predict(nox_gbm, df_train[,1:9])
  
  # test set predictions
  co_gbm_pred_t <- predict(co_gbm, df_test[,1:9])
  nox_gbm_pred_t <- predict(nox_gbm, df_test[,1:9])
  
  message("Training set performance metrics: ")
  # performance metrics for training set
  message("CO R^2: ", rsquare(co_gbm_pred, df_train[,10])[1])
  message("CO MAE: ", mae(co_gbm_pred, df_train[,10]))
  
  message("NOX R^2: ", rsquare(nox_gbm_pred, df_train[,11])[1])
  message("NOX MAE: ", mae(nox_gbm_pred, df_train[,11]))
  
  
  message("Test set performance metrics: ")
  # performance metrics for test set
  message("CO R^2: ", rsquare(co_gbm_pred_t, df_test[,10])[1])
  message("CO MAE: ", mae(co_gbm_pred_t, df_test[,10]))
  
  message("NOX R^2: ", rsquare(nox_gbm_pred_t, df_test[,11])[1])
  message("NOX MAE: ", mae(nox_gbm_pred_t, df_test[,11]))
}
