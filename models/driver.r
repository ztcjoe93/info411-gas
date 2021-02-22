## main driver file for executing code and training/evaluating respective models

setwd(sub("(.*/models/).*", "\\1", rstudioapi::getActiveDocumentContext()$path))
source("df_utilities.r")
source("svm.r")
source("rforest.r")
source("mlp.r")
source("gbm.r")

# call to parse datasets into dataframes
dataframes = getDf()
df_train = as.data.frame(dataframes[1])
df_validate = as.data.frame(dataframes[2])
df_test = as.data.frame(dataframes[3])

# call to respective model for hyperparam tuning, training and evaluation
evaluation_svm(df_train, df_validate, df_test)
evaluation_rf(df_train, df_validate, df_test)
evaluation_mlp(df_train, df_validate, df_test)
evaluation_gbm(df_train, df_validate, df_test)