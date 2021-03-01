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

################################# Visualization of variable importance #######################################
library(caret)
set.seed(100)

correlationMatrix <- cor(df_train[,1:9])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly correlated (ideally >0.85)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.85, name=TRUE)
# print indexes of highly correlated attributes
print(highlyCorrelated)

# Train an rpart model and compute variable importance for 'NOX'.
rPartMod <- train(NOX ~ ., data=na.omit(df_train[,-10]), method="rpart")
rpartImp <- varImp(rPartMod) # varlmp() is used to determine feature importance
print(rpartImp)
plot(rpartImp, top = 9, main='Variable Importance for NOX')
summary(rPartMod)

# Train an rpart model and compute variable importance for 'CO'.
rPartMod <- train(CO ~ ., data=na.omit(df_train[,-11]), method="rpart")
rpartImp <- varImp(rPartMod) # varlmp() is used to determine feature importance
print(rpartImp)
plot(rpartImp, top = 9, main='Variable Importance for CO')
summary(rPartMod)
##############################################################################################################