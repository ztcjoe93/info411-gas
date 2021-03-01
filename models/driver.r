## main driver file for executing code and training/evaluating respective models

setwd(sub("(.*/models/).*", "\\1", rstudioapi::getActiveDocumentContext()$path))
source("df_utilities.r")
source("svm.r")
source("rforest.r")
source("mlp.r")
source("gbm.r")

# call to parse datasets into dataframes
raw_dataframes = getDf(israw=TRUE)
dataframes = getDf()

# raw data w/o feature selection
df_raw_train = as.data.frame(raw_dataframes[1])
df_raw_validate = as.data.frame(raw_dataframes[2])
df_raw_test = as.data.frame(raw_dataframes[3])

################################# data pre-processing #######################################
library(ggplot2)

# Colour palette definition
pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2')

df_colNames = colnames(df_raw_train)

###Plot ScatterPlot against CO###
ggplot(df_raw_train, aes(x=AT, y=CO)) + geom_point()+ geom_smooth(method=lm)
ggplot(df_raw_train, aes(x=AP, y=CO)) + geom_point()+ geom_smooth(method=lm)
ggplot(df_raw_train, aes(x=AH, y=CO)) + geom_point()+ geom_smooth(method=lm)
ggplot(df_raw_train, aes(x=AFDP, y=CO)) + geom_point()+ geom_smooth(method=lm)
ggplot(df_raw_train, aes(x=GTEP, y=CO)) + geom_point()+ geom_smooth(method=lm)
ggplot(df_raw_train, aes(x=TIT, y=CO)) + geom_point()+ geom_smooth(method=lm)
ggplot(df_raw_train, aes(x=TAT, y=CO)) + geom_point()+ geom_smooth(method=lm)
ggplot(df_raw_train, aes(x=TEY, y=CO)) + geom_point()+ geom_smooth(method=lm)
ggplot(df_raw_train, aes(x=CDP, y=CO)) + geom_point()+ geom_smooth(method=lm)

###Plot ScatterPlot against NOX###
ggplot(df_raw_train, aes(x=AT, y=NOX)) + geom_point()+ geom_smooth(method=lm)
ggplot(df_raw_train, aes(x=AP, y=NOX)) + geom_point()+ geom_smooth(method=lm)
ggplot(df_raw_train, aes(x=AH, y=NOX)) + geom_point()+ geom_smooth(method=lm)
ggplot(df_raw_train, aes(x=AFDP, y=NOX)) + geom_point()+ geom_smooth(method=lm)
ggplot(df_raw_train, aes(x=GTEP, y=NOX)) + geom_point()+ geom_smooth(method=lm)
ggplot(df_raw_train, aes(x=TIT, y=NOX)) + geom_point()+ geom_smooth(method=lm)
ggplot(df_raw_train, aes(x=TAT, y=NOX)) + geom_point()+ geom_smooth(method=lm)
ggplot(df_raw_train, aes(x=TEY, y=NOX)) + geom_point()+ geom_smooth(method=lm)
ggplot(df_raw_train, aes(x=CDP, y=NOX)) + geom_point()+ geom_smooth(method=lm)

#Plot Histogram with Density Overlay
#--------AT------------#
ggplot(df_raw_train, aes(x = AT), color = )+ 
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white", bins = 30)+
  geom_density(alpha = .2, fill ="blue") + xlab("Ambient Temperature") + geom_rug()
#--------AP------------#
ggplot(df_raw_train, aes(x = AP), color = )+ 
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white", bins = 30)+
  geom_density(alpha = .2, fill ="blue")+ xlab("Ambient Pressure") + geom_rug()
#--------AH------------#
ggplot(df_raw_train, aes(x = AH), color = )+ 
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white", bins = 30)+
  geom_density(alpha = .2, fill ="blue")+ xlab("Ambient Humidity") + geom_rug()
#--------AFDP------------#
ggplot(df_raw_train, aes(x = AFDP), color = )+ 
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white", bins = 30)+
  geom_density(alpha = .2, fill ="blue")+ xlab("Air Filter Difference Pressure") + geom_rug()
#--------GTEP------------#
ggplot(df_raw_train, aes(x = GTEP), color = )+ 
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white", bins = 30)+
  geom_density(alpha = .2, fill ="blue")+ xlab("Gas Turbine Exhaust Pressure") + geom_rug()
#--------TIT------------#
ggplot(df_raw_train, aes(x = TIT), color = )+ 
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white", bins = 30)+
  geom_density(alpha = .2, fill ="blue")+ xlab("Turbine Inlet Temperature") + geom_rug()
#--------TAT------------#
ggplot(df_raw_train, aes(x = TAT), color = )+ 
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white", bins = 50)+
  geom_density(alpha = .2, fill ="blue")+ xlab("Turbine After Temperature") + geom_rug()
#--------TEY------------#
ggplot(df_raw_train, aes(x = TEY), color = )+ 
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white", bins = 30)+
  geom_density(alpha = .2, fill ="blue")+ xlab("Turbine Energy Yield") + geom_rug()
#--------CDP------------#
ggplot(df_raw_train, aes(x = CDP), color = )+ 
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white", bins = 30)+
  geom_density(alpha = .2, fill ="blue")+ xlab("Compressor Discharge Pressure") + geom_rug()
#--------CO------------#
ggplot(df_raw_train, aes(x = CO), color = )+ 
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white", bins = 50)+
  geom_density(alpha = .2, fill ="blue")+ xlab("Carbon Monoxide") + geom_rug()
#--------NOX------------#
ggplot(df_raw_train, aes(x = NOX), color = )+ 
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white", bins = 30)+
  geom_density(alpha = .2, fill ="blue")+ xlab("Nitrogen Oxides") + geom_rug()

#Find the general information and correlationship of the variables
summary = summary(df_raw_train)
cor_table = cor(df_raw_train)
print(summary)
print(cor_table)

#Remove incomplete samples:
incompletes <- which(!complete.cases(df_raw_train))
if (length(incompletes) > 0){
  print(paste0("Removing ", length(incompletes), " data points that have missing values."))
  data <- data[-incompletes, ]
}else{
  print("No incomplete data")
}

# standardization implemented within df_utilities.r, feature selection integrated within prediction methods

##############################################################################################################

# standardized data

df_train = as.data.frame(dataframes[1])
df_validate = as.data.frame(dataframes[2])
df_test = as.data.frame(dataframes[3])

df_train = as.data.frame()

# call to respective model for hyperparam tuning, training and evaluation
evaluation_svm(df_train, df_validate, df_test)
evaluation_rf(df_train, df_validate, df_test)
evaluation_mlp(df_train, df_validate, df_test)
evaluation_gbm(df_train, df_validate, df_test)
evaluation_dt(df_train, df_validate, df_test)
evaluation_lr(df_train, df_validate, df_test)

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