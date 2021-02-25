### -------------- Data processing -------------------------

### LOAD LIBRARIES - install with:
#install.packages(c("kohonen", "dummies", "ggplot2", "maptools", "sp", "reshape2", "rgeos", "normalr"))
#library(kohonen)
#library(dummies)
library(ggplot2)
#library(sp)
#library(maptools)
#library(reshape2)
#library(rgeos)

# Colour palette definition
pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2')

### DATA PREPARATION

#Set directory of raw data
setwd ("C:/Users/User/Desktop/INFO411/Group Project")

# Load the data into a data frame
data_raw_2011 <- read.csv("./pp_gas_emission/gt_2011.csv")  
data_raw_2012 <- read.csv("./pp_gas_emission/gt_2012.csv")  
data_raw_2013 <- read.csv("./pp_gas_emission/gt_2013.csv")  
data_raw_2014 <- read.csv("./pp_gas_emission/gt_2014.csv")  
data_raw_2015 <- read.csv("./pp_gas_emission/gt_2015.csv") 

#Load the data for train, validation and test sets
df_raw_train <-rbind(data_raw_2011, data_raw_2012)
df_raw_val = data_raw_2013
df_raw_test <- rbind(data_raw_2014, data_raw_2015)

#Get the Names of the Variables
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

#Plot boxplots ## TO remove if not needed
#boxplot(df_raw_train[1], main = "BoxPlot matrix", notch = TRUE, col = 1:4)


#Find the general information and correlationship of the variables
summary = summary(df_raw_train)
cor_table = cor(df_raw_train)
print(summary)
print(cor_table)

###Cleaning of data###
#Remove incomplete samples:
incompletes <- which(!complete.cases(df_raw_train))
if (length(incompletes) > 0){
  print(paste0("Removing ", length(incompletes), " data points that have missing values."))
  data <- data[-incompletes, ]
}else{
  print("No incomplete data")
}

#Standardisation of Data
df_scaled_train <- as.data.frame(scale(df_raw_train))
df_scaled_test <- as.data.frame(scale(df_raw_test))
df_scaled_val <- as.data.frame(scale(df_raw_val))


#Finalising train set for NOX
selected_col_NOX = c("AT","AH","NOX")  ##Enter all the values that we want to test for NOX
df_train <- df_scaled_train[selected_col_NOX]


#Finalising train set for CO
selected_col_CO = c("AT","AH","CO")   ##Enter all the values that we want to test for CO
df_train <- df_scaled_train[selected_col_CO]
