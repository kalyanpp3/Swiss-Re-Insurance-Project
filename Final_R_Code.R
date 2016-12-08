###########################################################################

# Clearing the variable from the environment
rm(list = ls())

#########################################################################
# Instruction :If there are any issues while loading the below package, # 
# please unhash the the packages installation step above for loading    #
# the respective package                                                #  
#########################################################################

#install.packages('caret')
library(caret)
#install.packages('sqldf')
library(sqldf)
#install.packages('doParallel')
library(doParallel)
#install.packages('readr')
library(readr)
#install.packages('ggplot2')
library(ggplot2)
#install.packages('randomForest')
library(randomForest)
#install.packages('rpart')
library(rpart)
#install.packages('rattle')
library(rattle)	
#install.packages('rpart.plot')
library(rpart.plot)
#install.packages('tree')
library(tree)
#install.packages('Hmisc')
library(Hmisc)
#install.packages('xgboost')
library(xgboost)
#install.packages('Matrix')
library(Matrix)
#install.packages('DMwR')
library(DMwR)
# install e1071 for SVM
library(e1071)

# setting the seed for reproducibility
set.seed(1000)

# Setting the directory while files are residing
setwd("/Users/jibybabu/Desktop/Work2/SwissRe/Latest-April/")
# read the "SwissRe_DataSet_Merged.csv" file
MergedDeals <- read.csv("SwissRe_DataSet_Merged.csv",na.strings=c(""," ","NA"))

####################################
# renaming the Column names in the #
# Excel sheet to customized names  #
####################################
colnames(MergedDeals)[which(names(MergedDeals) == "Status.L2.xxx")] = "Status_L2"
colnames(MergedDeals)[which(names(MergedDeals) == "Status.L3.xxx")] = "Status_L3"

colnames(MergedDeals)[which(names(MergedDeals) == "LCR.Economic.Activity.L1.xxx")] = "LCR_Economic_Activity_L1"
colnames(MergedDeals)[which(names(MergedDeals) == "LCR.Economic.Activity.L2.xxx")] = "LCR_Economic_Activity_L2"
colnames(MergedDeals)[which(names(MergedDeals) == "NAICS.Name")] = "NAICS_Name"
colnames(MergedDeals)[which(names(MergedDeals) == "NAICS.Name.Lvl.1")] = "NAICS_Name_Lvl_1"

colnames(MergedDeals)[which(names(MergedDeals) == "Expiration.Date.xxx")] = "Expiration_Date"
colnames(MergedDeals)[which(names(MergedDeals) == "Inception.Date.xxx")] = "Inception_Date"
colnames(MergedDeals)[which(names(MergedDeals) == "Creation_Date")] = "Creation_Date"

colnames(MergedDeals)[which(names(MergedDeals) == "Broker.Segment.L1")] = "Broker_Segment_L1"
colnames(MergedDeals)[which(names(MergedDeals) == "Broker.Segment.L2")] = "Broker_Segment_L2"
colnames(MergedDeals)[which(names(MergedDeals) == "Broker.Segment.L3")] = "Broker_Segment_L3"

colnames(MergedDeals)[which(names(MergedDeals) == "Insured.Region")] = "Insured_Region"
colnames(MergedDeals)[which(names(MergedDeals) == "Insured.Subregion")] = "Insured_SubRegion"
colnames(MergedDeals)[which(names(MergedDeals) == "Insured.State")] = "Insured_State"
colnames(MergedDeals)[which(names(MergedDeals) == "Broker.Region.L2")] = "Broker_SubRegion"
colnames(MergedDeals)[which(names(MergedDeals) == "Broker.Region.L4")] = "Broker_State"
colnames(MergedDeals)[which(names(MergedDeals) == "Insured.City")] = "Insured_City"

colnames(MergedDeals)[which(names(MergedDeals) == "Commission..")] = "Commission"
colnames(MergedDeals)[which(names(MergedDeals) == "Sum.Insured.100")] = "Sum_Insured_100"

#########################################################################################
# Setting up the dependent variable, 'target' column                                    #
# Logic : if Status_L2 contains "Not accepted by Swiss Re" or "Not accepted by Client", #  
# we are considering as Not Bound, 0, else Bound ,1                                     #
# Note : We are deleting the rows with "Pending Submission" and "Pending Offer" entries #
#########################################################################################
pendingOfferSubmission = ((MergedDeals$Status_L2 == "Pending submission") | (MergedDeals$Status_L2 == "Pending offer"))
MergedDeals = MergedDeals[!(pendingOfferSubmission),]
nrow(MergedDeals)
# Creating the target Column
MergedDeals$target = factor(ifelse((MergedDeals$Status_L2 == "Not accepted by Swiss Re" | MergedDeals$Status_L2 == "Not accepted by Client"),0,1))



#########################
#    Data Cleaning      #
#########################

# Converting the Broker_Segment_L3 as upper case and then converting into factor
MergedDeals$Broker_Segment_L3 =factor(toupper(MergedDeals$Broker_Segment_L3))
summary(MergedDeals$Broker_Segment_L3)

# Removing the Insured SubRegion 'Canada','Caribbean','Rest of Latin America'
nrow(MergedDeals)
MergedDeals <- MergedDeals[(MergedDeals$Insured_SubRegion %nin% c('Canada','Caribbean','Rest of Latin America')),]
nrow(MergedDeals)

# Removing the null InsuredSubRegion values
nrow(MergedDeals)
MergedDeals<- MergedDeals[!(is.na(MergedDeals$Insured_SubRegion)),]
nrow(MergedDeals)

# Removing the null LCR_Economic_Activity_L2
nrow(MergedDeals)
MergedDeals<- MergedDeals[!(is.na(MergedDeals$LCR_Economic_Activity_L2)),]
nrow(MergedDeals)

# Removing the null Broker.Region.L2
nrow(MergedDeals)
MergedDeals<- MergedDeals[!(is.na(MergedDeals$Broker_SubRegion)),]
nrow(MergedDeals)

## Missing Sum_Insured_100 value treatment ##
# Create the new column called isSumInsuredAvailable. 
# If Sum_Insured_100 exists, fill this column with 1, else 0
MergedDeals$isSumInsuredAvailable = factor(ifelse(is.na(MergedDeals$Sum_Insured_100),0,1))
# Finding the index of missing Sum_Insured_100.
missing <- is.na(MergedDeals$Sum_Insured_100)
# Fill the above figured out indexes with -999
MergedDeals$Sum_Insured_100[missing] <- -999

## Missing Commission value treatment ##
# Create the new column called isCommissionAvailable. 
# If Sum_Insured_100 exists, fill this column with 1, else 0
MergedDeals$isCommissionAvailable = factor(ifelse(is.na(MergedDeals$Commission),0,1))
# Finding the index of missing Sum_Insured_100.
missing <- is.na(MergedDeals$Commission)
# Fill the above figured out indexes with -111
MergedDeals$Commission[missing] <- -111

# Removing the null Insured_City values
nrow(MergedDeals)
MergedDeals<- MergedDeals[!(is.na(MergedDeals$Insured_City)),]
nrow(MergedDeals)

# Removing the null Broker_Segment_L3 values
nrow(MergedDeals)
MergedDeals<- MergedDeals[!(is.na(MergedDeals$Broker_Segment_L3)),]
nrow(MergedDeals)

# Removing the rows with Unknown Broker_Segment_L3
nrow(MergedDeals)
MergedDeals <- MergedDeals[(MergedDeals$Broker_Segment_L3 %nin% c('UNKNOWN')),]
nrow(MergedDeals)

# Removing the Insured_region other than NorthAmerica
nrow(MergedDeals)
MergedDeals<- MergedDeals[(MergedDeals$Insured_Region=='North America'),]
nrow(MergedDeals)


# Removing the 4706 NA values where we don't have any clue how to fill it.
nrow(MergedDeals)
MergedDeals<- MergedDeals[!(is.na(MergedDeals$NAICS_Name_Lvl_1)),]
nrow(MergedDeals)



getDataFrameAfterRemoveUniqueElementsInColumn <- function (df, column_name, uniqueCount) {
  df = MergedDeals
  sqldf("SELECT * from $(df)")
  
  return(1)
  df <- sqldf("SELECT * FROM @df WHERE column_name
              IN (SELECT column_name FROM consent GROUP BY column_name HAVING COUNT(*) > uniqueCount) 
              ORDER BY column_name")
  return (df)
}
getDataFrameAfterRemoveUniqueElementsInColumn(MergedDeals, "Insured_State",2)

# Removing the Insured_State which are coming less than 2 appearence
MergedDeals <- sqldf("SELECT * FROM MergedDeals WHERE Insured_State
                     IN (SELECT Insured_State FROM MergedDeals GROUP BY Insured_State HAVING COUNT(*) > 2) 
                     ORDER BY Insured_State")

# Removing the Broker_State which are coming less than 2 appearence
MergedDeals <- sqldf("SELECT * FROM MergedDeals WHERE Broker_State
                     IN (SELECT Broker_State FROM MergedDeals GROUP BY Broker_State HAVING COUNT(*) > 4) 
                     ORDER BY Broker_State")

# Delete the NAICS_Name_Lvl_1 entries which are coming less than 4 appearence
MergedDeals <- sqldf("SELECT * FROM MergedDeals WHERE NAICS_Name_Lvl_1
                     IN (SELECT NAICS_Name_Lvl_1 FROM MergedDeals GROUP BY NAICS_Name_Lvl_1 HAVING COUNT(*) > 4) 
                     ORDER BY NAICS_Name_Lvl_1")

# Delete the Broker_Segment_L3 entries which are coming less than 4 appearence
MergedDeals <- sqldf("SELECT * FROM MergedDeals WHERE Broker_Segment_L3
                     IN (SELECT Broker_Segment_L3 FROM MergedDeals GROUP BY Broker_Segment_L3 HAVING COUNT(*) > 4) 
                     ORDER BY Broker_Segment_L3")



################################
##    Feature Engineering     ##
################################

#### Dummy variable Creation: ###
# Referrence: 
# http://stackoverflow.com/questions/11952706/generate-a-dummy-variable-in-r
CreateDummies <- function (df,colName) {
  colName = factor(colName)
  colName_dummies <- as.data.frame(model.matrix(~colName))
  colName_dummies <- colName_dummies[-which(colnames(colName_dummies)=="(Intercept)")]
  colName_dummies <- as.data.frame(lapply(colName_dummies, factor))
  cat("Newly Created Dummies are following","\n")
  names(colName_dummies)
  df <- cbind(MergedDeals,colName_dummies)
  return (df)
}

# Create Dummies for NAICS_Name_Lvl_1 
MergedDeals <- CreateDummies(MergedDeals,MergedDeals$NAICS_Name_Lvl_1)

# Create Dummies for Broker_Segment_L3 
MergedDeals <- CreateDummies(MergedDeals,MergedDeals$Broker_Segment_L3)

# Creating the column for getting the number of days between Inception and Expiry dates
# Naming this column as PolicyDuration
MergedDeals$PolicyDuration <-
  as.numeric(as.Date(as.character(MergedDeals$Expiration_Date), format="%d.%m.%Y")-
               as.Date(as.character(MergedDeals$Inception_Date), format="%d.%m.%Y"))

# Function for formating the date format 
# from "09.06.2013 02:05:42 PM" to %d.%m.%Y
replace = function(x) {
  y = sub("([0-9][0-9].[0-9][0-9].[0-9]*) .*","\\1",x)
  return (trimws(y))
}

# Creating the new column called Inception_Creation which will give the 
# difference between the policy start period and deal creation period
MergedDeals<- MergedDeals[!(is.na(MergedDeals$Creation_Date)),]
MergedDeals$Inception_Creation = as.numeric(as.Date(as.character(MergedDeals$Inception_Date), format="%d.%m.%Y") - 
                                              as.Date(as.character(sapply(MergedDeals$Creation_Date, replace)),format="%d.%m.%Y"))

# Create a new Column isBrokerInsuredStateSame;
# If Broker and Insured States are same, fill the isBrokerInsuredStateSame with 1, else 0
MergedDeals$isBrokerInsuredStateSame = factor(ifelse(as.character(MergedDeals$Broker_State) == as.character(MergedDeals$Insured_State),1,0))

# Create a new Column isBrokerInsuredCitySame;
# If Broker and Insured Cities are same, fill the isBrokerInsuredCitySame with 1, else 0
MergedDeals$isBrokerInsuredCitySame = factor(ifelse(as.character(MergedDeals$Insured_City) == as.character(MergedDeals$Broker_City),1,0))


#########################################
# Preparing the data set  for Modeling #  
#########################################

# Removing all the columns in the "SwissRe_DataSet_Merged.csv" data set till Expiration_Date column 
MergedDeals <- MergedDeals[-c(1:which( colnames(MergedDeals)=="Expiration_Date"))]
names(MergedDeals)

# Removing if any missing values exists all together
# we figured out that 2 rows exists, so removing that
nrow(MergedDeals)
any(is.na(MergedDeals))
MergedDeals <- na.omit(MergedDeals)
nrow(MergedDeals)

# Moving the 'target' column to the end of the SwissRe_DataSet_Merged.csv" data set
target <- MergedDeals$target
MergedDeals<- MergedDeals[-which( colnames(MergedDeals)=="target")]
MergedDeals <- cbind(MergedDeals,target)

# Converting the Broker_SubRegion variable to factor data type
MergedDeals$Broker_SubRegion = as.factor(MergedDeals$Broker_SubRegion)

# Converting the Insured_SubRegion variable to factor data type
MergedDeals$Insured_SubRegion = as.factor(MergedDeals$Insured_SubRegion) 

# Removing the Broker_Segment_L3 Column not using in the model
MergedDeals <- MergedDeals[-which( colnames(MergedDeals)=="Broker_Segment_L3")]

# Removing the Broker_Segment_L2 Column not using in the model
MergedDeals <- MergedDeals[-which( colnames(MergedDeals)=="Broker_Segment_L2")]

# Removing the Insured_State Column not using in the model
MergedDeals <- MergedDeals[-which( colnames(MergedDeals)=="Insured_State")]

# Removing the Broker_State Column not using in the model
MergedDeals <- MergedDeals[-which( colnames(MergedDeals)=="Broker_State")]

# Removing the BNAICS_Name_Lvl_1 Column not using in the model
MergedDeals <- MergedDeals[-which( colnames(MergedDeals)=="NAICS_Name_Lvl_1")]

# Removing the Insured_City Column not using in the model
MergedDeals <- MergedDeals[-which( colnames(MergedDeals)=="Insured_City")]

# Removing the Broker_City Column not using in the model
MergedDeals <- MergedDeals[-which( colnames(MergedDeals)=="Broker_City")]

# Removing the Broker_Segment_L1 Column not using in the model
MergedDeals <- MergedDeals[-which( colnames(MergedDeals)=="Broker_Segment_L1")]
names(MergedDeals)

#########################################
# Creating Training and Validating test #
# Technique : 5-fold cross validation   #
#########################################
summary(MergedDeals$target)

indexr<-createFolds(MergedDeals$target,k=5, list = FALSE)
train<-rbind(MergedDeals[indexr==1,],MergedDeals[indexr==2,],MergedDeals[indexr==3,],MergedDeals[indexr==4,])
val<-MergedDeals[indexr==5,]

summary(train$target)
summary(val$target)

######################################################
# So the variables, we are going to use in the model #
######################################################
names(train)
#View(train)

###############################################################################################################
#  OverSampling
# Since the data is highly imbalanced, 95% 0's and 5% 1's , We need to do overSampling
# Technique Used : Synthetic Minority Over-sampling 
# References:
# http://machinelearningmastery.com/tactics-to-combat-imbalanced-classes-in-your-machine-learning-dataset/
# https://chandramanitiwary.wordpress.com/2013/09/10/class-imbalance-smote-and-r/
###############################################################################################################
summary(train$target)
train<-SMOTE(target ~ . , k=10, train,perc.over = 1500,perc.under=140)
summary(train$target)


######################
# Model Building     #
######################

# Creating the Confusion Matrix function to                    
# calculate the total Accuray, Sensitivity(True Positive Rate),
# Specificity(True Negative Rate) and MissClassification Rate  
# Input to this function will be the confusion matrix.

processConfusionMatrix <- function (confusionMatrix1) {
  # Computing the Accuracy of the Prediction
  Accuracy = (confusionMatrix1[1,1]+confusionMatrix1[2,2])/(confusionMatrix1[1,1]+confusionMatrix1[1,2]+confusionMatrix1[2,1]+confusionMatrix1[2,2])
  cat(" Total Accuracy = ", Accuracy,"\n")
  
  # Computing the Recall/Sensitivity/TruePositive of the Prediction
  # TPR = (TP)/(TP+FN)
  Sensitivity = (confusionMatrix1[2,2])/(confusionMatrix1[2,2]+confusionMatrix1[1,2])
  cat("Sensitivity/TPR = ", Sensitivity,"\n")
  
  # Computing the Specificity
  # TNR = (TN) / (TN + FP) 
  Specificity = (confusionMatrix1[1,1])/(confusionMatrix1[1,1]+confusionMatrix1[2,1])
  cat("Specificity/TNR = ", Specificity,"\n")
  
  # Computing the Miss ClassificationRatio
  # Missclassification = 1- [(TP)/(TP+FN)] = FN/(FN+TP)
  MissClassificationRatio = (confusionMatrix1[1,2])/(confusionMatrix1[1,2]+confusionMatrix1[2,2])
  cat("Miss Classification Ratio = ", MissClassificationRatio,"\n")
}

fineTuneModel <- function (predictionValues,validationSet,probability) {
  p <- predictionValues
  validationSet$p <- p
  # Getting the Logistic Regression Metrics
  #predictionValues[predictionValues$target==0,"p"]
  summary(factor(val$target))
  hist(validationSet[validationSet$target==1,"p"],xlab="probability",ylab="No. of 1's", main="Predicted Probability Vs 1's in Validation Set", col="green")
  hist(validationSet[validationSet$target==0,"p"],xlab="probability",ylab="No. of 0's", main="Predicted Probability Vs 0's in Validation Set", col="blue")
  prediction = ifelse(p<probability, 0, 1)
  return(prediction)
}

convertCharFactsToInt = function (df) {
  cat("assuming text variables are categorical & replacing them with numeric ids\n")
  cat("re-factor categorical vars & replacing them with numeric ids\n")
  
  featureNamesSet <- names(df)
  for (eachFeature in featureNamesSet) {
    if (class(df[[eachFeature]])=="character" || class(df[[eachFeature]])=="factor") {
      df[[eachFeature]] <- as.integer(factor(df[[eachFeature]]))
    }
  }
  return (df)
}

###############################
# Model 1 : Logistic Regression
###############################
logitRegModel <- glm(train$target ~ . , data = train, family = "binomial")
summary(logitRegModel)

sink("Logistic_Regression_summary.txt")
summary(logitRegModel)
sink()

logitRegModelPrediction <- predict(logitRegModel, newdata = val, type = "response")
TunedLogitRegressionPredictionValues <- fineTuneModel(logitRegModelPrediction,val,.1)
confusionMatrx = table(TunedLogitRegressionPredictionValues,val$target)
confusionMatrx
processConfusionMatrix(confusionMatrx)

###########################################
# Model 2:    Decision Tree Modeling      #
###########################################
decisionTreeFit <- rpart(train$target ~ ., data=train, method="class")
decisionTreePrediction <- predict(decisionTreeFit, val, type="class")

confusionMatrx2 = table(decisionTreePrediction, val$target)
confusionMatrx2
processConfusionMatrix(confusionMatrx2)
decisionTreeFit$variable.importance

prp(decisionTreeFit)
png("123.png", res=80, height=800, width=1600) 
fancyRpartPlot(decisionTreeFit, palettes=c("Greys", "Oranges"))
dev.off()

#######################################
# Model 3:  Random Forest Modeling    #
######################################
randomForestfit <- randomForest(train$target~., data=train, importance=TRUE, ntree=500)
randomForestfitPrediction <- predict(randomForestfit, val, type="prob")[,2]

TunedrandomForestPredictionValues <- fineTuneModel(randomForestfitPrediction,val,.20)

confusionMatrx3 = table(TunedrandomForestPredictionValues,val$target)
confusionMatrx3
processConfusionMatrix(confusionMatrx3)
randomForestfit$importance

###################################
# Model 4:         XgBoost        #
###################################
# Convert the factors and charectors of the training set into integer
train <- convertCharFactsToInt(train)
# Convert the factors and charectors of the validation set into integer
val <- convertCharFactsToInt(val)

# reCatogorizing the target values
train$target = train$target - 1
val$target = val$target - 1

# creating the matrix for training the model 
xgtrain <- xgb.DMatrix(as.matrix(train[,!colnames(train) %in% c('target')]), label = as.numeric(train$target))
# creating the matrix for validating the model
xgtest <- xgb.DMatrix(as.matrix(val[,!colnames(val) %in% c('target')])) 

param0 <- list(
  # some generic, non specific params
  "objective"  = "reg:logistic"
  , "booster"    = "gbtree"
  , "eval_metric" = "logloss"
  , "eta" = 0.01
  , "max_depth" = 8
  , "subsample" = 0.70
  , "colsample_bytree" = 0.90
  , "min_child_weight" = 1
  , "num_parallel_tree"= 1
  , "gamma" = 5
)

watchlist <- list('train' = xgtrain)

xgBoostModelfit = xgb.train(
  nrounds = 400
  , params = param0
  , data = xgtrain
  , watchlist = watchlist
  , print.every.n = 20
  , nthread = 4
)

xgBoostModelfitPrediction <- predict(xgBoostModelfit, xgtest)
TunedxgBoostPredictionValues <- fineTuneModel(xgBoostModelfitPrediction,val,.10)

confusionMatrx4 = table(TunedxgBoostPredictionValues,val$target)
confusionMatrx4
processConfusionMatrix(confusionMatrx4)

imp <- xgb.importance(model=xgBoostModelfit)
imp$Feature<-as.integer(imp$Feature)+1

MergedDeals$RandomForestPrediction <- TunedxgBoostPredictionValues
MergedDeals$xgBoostPrediction <- TunedrandomForestPredictionValues
MergedDeals$logisticPrediction <- TunedLogitRegressionPredictionValues

write.csv(MergedDeals, "SwissRe_Dataset_AfterModeling.csv", row.names=F, quote=F)


