###########################################################################################
## feature selection ######################################################################
###########################################################################################
setwd("/Volumes/Data Science/Google Drive/data_science_competition/melbourne_datathon/Melbourne_Datathon_Kaggle/")
rm(list = ls()); gc()
require(data.table)
require(plyr)
require(dplyr)
require(caret)
require(caTools)
load("../Datathon_Full_Dataset/trainData_less.RData")
load("../Datathon_Full_Dataset/valid1Data_less.RData")
load("../Datathon_Full_Dataset/valid2Data_less.RData")
load("../Datathon_Full_Dataset/testData_less.RData")
# dtTestFeatures <- fread("../data_files/semi_and_final_features.csv")
dtSampleSubmit <- fread("../data_files/sample_submission_bet_size.csv")
dtSampleSubmit[, Prediction := NULL]
setnames(dtSampleSubmit, "Account_ID")

###########################################################################################
## train, valid, and test #################################################################
###########################################################################################
require(xgboost)
require(Ckmeans.1d.dp)
x.train <- model.matrix(PRED ~., dt.train[, !c("ACCOUNT_ID"), with = F])[, -1]
y.train <- dt.train$PRED
dmx.train <- xgb.DMatrix(data =  x.train, label = y.train)

x.valid1 <- model.matrix(PRED ~., dt.valid1[, !c("ACCOUNT_ID"), with = F])[, -1]
y.valid1 <- ifelse(as.integer(dt.valid1$PRED) == 1, 0, 1)
dmx.valid1 <- xgb.DMatrix(data =  x.valid1, label = y.valid1)

x.valid2 <- model.matrix(PRED ~., dt.valid2[, !c("ACCOUNT_ID"), with = F])[, -1]
y.valid2 <- ifelse(as.integer(dt.valid2$PRED) == 1, 0, 1)
dmx.valid2 <- xgb.DMatrix(data =  x.valid2, label = y.valid2)

x.test <- model.matrix(~., dt.test[, !c("ACCOUNT_ID"), with = F])[, -1]

###########################################################################################
## randome forest #########################################################################
###########################################################################################
rfControl <- rfeControl(functions = rfFuncs
                        , method = "cv"
                        , number = 70
                        , verbose = T)

rfControl$functions$summary <- twoClassSummary()
subsets <- seq(10, 70, by = 10)

rfe.out <- rfe(x = x.train
               , y = y.train
               , sizes = subsets
               , metric = "ROC"
               , maximize = T
               , rfeControl = rfControl)










