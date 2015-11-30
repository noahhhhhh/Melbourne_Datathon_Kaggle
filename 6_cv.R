###########################################################################################
## Transform, Training and Testing sets ###################################################
###########################################################################################
setwd("/Volumes/Data Science/Google Drive/data_science_competition/melbourne_datathon/Melbourne_Datathon_Kaggle/")
rm(list = ls()); gc()
require(data.table)
require(dplyr)
require(caret)
require(caTools)
load("../Datathon_Full_Dataset/trainData.RData")
load("../Datathon_Full_Dataset/valid1Data.RData")
load("../Datathon_Full_Dataset/valid2Data.RData")
load("../Datathon_Full_Dataset/testData.RData")
# dtTestFeatures <- fread("../data_files/semi_and_final_features.csv")
dtSampleSubmit <- fread("../data_files/sample_submission_bet_size.csv")
dtSampleSubmit[, Prediction := NULL]
setnames(dtSampleSubmit, "Account_ID")
#####################################################################
## 1. cv the xgboost ################################################
#####################################################################
##############################
## 1.1 train, valid, and test transform
##############################
require(xgboost)
require(Ckmeans.1d.dp)
x.train <- model.matrix(PRED ~., dt.train[, !c("ACCOUNT_ID"), with = F])[, -1]
y.train <- ifelse(as.integer(dt.train$PRED) == 1, 0, 1)
dmx.train <- xgb.DMatrix(data =  x.train, label = y.train)

x.valid1 <- model.matrix(PRED ~., dt.valid1[, !c("ACCOUNT_ID"), with = F])[, -1]
y.valid1 <- ifelse(as.integer(dt.valid1$PRED) == 1, 0, 1)
dmx.valid1 <- xgb.DMatrix(data =  x.valid1, label = y.valid1)

x.valid2 <- model.matrix(PRED ~., dt.valid2[, !c("ACCOUNT_ID"), with = F])[, -1]
y.valid2 <- ifelse(as.integer(dt.valid2$PRED) == 1, 0, 1)
dmx.valid2 <- xgb.DMatrix(data =  x.valid2, label = y.valid2)

x.test <- model.matrix(~., dt.test[, !c("ACCOUNT_ID"), with = F])[, -1]

##############################
## 1.2 cv ####################
##############################
params <- list(nthread = 8
               , objective = "binary:logistic"
               , eval_metric = "auc"
               , eta = .025
               , subsample = .8
               , colsample_bytree = .3)
nrounds <- 
cv.xgboost <- xgb.cv(params = params
                     , data = dmx.train
                     , nrounds = nrounds
                     , nfold = 5
                     , showsd = T
                     , verbose = T)














