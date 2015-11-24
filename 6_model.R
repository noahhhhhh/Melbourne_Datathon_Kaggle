###########################################################################################
## modelling ##############################################################################
###########################################################################################
setwd("/Volumes/Data Science/Google Drive/data_science_competition/melbourne_datathon/Melbourne_Datathon_Kaggle/")
rm(list = ls()); gc()
require(data.table)
require(dplyr)
require(caret)
require(caTools)
load("../Datathon_Full_Dataset/transformedData.RData")
# dtTestFeatures <- fread("../data_files/semi_and_final_features.csv")
dtSampleSubmit <- fread("../data_files/sample_submission_bet_size.csv")
dtSampleSubmit[, Prediction := NULL]
setnames(dtSampleSubmit, "Account_ID")

#####################################################################
## 1. try using all the data set except the test set to train a rf ## does not perform well (0.56465)
#####################################################################
##############################
## 1.1 train and test ########
##############################
dt.train <- dt.3in1[!UNIT %in% c("42_43_44", "43_44_45", "44_45_46"), with = T]
dt.train <- dt.train[, !c("UNIT", "THIS_PROFIT_LOSS"), with = F]
dim(dt.train)
# [1] 298048     59
dt.test <- dt.3in1[UNIT == "44_45_46", with = T]
dt.test <- dt.test[, !c("UNIT", "THIS_PROFIT_LOSS", "PRED"), with = F]
dim(dt.test)
# [1] 12935    58

##############################
## 1.2 model - rf ############
##############################
# train
require(randomForest)
set.seed(100)
md.rf <- randomForest(PRED ~.
                      , data = dt.train[, !c("ACCOUNT_ID"), with = F]
                      , mtry = floor(sqrt(58))
                      , ntree = 100
                      , importance = T)
md.rf
# test
pred.test <- predict(md.rf, newdata = dt.test[, !c("ACCOUNT_ID"), with = F])
table(pred.test)
# pred.test
# 0     1 
# 10455  2480 
dt.submit <- data.table(Account_ID = dt.test$ACCOUNT_ID, Prediction = pred.test)
dt.submit <- merge(dtSampleSubmit, dt.submit, by = "Account_ID", all.x = T, sort = F)
write.csv(dt.submit, "submit/6_211115_1297_rf_with_3in1_no_valid.csv", row.names = F) # 0.56465

#####################################################################
## 2. try using all the data set except the test set to train a rf ##
#####################################################################