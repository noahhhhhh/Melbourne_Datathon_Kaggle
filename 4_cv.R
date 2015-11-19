###########################################################################################
## Transform, Training and Testing sets ###################################################
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
dtSampleSubmit$Account_ID <- as.character(dtSampleSubmit$Account_ID)

#######################################
## 1.0 scale ##########################
#######################################
##############################
## 1.1 scale all together ####
##############################
dt.scaledAll <- as.data.table(scale(dt.3in1[, !c("UNIT", "ACCOUNT_ID"
                                   , "THIS_ME2ME", "THIS_IN_AND_OUT_PLAY", "THIS_IS_FROM_WIN"
                                   , "THIS_IS_FROM_LOSE", "THIS_IS_FROM_NEITHER", "THIS_RESULT_EXPECTED"
                                   , "PRED"), with = F]))

dt.sacledAll <- cbind(dt.3in1[, c("UNIT", "ACCOUNT_ID"
                                  , "THIS_ME2ME", "THIS_IN_AND_OUT_PLAY", "THIS_IS_FROM_WIN"
                                  , "THIS_IS_FROM_LOSE", "THIS_IS_FROM_NEITHER", "THIS_RESULT_EXPECTED"
                                  , "PRED"), with = F]
                      , dt.scaledAll)

str(dt.sacledAll)
#######################################
## 1.0 train, valid, and test set #####
#######################################
##############################
## 1.1 train set #############
##############################
dt.train <- dt.sacledAll[!UNIT %in% c("43_44_45", "44_45_46"), with = T]
dt.train[, UNIT := NULL]
# dt.train[, ACCOUNT_ID := NULL]
dt.train[, THIS_PROFIT_LOSS := NULL]
str(dt.train)
dim(dt.train)
# [1] 309171     55 # exclude ACCOUNT_ID
##############################
## 1.2 valid set #############
##############################
dt.valid <- dt.sacledAll[UNIT == c("43_44_45"), with = T]
dt.valid[, UNIT := NULL]
# dt.valid[, ACCOUNT_ID := NULL]
dt.valid[, THIS_PROFIT_LOSS := NULL]
str(dt.valid)
dim(dt.valid)
# [1] 11893    55 # exclude ACCOUNT_ID
##############################
## 1.3 test set #############
##############################
dt.test <- dt.sacledAll[UNIT == c("44_45_46"), with = T]
dt.test[, UNIT := NULL]
# dt.test[, ACCOUNT_ID := NULL]
dt.test[, THIS_PROFIT_LOSS := NULL]
dt.test[, PRED := NULL]
str(dt.test)
dim(dt.test)
# [1] 12935    54 # exclude ACCOUNT_ID

#######################################
## 2.0 try to train a model  ##########
#######################################
##############################
## 2.1 bagged tree ###########
##############################
require(randomForest)
set.seed(1)
md.bagTree <- randomForest(PRED ~., data = dt.train[, !c("ACCOUNT_ID"), with = F], mtry = 54, ntree = 20, importance = T)
md.bagTree
# Call:
#     randomForest(formula = PRED ~ ., data = dt.train[, !c("ACCOUNT_ID"),      with = F], mtry = 54, ntree = 20, importance = T) 
# Type of random forest: classification
# Number of trees: 20
# No. of variables tried at each split: 54
# 
# OOB estimate of  error rate: 12.92%
# Confusion matrix:
#     0      1 class.error
# 0 135726  20682   0.1322311
# 1  19267 133461   0.1261524

varImpPlot(md.bagTree)

# try on valid set
pred.valid <- predict(md.bagTree, newdata = dt.valid[, !c("ACCOUNT_ID"), with = F])
pred.valid <- as.numeric(ifelse(pred.valid == 1, 1, 0))
colAUC(pred.valid, dt.valid$PRED, plotROC = T)
# [,1]
# 0 vs. 1 0.9248425
table(pred.valid, dt.valid$PRED)
# Confusion Matrix and Statistics
# 
# Reference
# Prediction    0    1
# 0 9214  229
# 1  457 1993
# 
# Accuracy : 0.9423         
# 95% CI : (0.938, 0.9464)
# No Information Rate : 0.8132         
# P-Value [Acc > NIR] : < 2.2e-16      
# 
# Kappa : 0.8174         
# Mcnemar's Test P-Value : < 2.2e-16      
# 
# Sensitivity : 0.9527         
# Specificity : 0.8969         
# Pos Pred Value : 0.9757         
# Neg Pred Value : 0.8135         
# Prevalence : 0.8132         
# Detection Rate : 0.7747         
# Detection Prevalence : 0.7940         
# Balanced Accuracy : 0.9248         
# 
# 'Positive' Class : 0 

# try on test set
pred.test <- predict(md.bagTree, newdata = dt.test[, !c("ACCOUNT_ID"), with = F])
table(pred.test)
# pred.test
# 0     1 
# 11725  1210 
dt.submit <- data.table(Account_ID = dt.test$ACCOUNT_ID, Prediction = pred.test)
dt.submit <- merge(dtSampleSubmit, dt.submit, by = "Account_ID", all.x = T, all.y = F)
write.csv(dt.submit, "submit/1_191115_1727_bagTree_with_3in1.csv", row.names = FALSE)

##############################
## 2.2 random forest #########
##############################
require(randomForest)
set.seed(1)
md.rf <- randomForest(PRED ~.
                      , data = dt.train[, !c("ACCOUNT_ID"), with = F]
                      , mtry = ceiling(sqrt(54))
                      , ntree = 20
                      , importance = T)
md.rf
# Call:
#     randomForest(formula = PRED ~ ., data = dt.train[, !c("ACCOUNT_ID"),      with = F], mtry = ceiling(sqrt(54)), ntree = 20, importance = T) 
# Type of random forest: classification
# Number of trees: 20
# No. of variables tried at each split: 8
# 
# OOB estimate of  error rate: 15.99%
# Confusion matrix:
#     0      1 class.error
# 0 130660  25753   0.1646474
# 1  23689 129028   0.1551170

varImpPlot(md.rf)
# try on valid set
pred.valid <- predict(md.rf, newdata = dt.valid[, !c("ACCOUNT_ID"), with = F])
pred.valid <- as.numeric(ifelse(pred.valid == 1, 1, 0))
colAUC(pred.valid, dt.valid$PRED, plotROC = T)
# [,1]
# 0 vs. 1 0.8782424
table(pred.valid, dt.valid$PRED)
# pred.valid    0    1
# 1 8829  348
# 2  841 1875
confusionMatrix(pred.valid, dt.valid$PRED)
# Confusion Matrix and Statistics
# 
# Reference
# Prediction    0    1
# 0 8846  346
# 1  824 1877
# 
# Accuracy : 0.9016          
# 95% CI : (0.8961, 0.9069)
# No Information Rate : 0.8131          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.7011          
# Mcnemar's Test P-Value : < 2.2e-16       
# 
# Sensitivity : 0.9148          
# Specificity : 0.8444          
# Pos Pred Value : 0.9624          
# Neg Pred Value : 0.6949          
# Prevalence : 0.8131          
# Detection Rate : 0.7438          
# Detection Prevalence : 0.7729          
# Balanced Accuracy : 0.8796          
# 
# 'Positive' Class : 0  
# try on test set
pred.test <- predict(md.rf, newdata = dt.test[, !c("ACCOUNT_ID"), with = F])
table(pred.test)
# pred.test
# 0     1 
# 10850  2085 
dt.submit <- data.table(Account_ID = dt.test$ACCOUNT_ID, Prediction = pred.test)
dt.submit <- merge(dtSampleSubmit, dt.submit, by = "Account_ID", all.x = T, all.y = F)
write.csv(dt.submit, "submit/2_191115_1731_rf_with_3in1.csv", row.names = FALSE) # 0.55338


