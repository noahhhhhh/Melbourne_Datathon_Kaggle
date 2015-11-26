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
#######################################
## 1.0 train, valid, and test set #####
#######################################
##############################
## 1.1 train set #############
##############################
dt.train <- dt.scaledAll[!UNIT %in% c("36_37_38", "37_38_39", "38_39_40" # valid set 1 related
                                      , "39_40_41", "40_41_42", "41_42_43" # valide set 2 related
                                      , "42_43_44", "43_44_45", "44_45_46") # test set related
                         , with = T]
dt.train[, UNIT := NULL]
# dt.train[, ACCOUNT_ID := NULL]
dt.train[, THIS_PROFIT_LOSS := NULL]
str(dt.train)
dim(dt.train)
# [1] 256719     59

##############################
## 1.2 valid set #############
##############################
# valid set 1
dt.valid1 <- dt.scaledAll[UNIT == c("38_39_40"), with = T]
dt.valid1[, UNIT := NULL]
# dt.valid[, ACCOUNT_ID := NULL]
dt.valid1[, THIS_PROFIT_LOSS := NULL]
str(dt.valid1)
dim(dt.valid1)
# [1] 6791   59

# valid set 2
dt.valid2 <- dt.scaledAll[UNIT == c("41_42_43"), with = T]
dt.valid2[, UNIT := NULL]
# dt.valid[, ACCOUNT_ID := NULL]
dt.valid2[, THIS_PROFIT_LOSS := NULL]
str(dt.valid2)
dim(dt.valid2)
# [1] 8082   59

##############################
## 1.3 test set #############
##############################
dt.test <- dt.scaledAll[UNIT == c("44_45_46"), with = T]
dt.test[, UNIT := NULL]
# dt.test[, ACCOUNT_ID := NULL]
dt.test[, THIS_PROFIT_LOSS := NULL]
dt.test[, PRED := NULL]
str(dt.test)
dim(dt.test)
# [1] 12935    58

#######################################
## 2.0 try to train a model  ##########
#######################################
##############################
## 2.1 bagged tree ###########
##############################
require(randomForest)
set.seed(1)
md.bagTree <- randomForest(PRED ~., data = dt.train[, !c("ACCOUNT_ID"), with = F], mtry = 57, ntree = 20, importance = T)
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
                      , data = dt.scaledAll[, !c("ACCOUNT_ID", "UNIT", "THIS_PROFIT_LOSS"), with = F]
                      , mtry = floor(sqrt(57))
                      , ntree = 100
                      , importance = T)
md.rf
# Call:
#     randomForest(formula = PRED ~ ., data = dt.train[, !c("ACCOUNT_ID"),      with = F], mtry = floor(sqrt(57)), ntree = 20, importance = T) 
# Type of random forest: classification
# Number of trees: 20
# No. of variables tried at each split: 7
# 
# OOB estimate of  error rate: 16.81%
# Confusion matrix:
#     0      1 class.error
# 0 110180  24172   0.1799154
# 1  21820 117483   0.1566370

varImpPlot(md.rf)
# try on valid set
pred.valid1 <- predict(md.rf, newdata = dt.valid1[, !c("ACCOUNT_ID"), with = F])
pred.valid1 <- as.numeric(ifelse(pred.valid1 == 1, 1, 0))
colAUC(pred.valid1, dt.valid1$PRED, plotROC = T)
# [,1]
# 0 vs. 1 0.815286
table(pred.valid1, dt.valid1$PRED)
# pred.valid    0    1
# 1 3492  691
# 2  803 3096
confusionMatrix(pred.valid1, dt.valid1$PRED)
# Confusion Matrix and Statistics
# 
# Reference
# Prediction    0    1
# 0 3496  675
# 1  799 3112
# 
# Accuracy : 0.8176        
# 95% CI : (0.809, 0.826)
# No Information Rate : 0.5314        
# P-Value [Acc > NIR] : < 2.2e-16     
# 
# Kappa : 0.6345        
# Mcnemar's Test P-Value : 0.001357      
#                                         
#             Sensitivity : 0.8140        
#             Specificity : 0.8218        
#          Pos Pred Value : 0.8382        
#          Neg Pred Value : 0.7957        
#              Prevalence : 0.5314        
#          Detection Rate : 0.4326        
#    Detection Prevalence : 0.5161        
#       Balanced Accuracy : 0.8179        
#                                         
#        'Positive' Class : 0 

pred.valid2 <- predict(md.rf, newdata = dt.valid2[, !c("ACCOUNT_ID"), with = F])
pred.valid2 <- as.numeric(ifelse(pred.valid2 == 1, 1, 0))
colAUC(pred.valid2, dt.valid2$PRED, plotROC = T)
# [,1]
# 0 vs. 1 0.815286
table(pred.valid2, dt.valid2$PRED)
# pred.valid    0    1
# 1 3492  691
# 2  803 3096
confusionMatrix(pred.valid2, dt.valid2$PRED)


pred.test <- predict(md.rf, newdata = dt.test[, !c("ACCOUNT_ID"), with = F])
table(pred.test)
# pred.test
# 0    1 
# 9801 3134 
dt.submit <- data.table(Account_ID = dt.test$ACCOUNT_ID, Prediction = pred.test)
dt.submit <- merge(dtSampleSubmit, dt.submit, by = "Account_ID", all.x = T, all.y = F)
write.csv(dt.submit, "submit/5_201115_2117_rf_with_3in1_corrected_train_and_valid.csv", row.names = FALSE) # 0.53..

##############################
## 2.3 boosting tree #########
##############################
require(gbm)
set.seed(1)
md.boostTree <- gbm(PRED ~., data = dt.train[, !c("ACCOUNT_ID"), with = F]
                    , distribution = "adaboost"
                    , n.trees = 200
                    , shrinkage = 1
                    )
md.boostTree <- gbm.fit(x = dt.train[, !c("ACCOUNT_ID", "PRED"), with = F]
                        , y = dt.train$PRED)
md.boostTree
summary(md.boostTree)

# try on valid set
pred.valid <- predict(md.boostTree, newdata = dt.valid[, !c("ACCOUNT_ID"), with = F], n.trees = 200)
pred.valid <- as.numeric(ifelse(pred.valid == 1, 1, 0))
colAUC(pred.valid, dt.valid$PRED, plotROC = T)

table(pred.valid, dt.valid$PRED)

confusionMatrix(pred.valid, dt.valid$PRED)

# try on test
pred.test <- predict(md.boostTree, newdata = dt.test[, !c("ACCOUNT_ID"), with = F])
table(pred.test)





