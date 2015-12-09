###########################################################################################
## ensemble ###############################################################################
###########################################################################################
setwd("/Volumes/Data Science/Google Drive/data_science_competition/melbourne_datathon/Melbourne_Datathon_Kaggle/")
rm(list = ls()); gc()
require(data.table)
require(plyr)
require(dplyr)
require(caret)
require(caTools)
library(doMC)
library(parallel)
library(doParallel)
require(foreach)
require(class)
require(randomForest)
require(neuralnet)
require(glmnet)
load("../Datathon_Full_Dataset/md_rf.RData")
load("../Datathon_Full_Dataset/md_nnet.RData")
load("../Datathon_Full_Dataset/md_lasso.RData")
load("../Datathon_Full_Dataset/md_step_lr.RData")
load("../Datathon_Full_Dataset/md_xgb_gblinear.RData")
load("../Datathon_Full_Dataset/md_xgb_gbtree.RData")
load("../Datathon_Full_Dataset/md_xgb_gbtree.RData")

load("../Datathon_Full_Dataset/pred_valid1_xgb.RData")
load("../Datathon_Full_Dataset/pred_valid2_xgb.RData")
load("../Datathon_Full_Dataset/pred_valid1_xgb_linear.RData")
load("../Datathon_Full_Dataset/pred_valid2_xgb_linear.RData")
load("../Datathon_Full_Dataset/pred_test_xgb.RData")
load("../Datathon_Full_Dataset/pred_test_xgb_linear.RData")

load("../Datathon_Full_Dataset/trainData.RData")
load("../Datathon_Full_Dataset/valid1Data.RData")
load("../Datathon_Full_Dataset/valid2Data.RData")
load("../Datathon_Full_Dataset/testData.RData")

dtSampleSubmit <- fread("../data_files/sample_submission_bet_size.csv")
dtSampleSubmit[, Prediction := NULL]
setnames(dtSampleSubmit, "Account_ID")

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
# 
# colTrain.knn <- knn(x.train
#                     , x.train
#                     , y.train
#                     , k = 1
#                     , prob = T)
# colValid1.knn <- knn(x.train
#                     , x.valid1
#                     , y.train
#                     , k = 1
#                     , prob = T)
# colValid2.knn <- knn(x.train
#                     , x.valid2
#                     , y.train
#                     , k = 1
#                     , prob = T)
# colTest.knn <- knn(x.train
#                     , x.test
#                     , y.train
#                     , k = 1
#                     , prob = T)

##############################
## 1. ensemble ###############
##############################
#######################
## predictions ########
#######################
# rf 0.62786
colTrain.rf <- as.numeric(predict(md.rf, x.train, type = "prob")[, 1])
colValid1.rf <- as.numeric(predict(md.rf, x.valid1, type = "prob")[, 1])
colValid2.rf <- as.numeric(predict(md.rf, x.valid2, type = "prob")[, 1])
colTest.rf <- as.numeric(predict(md.rf, x.test, type = "prob")[, 1])

# nnet 0.62682
colTrain.nnet <- as.numeric(compute(md.nnet, x.train)$net.result)
colValid1.nnet <- as.numeric(compute(md.nnet, x.valid1)$net.result)
colValid2.nnet <- as.numeric(compute(md.nnet, x.valid2)$net.result)
colTest.nnet <- as.numeric(compute(md.nnet, x.test)$net.result)

# lasso 0.61734
colTrain.lasso <- as.numeric(predict(md.lasso, x.train, s = 6e-05, type = "response"))
colValid1.lasso <- as.numeric(predict(md.lasso, x.valid1, s = 6e-05, type = "response"))
colValid2.lasso <- as.numeric(predict(md.lasso, x.valid2, s = 6e-05, type = "response"))
colTest.lasso <- as.numeric(predict(md.lasso, x.test, s = 6e-05, type = "response"))

# stepped logistic regression ?
colTrain.lr <- as.numeric(predict(step.lr, newdata = dt.train, type = "response"))
colValid1.lr <- as.numeric(predict(step.lr, newdata = dt.valid1, type = "response"))
colValid2.lr <- as.numeric(predict(step.lr, newdata = dt.valid2, type = "response"))
colTest.lr <- as.numeric(predict(step.lr, newdata = dt.test, type = "response"))

# xgboost - gbtree 0.63165
colTrain.xgb.gbtree <- predict(md.xgboost, x.train)
colValid1.xgb.gbtree <- pred.valid1.xgb
colValid2.xgb.gbtree <- pred.valid2.xgb
colTest.xgb.gbtree <- pred.test.xgb

# xgboost - gblinear ?
colTrain.xgb.gblinear <- predict(md.xgboost.linear, x.train)
colValid1.xgb.gblinear <- pred.valid1.xgb.linear
colValid2.xgb.gblinear <- pred.valid2.xgb.linear
colTest.xgb.gblinear <- pred.test.xgb.linear

#######################
## dt.train.ensemble ##
#######################
dt.train.ensemble <- data.table(PRED = y.train
                                , rf = colTrain.rf
                                , nnet = colTrain.nnet
                                , lasso = colTrain.lasso
                                , lr = colTrain.lr
                                , xgb.gbtree = colTrain.xgb.gbtree
                                , xgb.gblinear = colTrain.xgb.gblinear)

#######################
## dt.valid1.ensemble #
#######################
dt.valid1.ensemble <- data.table(PRED = y.valid1
                                , rf = colValid1.rf
                                , nnet = colValid1.nnet
                                , lasso = colValid1.lasso
                                , lr = colValid1.lr
                                , xgb.gbtree = colValid1.xgb.gbtree
                                , xgb.gblinear = colValid1.xgb.gblinear)

#######################
## dt.valid2.ensemble #
#######################
dt.valid2.ensemble <- data.table(PRED = y.valid2
                                 , rf = colValid2.rf
                                 , nnet = colValid2.nnet
                                 , lasso = colValid2.lasso
                                 , lr = colValid2.lr
                                 , xgb.gbtree = colValid2.xgb.gbtree
                                 , xgb.gblinear = colValid2.xgb.gblinear)

#######################
## dt.test.ensemble ###
#######################
dt.test.ensemble <- data.table(rf = colTest.rf
                               , nnet = colTest.nnet
                               , lasso = colTest.lasso
                               , lr = colTest.lr
                               , xgb.gbtree = colTest.xgb.gbtree
                               , xgb.gblinear = colTest.xgb.gblinear)

save(dt.train.ensemble, file = "../Datathon_Full_Dataset/trainEnsemble.RData")
save(dt.valid1.ensemble, file = "../Datathon_Full_Dataset/valid1Ensemble.RData")
save(dt.valid2.ensemble, file = "../Datathon_Full_Dataset/valid2Ensemble.RData")
save(dt.test.ensemble, file = "../Datathon_Full_Dataset/testEnsemble.RData")

##################################
## ensemble model - blending #####
##################################
setwd("/Volumes/Data Science/Google Drive/data_science_competition/melbourne_datathon/Melbourne_Datathon_Kaggle/")
rm(list = ls()); gc();
require(data.table)
load("../Datathon_Full_Dataset/trainEnsemble.RData")
load("../Datathon_Full_Dataset/valid1Ensemble.RData")
load("../Datathon_Full_Dataset/valid2Ensemble.RData")
load("../Datathon_Full_Dataset/testEnsemble.RData")

load("../Datathon_Full_Dataset/trainData.RData")
load("../Datathon_Full_Dataset/valid1Data.RData")
load("../Datathon_Full_Dataset/valid2Data.RData")
load("../Datathon_Full_Dataset/testData.RData")

dtSampleSubmit <- fread("../data_files/sample_submission_bet_size.csv")
dtSampleSubmit[, Prediction := NULL]
setnames(dtSampleSubmit, "Account_ID")


dt.test.ensemble[, rf:= 1 - rf]
# try simple blending with rf[, 2]
pred.bagging <- rowSums(dt.test.ensemble) # no weight!
pred.bagging <- (dt.test.ensemble$rf * .10
                 + dt.test.ensemble$nnet * .30
                 + dt.test.ensemble$lasso * .05
                 + dt.test.ensemble$lr * .05
                 + dt.test.ensemble$xgb.gbtree * .30
                 + dt.test.ensemble$xgb.gblinear * .15) # has weight!

dt.submit <- data.table(Account_ID = dt.test$ACCOUNT_ID, Prediction = pred.bagging)
dim(dt.submit)
# [1] 12935     2
dt.submit <- merge(dtSampleSubmit, dt.submit, by = "Account_ID", all.x = T, sort = F)
# [1] 7374    2

# set the scores of new accts to .437 * 2 * original prediction
trainAcct <- unique(dt.train$ACCOUNT_ID)
valid1Acct <- unique(dt.valid1$ACCOUNT_ID)
valid2Acct <- unique(dt.valid2$ACCOUNT_ID)

existingAcct <- c(trainAcct, valid1Acct, valid2Acct)
existingAcct <- unique(existingAcct)
length(existingAcct)
# [1] 20841

newAcct <- setdiff(dt.submit$Account_ID, existingAcct)

# Prediction_New <- rnorm(1001, mean = .43, sd = .1)
Prediction_New <- .437
dt.newAcct <- data.table(Account_ID = newAcct, Prediction_New = Prediction_New)
# bestSub[, Prediction := Prediction/14]

newSub <- merge(dt.submit, dt.newAcct, by = "Account_ID", all.x = T)
newSub[, Prediction_New := Prediction * 2 * Prediction_New]
newSub[, Prediction := ifelse(is.na(newSub$Prediction_New), newSub$Prediction, newSub$Prediction_New)]
newSub[, Prediction_New := NULL]
newSub[Prediction == .437, with = T] # 0.64343
write.csv(newSub, "submit/42_081215_1743_6_model_bagging_with_weights_20_25_05_10_30_10.csv", row.names = F) # 0.63165

# try add knn
dt.test.ensemble[, knn := colTest.knn]
save(dt.test.ensemble, file = "../Datathon_Full_Dataset/testEnsemble_with_knn.RData")

















