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

# test.knn <- knn(x.train
#                 , x.test
#                 , y.train
#                 , k = 1
#                 , prob = T)

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
## lr ##
md.blend.lr <- glm(PRED ~.
                   , family = binomial
                   , data = dt.train.ensemble)

colAUC(predict(md.blend.lr, newdata = dt.train.ensemble, type = "response"), dt.train.ensemble$PRED)
# [,1]
# 0 vs. 1 0.871239
colAUC(predict(md.blend.lr, newdata = dt.valid1.ensemble, type = "response"), dt.valid1.ensemble$PRED)
# [,1]
# 0 vs. 1 0.8971305
colAUC(predict(md.blend.lr, newdata = dt.valid2.ensemble, type = "response"), dt.valid2.ensemble$PRED)
# [,1]
# 0 vs. 1 0.7876071

# try submit
pred.test <- predict(md.blend.lr, newdata = dt.test.ensemble, type = "response")
dt.submit <- data.table(Account_ID = dt.test$ACCOUNT_ID, Prediction = pred.test)
dim(dt.submit)
# [1] 12935     2
dt.submit <- merge(dtSampleSubmit, dt.submit, by = "Account_ID", all.x = T)
# [1] 7374    2
write.csv(dt.submit, "submit/31_071215_2203_blending_lr_.csv", row.names = F) # 0.50377

# try add up 
pred.test <-  colTest.rf + colTest.nnet + colTest.lasso + colTest.lr + colTest.xgb.gbtree + colTest.xgb.gblinear
dt.submit <- data.table(Account_ID = dt.test$ACCOUNT_ID, Prediction = pred.test)
dim(dt.submit)
# [1] 12935     2
dt.submit <- merge(dtSampleSubmit, dt.submit, by = "Account_ID", all.x = T)
# [1] 7374    2
write.csv(dt.submit, "submit/32_071215_2203_bagging_.csv", row.names = F) # 0.50377























