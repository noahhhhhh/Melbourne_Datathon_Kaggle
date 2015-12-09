###########################################################################################
## final model ############################################################################
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
# load("../Datathon_Full_Dataset/trainData_less.RData")
# load("../Datathon_Full_Dataset/valid1Data_less.RData")
# load("../Datathon_Full_Dataset/valid2Data_less.RData")
# load("../Datathon_Full_Dataset/testData_less.RData")
load("../Datathon_Full_Dataset/trainData.RData")
load("../Datathon_Full_Dataset/valid1Data.RData")
load("../Datathon_Full_Dataset/valid2Data.RData")
load("../Datathon_Full_Dataset/testData.RData")
# dtTestFeatures <- fread("../data_files/semi_and_final_features.csv")
dtSampleSubmit <- fread("../data_files/sample_submission_bet_size.csv")
dtSampleSubmit[, Prediction := NULL]
setnames(dtSampleSubmit, "Account_ID")

#####################################################################
## 1. xgboost on more random 3in1 ###################################
#####################################################################
##############################
## 1.1 take a sample #########
##############################
indTrain <- createDataPartition(dt.train$PRED
                                , p = .1
                                , list = F)

dt.sampleTrain <- dt.train[indTrain, with = T]
x.sampleTrain <- model.matrix(as.integer(PRED) - 1 ~., dt.sampleTrain[, !c("ACCOUNT_ID"), with = F])[, -1]
y.sampleTrain <- ifelse(as.integer(dt.sampleTrain$PRED) == 1, 0, 1)
dmx.sampleTrain <- xgb.DMatrix(data =  x.sampleTrain, label = y.sampleTrain)

##############################
## 1.2 xgboost train, valid, and test transform
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
## 1.3 test model - xgboost
#############################
for (eta in seq(.01, 1, by = .05)){
    for (nrounds in seq(100, 1500, by = 200)){
        md.test.xgboost <- xgb.train(data = dmx.sampleTrain
                                     , objective = "binary:logistic"
                                     , params = list(nthread = 8
                                                     , eval_metric = "auc"
                                                     , eta = eta
                                                     , max_depth = 8
                                                     # , subsample = subsample[j]
                                                     , colsample_bytree = 1
                                     )
                                     #                              , watchlist = list(eval1 = dmx.valid1
                                     #                                                 , eval2 = dmx.valid2
                                     #                                                 , train = dmx.sampleTrain)
                                     , nrounds = nrounds
                                     , verbose = T
        )
        pred.valid2.xgb.sample <- predict(md.test.xgboost, newdata = x.valid2)
        auc <- colAUC(pred.valid2.xgb.sample, y.valid2)
        print(paste("eat:", eta, "; nrounds:", nrounds, "; auc:", auc))
    }
}
# [1] "eat: 0.26 ; nrounds: 1500 ; auc: 0.787609529616889"
set.seed(1)
md.test.xgboost <- xgb.train(data = dmx.sampleTrain
                             , objective = "binary:logistic"
                             , params = list(nthread = 8
                                             , eval_metric = "auc"
                                             , eta = 0.025
                                             , max_depth = 8
                                             , subsample = .3
                                             , colsample_bytree = .3
                                             )
                             , watchlist = list(eval1 = dmx.valid1
                                                , eval2 = dmx.valid2
                                                , train = dmx.sampleTrain)
                             , nrounds = 2000
                             , verbose = T
                             )

pred.train.xgb.sample <- predict(md.test.xgboost, newdata = x.train)
colAUC(pred.train.xgb.sample, y.train)
# [,1]
# 0 vs. 1 0.781576 col = 1
# [,1]
# 0 vs. 1 0.7734595 col = .3
pred.valid1.xgb.sample <- predict(md.test.xgboost, newdata = x.valid1)
colAUC(pred.valid1.xgb.sample, y.valid1)
# [,1]
# 0 vs. 1 0.8583072 col = 1
# [,1]
# 0 vs. 1 0.8525196 col = .3
pred.valid2.xgb.sample <- predict(md.test.xgboost, newdata = x.valid2)
colAUC(pred.valid2.xgb.sample, y.valid2)
# [,1]
# 0 vs. 1 0.7434026 col = 1
# [,1]
# 0 vs. 1 0.7350793 col = .3

##############################
## 1.3 submit xgboost - test
##############################
pred.test.xgb.sample <- predict(md.test.xgboost, newdata = x.test)
dt.submit <- data.table(Account_ID = dt.test$ACCOUNT_ID, Prediction = pred.test.xgb.sample)
dim(dt.submit)
# [1] 12935     2
dt.submit <- merge(dtSampleSubmit, dt.submit, by = "Account_ID", all.x = T, sort = F)
# [1] 7374    2
write.csv(dt.submit, "submit/22_051215_2254_1_xgboost_sample_.3_cols_with_more_random_3in1_preprocess_valid1_valid2_.csv", row.names = F) # 0.56181


##############################
## 1.3 model - xgboost - gbtree
#############################
eta <- rep(.025, 9)
# max_depth <- c(8, 7, 7, 6, 5)
# min.child <- c(5, 5, 5, 5, 5)
subsample <- rep(.8, 9)
colsample_bytree <- rep(.3, 9)
nrounds <- rep(2500, 9)

reps <- 1
models <- 9

pred.valid1.xgb <- rep(0, nrow(x.valid1))
pred.valid2.xgb <- rep(0, nrow(x.valid2))
pred.test.xgb <- rep(0, nrow(dtSampleSubmit))

for (i in 1:reps){
    print(paste("rep", i, "- Start"))
    for(j in 1:models){
        print(paste("------model", j, ": Start"))
        # set.seed(1000 * i + 100 * j)
        set.seed(j)
        md.xgboost <- xgb.train(data = dmx.train
                                , objective = "binary:logistic"
                                , params = list(nthread = 8
                                                , eval_metric = "auc"
                                                , eta = eta[j]
                                                # , max_depth = max_depth[j]
                                                , subsample = subsample[j]
                                                , colsample_bytree = colsample_bytree[j])
                                , watchlist = list(eval1 = dmx.valid1
                                                   , eval2 = dmx.valid2
                                                   , train = dmx.train)
                                , nrounds = nrounds[j]
                                , verbose = T)
        pred.valid1.xgb <- pred.valid1.xgb + predict(md.xgboost, x.valid1) / models 
        pred.valid2.xgb <- pred.valid2.xgb + predict(md.xgboost, x.valid2) / models
        pred.test.xgb <- pred.test.xgb + predict(md.xgboost, x.test) / models
        print(paste("------model", j, ": End"))
    }
    print(paste("rep", i, "- End"))
}
# average valid1 = 0.842147
# average valid2 = 0.725920
pred.test
summary(pred.test)

save(pred.valid1.xgb, file = "../Datathon_Full_Dataset/pred_valid1_xgb.RData")
save(pred.valid2.xgb, file = "../Datathon_Full_Dataset/pred_valid2_xgb.RData")
save(pred.test.xgb, file = "../Datathon_Full_Dataset/pred_test_xgb.RData")
save(md.xgboost, file = "../Datathon_Full_Dataset/md_xgb_gbtree.RData")

colAUC(pred.valid1.xgb, y.valid1)
# [,1]
# 0 vs. 1 0.8717821
colAUC(pred.valid2.xgb, y.valid2)
# [,1]
# 0 vs. 1 0.7443788

##############################
## 1.4 model - xgboost - gblinear
#############################
eta <- rep(.025, 9)
# max_depth <- c(8, 7, 7, 6, 5)
# min.child <- c(5, 5, 5, 5, 5)
subsample <- rep(.8, 9)
colsample_bytree <- rep(.3, 9)
nrounds <- rep(239, 9)

reps <- 1
models <- 9

pred.valid1.xgb.linear <- rep(0, nrow(x.valid1))
pred.valid2.xgb.linear <- rep(0, nrow(x.valid2))
pred.test.xgb.linear <- rep(0, nrow(dtSampleSubmit))

for (i in 1:reps){
    print(paste("rep", i, "- Start"))
    for(j in 1:models){
        print(paste("------model", j, ": Start"))
        # set.seed(1000 * i + 100 * j)
        set.seed(j)
        md.xgboost.linear <- xgb.train(data = dmx.train
                                , objective = "binary:logistic"
                                , params = list(nthread = 8
                                                , booster = "gblinear" # linear
                                                , eval_metric = "auc"
                                                , eta = eta[j]
                                                # , max_depth = max_depth[j]
                                                , subsample = subsample[j]
                                                , colsample_bytree = colsample_bytree[j])
                                , watchlist = list(eval1 = dmx.valid1
                                                   , eval2 = dmx.valid2
                                                   , train = dmx.train)
                                , nrounds = nrounds[j]
                                , verbose = T)
        pred.valid1.xgb.linear <- pred.valid1.xgb.linear + predict(md.xgboost, x.valid1) / models 
        pred.valid2.xgb.linear <- pred.valid2.xgb.linear + predict(md.xgboost, x.valid2) / models
        pred.test.xgb.linear <- pred.test.xgb.linear + predict(md.xgboost, x.test) / models
        print(paste("------model", j, ": End"))
    }
    print(paste("rep", i, "- End"))
}
# [143]	eval1-auc:0.728967	eval2-auc:0.630897	train-auc:0.612640
# [239]	eval1-auc:0.727975	eval2-auc:0.631165	train-auc:0.613446
save(pred.valid1.xgb.linear, file = "../Datathon_Full_Dataset/pred_valid1_xgb_linear.RData")
save(pred.valid2.xgb.linear, file = "../Datathon_Full_Dataset/pred_valid2_xgb_linear.RData")
save(pred.test.xgb.linear, file = "../Datathon_Full_Dataset/pred_test_xgb_linear.RData")
save(md.xgboost.linear, file = "../Datathon_Full_Dataset/md_xgb_gblinear.RData")

colAUC(pred.valid1.xgb.linear, y.valid1)
# [,1]
# 0 vs. 1 0.870309
colAUC(pred.valid2.xgb.linear, y.valid2)
# [,1]
# 0 vs. 1 0.7420239


##############################
## 1.3 submit xgboost
##############################
dt.submit <- data.table(Account_ID = dt.test$ACCOUNT_ID, Prediction = (pred.test.xgb + pred.test.xgb.linear) / 2)
dim(dt.submit)
# [1] 12935     2
dt.submit <- merge(dtSampleSubmit, dt.submit, by = "Account_ID", all.x = T, sort = F)
# [1] 7374    2
write.csv(dt.submit, "submit/29_071215_1743_9_gbtree_9_gblinear_with_random_3in1_preprocess_valid1_valid2_.csv", row.names = F) # 0.63165

#####################################################################
## 2. rf on random 3in1 #############################################
#####################################################################
##############################
## 2.1 train, valid, and test transform
##############################
require(xgboost)
require(Ckmeans.1d.dp)
x.train <- model.matrix(PRED ~., dt.train[, !c("ACCOUNT_ID"), with = F])[, -1]
y.train <- dt.train$PRED
dmx.train <- xgb.DMatrix(data =  x.train, label = y.train)

x.valid1 <- model.matrix(PRED ~., dt.valid1[, !c("ACCOUNT_ID"), with = F])[, -1]
y.valid1 <- dt.valid1$PRED
dmx.valid1 <- xgb.DMatrix(data =  x.valid1, label = y.valid1)

x.valid2 <- model.matrix(PRED ~., dt.valid2[, !c("ACCOUNT_ID"), with = F])[, -1]
y.valid2 <- dt.valid2$PRED
dmx.valid2 <- xgb.DMatrix(data =  x.valid2, label = y.valid2)

x.test <- model.matrix(~., dt.test[, !c("ACCOUNT_ID"), with = F])[, -1]

##############################
## 2.2 rf model
##############################
require(randomForest)
cores <- detectCores()
cl <- makeCluster(cores)
registerDoParallel(cl)
set.seed(1)
md.rf <- foreach(ntree = rep(250, 8)
                 , .combine = combine
                 , .packages = "randomForest") %dopar% 
    randomForest(x = x.train
                 , y = y.train
                 # , xtest = x.valid2
                 # , ytest = y.valid2
                 , ntree = ntree
                 , mtry = floor(sqrt(ncol(x.train)))
                 , replace = T
                 , nodesize = 100
                 , importance = T
                 , keep.forest = T
    )
stopCluster(cl)
varImpPlot(md.rf)

save(md.rf, file = "../Datathon_Full_Dataset/md_rf.RData")

pred.train <- predict(md.rf, newdata = x.train, type = "prob")[, 2]
colAUC(pred.train, y.train)
# [,1]
# 0 vs. 1 0.7786889
pred.valid1 <- predict(md.rf, newdata = x.valid1, type = "prob")[, 2]
colAUC(pred.valid1, y.valid1)
# [,1]
# 0 vs. 1 0.894758
pred.valid2 <- predict(md.rf, newdata = x.valid2, type = "prob")[, 2]
colAUC(pred.valid2, y.valid2)
# [,1]
# 0 vs. 1 0.778537

##############################
## 2.3 submit rf
##############################
pred.test <- predict(md.rf, newdata = x.test, type = "prob")[, 2]
dt.submit <- data.table(Account_ID = dt.test$ACCOUNT_ID, Prediction = pred.test)
dim(dt.submit)
# [1] 12935     2
dt.submit <- merge(dtSampleSubmit, dt.submit, by = "Account_ID", all.x = T, sort = F)
# [1] 7374    2
write.csv(dt.submit, "submit/20_051215_2207_rf_wit_random_3in1_preprocess_valid1_valid2_.csv", row.names = F) # 0.62786


#####################################################################
## 3. knn on random 3in1 ############################################
#####################################################################
##############################
## 3.1 take a sample #########
##############################
indTrain <- createDataPartition(dt.train$PRED
                                , p = .1
                                , list = F)

dt.sampleTrain <- dt.train[indTrain, with = T]
x.sampleTtrain <- model.matrix(PRED ~., dt.sampleTrain[, !c("ACCOUNT_ID"), with = F])[, -1]
y.sampleTrain <- dt.sampleTrain$PRED
##############################
## 3.2 train, valid, and test transform
##############################
# only choose the most important features for knn
# load("../Datathon_Full_Dataset/md_rf.RData")
# require(randomForest)
# varImp(md.rf)[order(-varImp(md.rf)[1]), ]
# varImpPlot(md.rf)

dt.train.knn <- dt.train[, !c("ACCOUNT_ID"), with = F]
dt.train.knn <- dt.train.knn[, c("PRE_RATE_WIN", "RPE_ATTENDED_ABSENT", "LAST_PROFIT_LOSS"
                                 , "THIS_AVG_TIME_DIFF", "THIS_MAX_BET_SIZE", "THIS_MIN_BET_SIZE"
                                 , "PRE_AVG_PROFIT_LOSS", "LAST_MIN_BET_SIZE"
                                 , "PRE_WIN_LOSE", "PRE_PERC_ATTENDING_EXPECTED_EVENT"
                                 , "PRED"), with = F]
indTrain <- createDataPartition(dt.train.knn$PRED
                                , p = .1
                                , list = F)

dt.sampleTrain.knn <- dt.train.knn[indTrain, with = T]

dt.valid1.knn <- dt.valid1[, !c("ACCOUNT_ID"), with = F]
dt.valid1.knn <- dt.valid1.knn[, c("PRE_RATE_WIN", "RPE_ATTENDED_ABSENT", "LAST_PROFIT_LOSS"
                                 , "THIS_AVG_TIME_DIFF", "THIS_MAX_BET_SIZE", "THIS_MIN_BET_SIZE"
                                 , "PRE_AVG_PROFIT_LOSS", "LAST_MIN_BET_SIZE"
                                 , "PRE_WIN_LOSE", "PRE_PERC_ATTENDING_EXPECTED_EVENT"
                                 , "PRED"), with = F]
dt.valid2.knn <- dt.valid2[, !c("ACCOUNT_ID"), with = F]
dt.valid2.knn <- dt.valid2.knn[, c("PRE_RATE_WIN", "RPE_ATTENDED_ABSENT", "LAST_PROFIT_LOSS"
                                   , "THIS_AVG_TIME_DIFF", "THIS_MAX_BET_SIZE", "THIS_MIN_BET_SIZE"
                                   , "PRE_AVG_PROFIT_LOSS", "LAST_MIN_BET_SIZE"
                                   , "PRE_WIN_LOSE", "PRE_PERC_ATTENDING_EXPECTED_EVENT"
                                   , "PRED"), with = F]

dt.test.knn <- dt.test[, !c("ACCOUNT_ID"), with = F]
dt.test.knn <- dt.test.knn[, c("PRE_RATE_WIN", "RPE_ATTENDED_ABSENT", "LAST_PROFIT_LOSS"
                                   , "THIS_AVG_TIME_DIFF", "THIS_MAX_BET_SIZE", "THIS_MIN_BET_SIZE"
                                   , "PRE_AVG_PROFIT_LOSS", "LAST_MIN_BET_SIZE"
                                   , "PRE_WIN_LOSE", "PRE_PERC_ATTENDING_EXPECTED_EVENT"), with = F]

require(xgboost)
require(Ckmeans.1d.dp)

x.sampleTtrain.knn <- model.matrix(PRED ~., dt.sampleTrain.knn)[, -1]
y.sampleTrain.knn <- as.integer(dt.sampleTrain.knn$PRED) - 1

x.train.knn <- model.matrix(PRED ~., dt.train.knn)[, -1]
y.train.knn <- as.integer(dt.train$PRED) - 1
dmx.train.knn <- xgb.DMatrix(data =  x.train.knn, label = y.train.knn)

x.valid1.knn <- model.matrix(PRED ~., dt.valid1.knn)[, -1]
y.valid1.knn <- as.integer(dt.valid1$PRED) - 1
dmx.valid1.knn <- xgb.DMatrix(data =  x.valid1.knn, label = y.valid1.knn)

x.valid2.knn <- model.matrix(PRED ~., dt.valid2.knn)[, -1]
y.valid2.knn <- as.integer(dt.valid2$PRED) - 1
dmx.valid2.knn <- xgb.DMatrix(data =  x.valid2.knn, label = y.valid2.knn)

x.test.knn <- model.matrix(~., dt.test.knn)[, -1]

##############################
## 3.3 model knn #############
##############################
require(class)

# for (k in 3:15){
#     md.knn <- knn(x.train.knn
#                   , x.valid2.knn
#                   , y.train.knn
#                   , k = k
#                   , prob = T)
#     auc <- colAUC(as.integer(md.knn), y.valid2.knn)
#     print(paste(k, ": AUC:", auc))
# }
# [1] "1 : AUC: 0.900681639540867"
# [1] "3 : AUC: 0.829170515734584"
# [1] "4 : AUC: 0.807709176583081"
# [1] "5 : AUC: 0.791309614971659"
# [1] "6 : AUC: 0.770815987204888"
# [1] "7 : AUC: 0.761724969916496"
# [1] "8 : AUC: 0.746936572425277"
# [1] "9 : AUC: 0.738621319582032"
# [1] "10 : AUC: 0.725412939148262"
# [1] "11 : AUC: 0.717709991977732"
# [1] "12 : AUC: 0.711296482559671"
# [1] "13 : AUC: 0.708463365989636"
# [1] "14 : AUC: 0.703074670904693"
# [1] "15 : AUC: 0.695688258924773"

# so try k = 3, since it can produce a proability
colTest.knn.3 <- knn(x.train.knn
                   , x.test.knn
                   , y.train.knn
                   , k = 3
                   , prob = T)
colTest.knn.3 <- attributes(colTest.knn.3)[["prob"]]
save(colTest.knn.3, file = "../Datathon_Full_Dataset/colTestKnn.RData")

dt.submit <- data.table(Account_ID = dt.test$ACCOUNT_ID, Prediction = colTest.knn.3)
dim(dt.submit)
# [1] 12935     2
dt.submit <- merge(dtSampleSubmit, dt.submit, by = "Account_ID", all.x = T, sort = F)
# [1] 7374    2
write.csv(dt.submit, "submit/51_091215_2044_knn_3_wit_random_3in1_preprocess_valid1_valid2_.csv", row.names = F) # 0.62786




