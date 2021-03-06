###########################################################################################
## modelling ##############################################################################
###########################################################################################
setwd("/Volumes/Data Science/Google Drive/data_science_competition/melbourne_datathon/Melbourne_Datathon_Kaggle/")
rm(list = ls()); gc()
require(data.table)
require(dplyr)
require(caret)
require(caTools)
# load("../Datathon_Full_Dataset/trainData.RData")
# load("../Datathon_Full_Dataset/valid1Data.RData")
# load("../Datathon_Full_Dataset/valid2Data.RData")
# load("../Datathon_Full_Dataset/testData.RData")
load("../Datathon_Full_Dataset/trainData_less.RData")
load("../Datathon_Full_Dataset/valid1Data_less.RData")
load("../Datathon_Full_Dataset/valid2Data_less.RData")
load("../Datathon_Full_Dataset/testData_less.RData")
# dtTestFeatures <- fread("../data_files/semi_and_final_features.csv")
dtSampleSubmit <- fread("../data_files/sample_submission_bet_size.csv")
dtSampleSubmit[, Prediction := NULL]
setnames(dtSampleSubmit, "Account_ID")
#####################################################################
## 1. try using all 3in1 the data set except the test set to train a rf ## does not perform well (0.56465)
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
## 2. try 1.1 data on lasso regression ##############################
#####################################################################
##############################
## 2.1 filter features #######
##############################
dt.ready <- dt1.1[, !c("EVENT_ID", "COUNTRY_OF_RESIDENCE_NAME", "MATCH"
                       , "OFF_DT", "MIN_PLCAED_DT_PER_MATCH", "RANK", "NO_OF_ACCT", "INPLAY_BET", "RESULT"
                       , "CUM_RESULT_EXPECTED", "CUM_RESULT_SUPRISED", "TIMES_ATTENDING_EXPECTED_EVENT"
                       , "TIMES_ATTENDING_SUPRISED_EVENT", "ATTENDED", "CUM_ATTENDED", "NO_OF_EVENT_ATTENDED"
                       , "NO_OF_EVENT_ATTENDED", "NO_OF_EVENT_ABSENT", "CUM_INPLAY_Y" 
                       , "CUM_INPLAY_N", "TIMES_INPLAY_Y", "TIMES_INPLAY_N", "IND_WIN", "IND_LOSE", "CUM_WIN"
                       , "NO_OF_WIN", "CUM_LOSE", "NO_OF_LOSE", "CUM_PROFIT_LOSS", "TTL_PROFIT_LOSS"
                       , "CUM_AVG_PROFIT_LOSS", "CUM_MAX_PROFIT_LOSS", "CUM_MIN_PROFIT_LOSS"
                       , "CUM_ME2ME", "TIMES_BEING_A_ME2ME", "CUM_IN_AND_OUT_PLAY", "TIMES_IN_AND_OUT_PLAY"
                       , "WIN_TEAM", "LOSE_TEAM"), with = F]

dim(dt.ready)
# [1] 197202     90
##############################
## 3.2 scale #################
##############################
facts <- sapply(dt.ready, is.factor)
dt.readyForScale <- dt.ready[, !facts, with = F]
dim(dt.readyForScale)
# [1] 197202     59
dt.scaledAll <- as.data.table(scale(dt.readyForScale[, !c("EVENT_SEQ", "ACCOUNT_ID"
                                                 , "PROFIT_LOSS"), with = F]))
dim(dt.scaledAll)
# [1] 197202     56
dt.scaledAll <- cbind(dt.ready[, c("EVENT_SEQ", "ACCOUNT_ID"
                                    , "PROFIT_LOSS"), with = F]
                      , dt.ready[, facts, with = F]
                      , dt.scaledAll)
dim(dt.scaledAll)
# [1] 197202     90

##############################
## 3.3 train and test ########
##############################
# train
dt.train <- dt.scaledAll[!EVENT_SEQ %in% c(43, 44, 45, 46), with = T]
dt.train <- dt.train[, EVENT_SEQ := NULL]
dt.train[, PRED := as.factor(ifelse(PROFIT_LOSS > 0, 1, 0))]
dt.train[, PROFIT_LOSS := NULL]
dim(dt.train)
# [1] 169817     89
dt.valid <- dt.scaledAll[EVENT_SEQ == 43, with = T]
dt.valid <- dt.valid[, EVENT_SEQ := NULL]
dt.valid[, PRED := as.factor(ifelse(PROFIT_LOSS > 0, 1, 0))]
dt.valid[, PROFIT_LOSS := NULL]
dim(dt.valid)
# [1] 4409   89

##############################
## 4.4 model - the lasso #####
##############################
require(glmnet)
x.train <- model.matrix(PRED ~., dt.train[, !c("ACCOUNT_ID"), with = F])[, -1]
y.train <- dt.train$PRED
grid <- 10^seq(10, -2, length = 100)

# train
md.lasso <- glmnet(x.train, y.train, alpha = 1, lambda = grid, family = "binomial")
plot(md.lasso)

# cv to choose λ
set.seed(1)
cv.out <- cv.glmnet(x.train, y.train, alpha = 1, type.measure = "auc", family = "binomial")
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam

# valid
x.valid <- model.matrix(PRED ~., dt.valid[, !c("ACCOUNT_ID"), with = F])[, -1]
y.valid <- dt.valid$PRED
pred.lasso <- predict(md.lasso , s = bestlam , newx = x.valid)
colAUC(pred.lasso, y.valid)
# 1
# 0 vs. 1 0.7686733
# mean((pred.lasso - dt.valid$PROFIT_LOSS)^2)
coef.lasso <- predict(md.lasso, type = "coefficients", s = bestlam)
coef.lasso
coef.lasso[coef.lasso != 0]

#####################################################################
## 3. try 3in1 data on xgboost ######################################
#####################################################################
##############################
## 3.1 train, valid, and test transform
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
## 3.2 model - xgboost
##############################
set.seed(1)
md.xgboost1 <- xgb.train(data = dmx.train
                      , params = list(nthread = 8
                                      , eval_metric = "auc"
                                      , eta = .025
                                      , max_depth = 6
                                      , subsample = .8
                                      , colsample_bytree = .3)
                      , watchlist = list(eval1 = dmx.valid1
                                         , eval2 = dmx.valid2
                                         , train = dmx.train)
                      , nrounds = 1700
                      , verbose = T)

set.seed(2)
md.xgboost2 <- xgb.train(data = dmx.train
                         , params = list(nthread = 8
                                         , eval_metric = "auc"
                                         , eta = .025
                                         , subsample = .8
                                         , colsample_bytree = .3)
                         , watchlist = list(eval1 = dmx.valid1
                                            , eval2 = dmx.valid2
                                            , train = dmx.train)
                         , nrounds = 1700
                         , verbose = T)

set.seed(3)
md.xgboost3 <- xgb.train(data = dmx.train
                         , params = list(nthread = 8
                                         , eval_metric = "auc"
                                         , eta = .025
                                         , subsample = .8
                                         , colsample_bytree = .3)
                         , watchlist = list(eval1 = dmx.valid1
                                            , eval2 = dmx.valid2
                                            , train = dmx.train)
                         , nrounds = 1700
                         , verbose = T)

set.seed(4)
md.xgboost4 <- xgb.train(data = dmx.train
                         , params = list(nthread = 8
                                         , eval_metric = "auc"
                                         , eta = .025
                                         , subsample = .8
                                         , colsample_bytree = .3)
                         , watchlist = list(eval1 = dmx.valid1
                                            , eval2 = dmx.valid2
                                            , train = dmx.train)
                         , nrounds = 1700
                         , verbose = T)

set.seed(5)
md.xgboost5 <- xgb.train(data = dmx.train
                         , params = list(nthread = 8
                                         , eval_metric = "auc"
                                         , eta = .025
                                         , subsample = .8
                                         , colsample_bytree = .3)
                         , watchlist = list(eval1 = dmx.valid1
                                            , eval2 = dmx.valid2
                                            , train = dmx.train)
                         , nrounds = 1700
                         , verbose = T)

set.seed(6)
md.xgboost6 <- xgb.train(data = dmx.train
                         , params = list(nthread = 8
                                         , eval_metric = "auc"
                                         , eta = .025
                                         , subsample = .8
                                         , colsample_bytree = .3)
                         , watchlist = list(eval1 = dmx.valid1
                                            , eval2 = dmx.valid2
                                            , train = dmx.train)
                         , nrounds = 1700
                         , verbose = T)

set.seed(7)
md.xgboost7 <- xgb.train(data = dmx.train
                         , params = list(nthread = 8
                                         , eval_metric = "auc"
                                         , eta = .025
                                         , subsample = .8
                                         , colsample_bytree = .3)
                         , watchlist = list(eval1 = dmx.valid1
                                            , eval2 = dmx.valid2
                                            , train = dmx.train)
                         , nrounds = 1700
                         , verbose = T)

logit2prop <- function(l){exp(l)/(1+exp(l))}

# valid1
pred.valid1.1 <- predict(md.xgboost1, x.valid1)
pred.valid1.2 <- predict(md.xgboost2, x.valid1)
pred.valid1.3 <- predict(md.xgboost3, x.valid1)
pred.valid1.4 <- predict(md.xgboost4, x.valid1)
pred.valid1.5 <- predict(md.xgboost5, x.valid1)
pred.valid1.6 <- predict(md.xgboost6, x.valid1)
pred.valid1.7 <- predict(md.xgboost7, x.valid1)

colAUC(pred.valid1.1, y.valid1)
# [,1]
# 0 vs. 1 0.851747
# [,1]
# 0 vs. 1 0.8590526
colAUC(pred.valid1.2, y.valid1)
# [,1]
# 0 vs. 1 0.8510586
# [,1]
# 0 vs. 1 0.8570585
colAUC(pred.valid1.3, y.valid1)
# [,1]
# 0 vs. 1 0.851204
# [,1]
# 0 vs. 1 0.8593713
colAUC(pred.valid1.4, y.valid1)
# [,1]
# 0 vs. 1 0.851204
colAUC(pred.valid1.5, y.valid1)
# [,1]
# 0 vs. 1 0.8477428
# [,1]
# 0 vs. 1 0.8597132
colAUC(pred.valid1.6, y.valid1)
# [,1]
# 0 vs. 1 0.8491994
# [,1]
# 0 vs. 1 0.8588805
colAUC(pred.valid1.7, y.valid1)
# [,1]
# 0 vs. 1 0.8499412
# [,1]
# 0 vs. 1 0.8585816

importance_matrix1 <- xgb.importance(colnames(x.train), model = md.xgboost1)
print(importance_matrix1)
xgb.plot.importance(importance_matrix1)

pred.valid1 <- logit2prop(pred.valid1.1) + logit2prop(pred.valid1.2) + logit2prop(pred.valid1.3) + logit2prop(pred.valid1.4)
                   + logit2prop(pred.valid1.5) + logit2prop(pred.valid1.6) + logit2prop(pred.valid1.7)
colAUC(pred.valid1, y.valid1)
# [,1]
# 0 vs. 1 0.8520213
# [,1]
# 0 vs. 1 0.8596633

# valid2
pred.valid2.1 <- predict(md.xgboost1, x.valid2)
pred.valid2.2 <- predict(md.xgboost2, x.valid2)
pred.valid2.3 <- predict(md.xgboost3, x.valid2)
pred.valid2.4 <- predict(md.xgboost4, x.valid2)
pred.valid2.5 <- predict(md.xgboost5, x.valid2)
pred.valid2.6 <- predict(md.xgboost6, x.valid2)
pred.valid2.7 <- predict(md.xgboost7, x.valid2)

colAUC(pred.valid2.1, y.valid2)
# [,1]
# 0 vs. 1 0.7292966
# [,1]
# 0 vs. 1 0.7369436
colAUC(pred.valid2.2, y.valid2)
# [,1]
# 0 vs. 1 0.7313229
# [,1]
# 0 vs. 1 0.735727
colAUC(pred.valid2.3, y.valid2)
# [,1]
# 0 vs. 1 0.7305871
# [,1]
# 0 vs. 1 0.7358102
colAUC(pred.valid2.4, y.valid2)
# [,1]
# 0 vs. 1 0.7323578
# [,1]
# 0 vs. 1 0.7357924
colAUC(pred.valid2.5, y.valid2)
# [,1]
# 0 vs. 1 0.7304927
# [,1]
# 0 vs. 1 0.7353548
colAUC(pred.valid2.6, y.valid2)
# [,1]
# 0 vs. 1 0.7307749
# [,1]
# 0 vs. 1 0.7350671
colAUC(pred.valid2.7, y.valid2)
# [,1]
# 0 vs. 1 0.7333653
# [,1]
# 0 vs. 1 0.7356207

pred.valid2 <- logit2prop(pred.valid2.1) + logit2prop(pred.valid2.2) + logit2prop(pred.valid2.3) + logit2prop(pred.valid2.4)
                   + logit2prop(pred.valid2.5) + logit2prop(pred.valid2.6) + logit2prop(pred.valid2.7)
colAUC(pred.valid2, y.valid2)
# [,1]
# 0 vs. 1 0.7341297
# [,1]
# 0 vs. 1 0.7366729
##############################
## 3.3 submit
##############################
pred.test1 <- logit2prop(predict(md.xgboost1, x.test))
pred.test2 <- logit2prop(predict(md.xgboost2, x.test))
pred.test3 <- logit2prop(predict(md.xgboost3, x.test))
pred.test4 <- logit2prop(predict(md.xgboost4, x.test))
pred.test5 <- logit2prop(predict(md.xgboost5, x.test))
pred.test6 <- logit2prop(predict(md.xgboost6, x.test))
pred.test7 <- logit2prop(predict(md.xgboost7, x.test))

pred.test <- pred.test1 + pred.test2 + pred.test3 + pred.test4
                 + pred.test5 + pred.test6 + pred.test7
pred.test.tree <- pred.test
table(pred.test)
dt.submit <- data.table(Account_ID = dt.test$ACCOUNT_ID, Prediction = pred.test)
dim(dt.submit)
# [1] 12935     2
dt.submit <- merge(dtSampleSubmit, dt.submit, by = "Account_ID", all.x = T, sort = F)
# [1] 7374    2
write.csv(dt.submit, "submit/12_301115_1837_7_xgboost_combined_prob_with_random_3in1_preprocess_valid1_valid2_.csv", row.names = F) # 0.62915

#####################################################################
## 4. try 3in1 non-overlap data on automated xgboost ################
#####################################################################
##############################
## 4.1 train, valid, and test transform
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
## 4.2 model - xgboost
##############################
eta <- c(.01, .015, .02, .025, .03)
max_depth <- c(8, 7, 7, 6, 5)
min.child <- c(5, 5, 5, 5, 5)
subsample <- c(.8, .85, .90, .95, .1)
colsample_bytree <- c(.3, .35, .4, .45, .5)
nrounds <- c(2000, 1800, 1600, 1400, 1200)

models <- 5
reps <- 10

pred.test <- rep(0, nrow(dtSampleSubmit))
for (i in 1:reps){
    print(paste("rep", i, "- Start"))
    for(j in 1:models){
        print(paste("------model", j, ": Start"))
        set.seed(1000 * i + 100 * j)
        md.xgboost <- xgb.train(data = dmx.train
                                 , params = list(nthread = 8
                                                 , eval_metric = "auc"
                                                 , eta = eta[j]
                                                 , max_depth = max_depth[j]
                                                 , subsample = subsample[j]
                                                 , colsample_bytree = colsample_bytree[j])
                                 , watchlist = list(eval1 = dmx.valid1
                                                    , eval2 = dmx.valid2
                                                    , train = dmx.train)
                                 , nrounds = nrounds[j]
                                 , verbose = T)
        pred.test <- pred.test + predict(md.xgboost, x.test)
        print(paste("------model", j, ": End"))
    }
    print(paste("rep", i, "- End"))
}
pred.test

##############################
## 4.3 submit
##############################
dt.submit <- data.table(Account_ID = dt.test$ACCOUNT_ID, Prediction = pred.test)
dim(dt.submit)
# [1] 12935     2
dt.submit <- merge(dtSampleSubmit, dt.submit, by = "Account_ID", all.x = T, sort = F)
# [1] 7374    2
write.csv(dt.submit, "submit/9_291115_0944_50_xgboost_with_non_overlap_3in1_preprocess_valid1_valid2_.csv", row.names = F) # 0.61628

#####################################################################
## 5. try 3in1 data on xgboost - regression #########################
#####################################################################
##############################
## 5.1 train, valid, and test transform
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
## 5.2 model - xgboost - gblinear
##############################
set.seed(1)
md.xgboost1.gblinear <- xgb.train(data = dmx.train
                         , booster = "gblinear"
                         , params = list(nthread = 8
                                         , eval_metric = "auc"
                                         , eta = .025
                                         , max_depth = 6
                                         , subsample = .8
                                         , colsample_bytree = .3)
                         , watchlist = list(eval1 = dmx.valid1
                                            , eval2 = dmx.valid2
                                            , train = dmx.train)
                         , nrounds = 1700
                         , verbose = T)

# valid1
pred.valid1.1 <- predict(md.xgboost1.gblinear, x.valid1)

colAUC(pred.valid1.1, y.valid1)
# [,1]
# 0 vs. 1 0.7264964
importance_matrix1 <- xgb.importance(colnames(x.train), model = md.xgboost1)
print(importance_matrix1)
xgb.plot.importance(importance_matrix1)

# valid2
pred.valid2.1 <- predict(md.xgboost1.gblinear, x.valid2)

colAUC(pred.valid2.1, y.valid2)
# [,1]
# 0 vs. 1 0.6307768

pred.test <- predict(md.xgboost1.gblinear, x.test)
pred.test.linear <- pred.test * 7
pred.test.linear
pred.test.tree
pred.test <- pred.test.linear + pred.test.tree
##############################
## 5.3 submit
##############################
dt.submit <- data.table(Account_ID = dt.test$ACCOUNT_ID, Prediction = pred.test)
dim(dt.submit)
# [1] 12935     2
dt.submit <- merge(dtSampleSubmit, dt.submit, by = "Account_ID", all.x = T, sort = F)
# [1] 7374    2
write.csv(dt.submit, "submit/13_301115_1842_7_tree_xgboost_and_1_linear_xgboost_with_combined_3in1_preprocess_valid1_valid2_.csv", row.names = F) # 0.61693

#####################################################################
## 6. try 3in1 random data on automated xgboost #####################
#####################################################################
##############################
## 6.1 train, valid, and test transform
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
## 6.2 model - xgboost
#############################
eta <- rep(.025, 7)
# max_depth <- c(8, 7, 7, 6, 5)
# min.child <- c(5, 5, 5, 5, 5)
subsample <- rep(.8, 7)
colsample_bytree <- rep(.3, 7)
nrounds <- rep(1700, 7)

reps <- 1
models <- 7

pred.test <- rep(0, nrow(dtSampleSubmit))

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
        pred.test <- pred.test + predict(md.xgboost, x.test)
        print(paste("------model", j, ": End"))
    }
    print(paste("rep", i, "- End"))
}
# average valid1 = 0.842147
# average valid2 = 0.725920
pred.test
summary(pred.test)
##############################
## 6.3 submit
##############################
dt.submit <- data.table(Account_ID = dt.test$ACCOUNT_ID, Prediction = pred.test)
dim(dt.submit)
# [1] 12935     2
dt.submit <- merge(dtSampleSubmit, dt.submit, by = "Account_ID", all.x = T, sort = F)
# [1] 7374    2
write.csv(dt.submit, "submit/14_011215_0718_7_xgboost_binary_logits_with_random_3in1_preprocess_valid1_valid2_.csv", row.names = F) # 0.63186

##############################
## 6.4 ensemble with linear boost
##############################
set.seed(1)
md.xgboost1.gblinear <- xgb.train(data = dmx.train
                                  , booster = "gblinear"
                                  , objective = "binary:logistic"
                                  , params = list(nthread = 8
                                                  , eval_metric = "auc"
                                                  , eta = .025
                                                  , max_depth = 6
                                                  , subsample = .8
                                                  , colsample_bytree = .3)
                                  , watchlist = list(eval1 = dmx.valid1
                                                     , eval2 = dmx.valid2
                                                     , train = dmx.train)
                                  , nrounds = 1700
                                  , verbose = T)

# valid1
pred.valid1.1 <- predict(md.xgboost1.gblinear, x.valid1)

colAUC(pred.valid1.1, y.valid1)
# [,1]
# 0 vs. 1 0.7274866
importance_matrix1 <- xgb.importance(colnames(x.train), model = md.xgboost1.gblinear)
print(importance_matrix1)
xgb.plot.importance(importance_matrix1)

# valid2
pred.valid2.1 <- predict(md.xgboost1.gblinear, x.valid2)

colAUC(pred.valid2.1, y.valid2)
# [,1]z
# 0 vs. 1 0.6308655

pred.test.linear <- predict(md.xgboost1.gblinear, x.test)
pred.test.ensemble <- pred.test.linear * 7 + pred.test

dt.submit <- data.table(Account_ID = dt.test$ACCOUNT_ID, Prediction = pred.test.ensemble)
dim(dt.submit)
# [1] 12935     2
dt.submit <- merge(dtSampleSubmit, dt.submit, by = "Account_ID", all.x = T, sort = F)
# [1] 7374    2
write.csv(dt.submit, "submit/15_011215_0818_7_tree_xgboost_binary_logits_and_1_linear_xgboost_binary_logits_with_random_3in1_preprocess_valid1_valid2_.csv", row.names = F) # 0.61628

#####################################################################
## 7. the logistic regression, and lasso, again #####################
#####################################################################
require(glmnet)
##############################
## 7.1 train, valid, and test transform
##############################
x.train <- model.matrix(PRED ~., dt.train[, !c("ACCOUNT_ID"), with = F])[, -1]
y.train <- ifelse(as.integer(dt.train$PRED) == 1, 0, 1)

x.valid1 <- model.matrix(PRED ~., dt.valid1[, !c("ACCOUNT_ID"), with = F])[, -1]
y.valid1 <- ifelse(as.integer(dt.valid1$PRED) == 1, 0, 1)

x.valid2 <- model.matrix(PRED ~., dt.valid2[, !c("ACCOUNT_ID"), with = F])[, -1]
y.valid2 <- ifelse(as.integer(dt.valid2$PRED) == 1, 0, 1)

x.test <- model.matrix(~., dt.test[, !c("ACCOUNT_ID"), with = F])[, -1]

##############################
## 7.2 model lasso ###########
##############################
cv.lasso.out <- cv.glmnet(x = x.train
                          , y = y.train
                          , alpha = 1
                          , family = "binomial"
                          , lambda = seq(1e-6, 1e-3, by = 1e-5)
                          , type.measure = "auc"
                          , nfolds = 10
)

cv.lasso.out$lambda.min
# [1] 8.1e-05
md.lasso <- glmnet(x = x.train
                  , y= y.train
                  , alpha = 1
                  , family = "binomial"
                  , lambda = cv.lasso.out$lambda.min)

pred.valid1 <- predict(md.lasso, s = cv.lasso.out$lambda.min, newx = x.valid1)
colAUC(pred.valid1, y.valid1)
# 1
# 0 vs. 1 0.7264308
pred.valid2 <- predict(md.lasso, s = cv.lasso.out$lambda.min, newx = x.valid2)
colAUC(pred.valid2, y.valid2)
# 1
# 0 vs. 1 0.6304754

##############################
## 7.3 model logistic regression
##############################
md.lr <- glm(PRED ~.
             , family = binomial
             , data = dt.train[, !c("ACCOUNT_ID"), with = F])

pred.train <- predict(md.lr)
colAUC(pred.train, y.train)
# [,1]
# 0 vs. 1 0.6134507
pred.valid1 <- predict(md.lr, newdata = dt.valid1)
colAUC(pred.valid1, y.valid1)
# [,1]
# 0 vs. 1 0.7264089
pred.valid2 <- predict(md.lr, newdata = dt.valid2, type = "response")
colAUC(pred.valid2, y.valid2)
# [,1]
# 0 vs. 1 0.6303594

##############################
## 7.4 submit
##############################
# lasso and the simple logistic regression outpus similar results, so use either
# lasso
pred.test <- predict(md.lasso, s = cv.lasso.out$lambda.min, newx = x.test)
dt.submit <- data.table(Account_ID = dt.test$ACCOUNT_ID, Prediction = pred.test)
colnames(dt.submit) <- c("Account_ID", "Prediction")
dim(dt.submit)
# [1] 12935     2
dt.submit <- merge(dtSampleSubmit, dt.submit, by = "Account_ID", all.x = T, sort = F)
# [1] 7374    2
write.csv(dt.submit, "submit/16_021215_0855_the_lasso_lr_with_random_3in1_preprocess_valid1_valid2_.csv", row.names = F) # 0.61734

################################
## 7.5 ensemble with xgboost and submit
##############################
dt.submit.lasso <- dt.submit
dt.submit.xgboost <- fread("submit/15_011215_0818_7_tree_xgboost_binary_logits_and_1_linear_xgboost_binary_logits_with_random_3in1_preprocess_valid1_valid2_.csv")
dt.submit.ensemble <- data.table(Account_ID = dt.submit.xgboost$Account_ID
                                 , Prediction = dt.submit.lasso$Prediction * 7 + dt.submit.xgboost$Prediction)
dim(dt.submit.ensemble)
# [1] 7374    2
write.csv(dt.submit.ensemble, "submit/17_021215_0912_ensembled_xgboost_and_lasso_lr_with_random_3in1_preprocess_valid1_valid2_.csv", row.names = F) # 0.62556




