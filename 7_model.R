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
load("../Datathon_Full_Dataset/engineeredData.RData")
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
trainControlxgbTree <- trainControl(method = "boot",number = 4,repeats = 1,#summaryFunction = twoClassSummary,
                                    verboseIter = TRUE,
                                    preProcOptions = list(thresh = 0.999,ICAcomp = 111)
)

set.seed(1)
md.xgboost1 <- xgb.train(data = dmx.train
                      , params = list(nthread = 8
                                      , eval_metric = "auc"
                                      , eta = .05
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
                                         , eta = .05
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
                                         , eta = .05
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
                                         , eta = .05
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
                                         , eta = .05
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
                                         , eta = .05
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
                                         , eta = .05
                                         , subsample = .8
                                         , colsample_bytree = .3)
                         , watchlist = list(eval1 = dmx.valid1
                                            , eval2 = dmx.valid2
                                            , train = dmx.train)
                         , nrounds = 1700
                         , verbose = T)

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
# 0 vs. 1 0.8834542
colAUC(pred.valid1.2, y.valid1)
# [,1]
# 0 vs. 1 0.8841164
colAUC(pred.valid1.3, y.valid1)
# [,1]
# 0 vs. 1 0.8838919
colAUC(pred.valid1.4, y.valid1)
# [,1]
# 0 vs. 1 0.8816272
colAUC(pred.valid1.5, y.valid1)
# [,1]
# 0 vs. 1 0.8829494
colAUC(pred.valid1.6, y.valid1)
# [,1]
# 0 vs. 1 0.8838063
colAUC(pred.valid1.7, y.valid1)
# [,1]
# 0 vs. 1 0.8833958

importance_matrix1 <- xgb.importance(colnames(x.train), model = md.xgboost1)
print(importance_matrix1)
xgb.plot.importance(importance_matrix1)

pred.valid1 <- pred.valid1.1 + pred.valid1.2 + pred.valid1.3 + pred.valid1.4
                   + pred.valid1.5 + pred.valid1.6 + pred.valid1.7
colAUC(pred.valid1, y.valid1)

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
# 0 vs. 1 0.7797133
colAUC(pred.valid2.2, y.valid2)
# [,1]
# 0 vs. 1 0.7853802
colAUC(pred.valid2.3, y.valid2)
# [,1]
# 0 vs. 1 0.7793123
colAUC(pred.valid2.4, y.valid2)
# [,1]
# 0 vs. 1 0.7816157
colAUC(pred.valid2.5, y.valid2)
# [,1]
# 0 vs. 1 0.7846166
colAUC(pred.valid2.6, y.valid2)
# [,1]
# 0 vs. 1 0.7792754
colAUC(pred.valid2.7, y.valid2)
# [,1]
# 0 vs. 1 0.782432

pred.valid2 <- pred.valid2.1 + pred.valid2.2 + pred.valid2.3 + pred.valid2.4
                   + pred.valid2.5 + pred.valid2.6 + pred.valid2.7
colAUC(pred.valid2, y.valid2)

##############################
## 3.3 submit
##############################
pred.test1 <- predict(md.xgboost1, x.test)
pred.test2 <- predict(md.xgboost2, x.test)
pred.test3 <- predict(md.xgboost3, x.test)
pred.test4 <- predict(md.xgboost4, x.test)
pred.test5 <- predict(md.xgboost5, x.test)
pred.test6 <- predict(md.xgboost6, x.test)
pred.test7 <- predict(md.xgboost7, x.test)

pred.test <- pred.test1 + pred.test2 + pred.test3 + pred.test4
                 + pred.test5 + pred.test6 + pred.test7

table(pred.test)
dt.submit <- data.table(Account_ID = dt.test$ACCOUNT_ID, Prediction = pred.test)
dt.submit <- merge(dtSampleSubmit, dt.submit, by = "Account_ID", all.x = T, sort = F)
write.csv(dt.submit, "submit/7_281115_1041_7_xgboost_with_3in1_valid1_valid2_.csv", row.names = F) # 0.61456




























