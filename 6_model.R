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
## 1.1 filter features #######
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
## 1.2 scale #################
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
## 1.3 train and test ########
##############################
# train
dt.train <- dt.scaledAll[!EVENT_SEQ %in% c(43, 44, 45, 46), with = T]
dt.train <- dt.train[, EVENT_SEQ := NULL]
dim(dt.train)
# [1] 169817     89
dt.valid <- dt.scaledAll[EVENT_SEQ == 43, with = T]
dt.valid <- dt.valid[, EVENT_SEQ := NULL]
dim(dt.valid)
# [1] 4409   89

##############################
## 1.4 model - the lasso #####
##############################
require(glmnet)
x <- model.matrix(PROFIT_LOSS ~., dt.train)[, -1]
y <- dt.train$PROFIT_LOSS
grid <- 10^seq(10, -2, length = 100)

# train
md.lasso <- glmnet(x, y, alpha = 1, lambda = grid, standardize = F)
plot(md.lasso)

# cv to choose λ
set.seed(1)
cv.out <- cv.glmnet(x, y, alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min

# valid
x.valid <- model.matrix(PROFIT_LOSS ~., dt.valid)[, -1]
pred.lasso <- predict(md.lasso ,s = bestlam ,newx = x.valid)
mean((pred.lasso - dt.valid$PROFIT_LOSS)^2)



