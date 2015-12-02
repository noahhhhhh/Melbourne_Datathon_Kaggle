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
load("../Datathon_Full_Dataset/trainData_less.RData")
load("../Datathon_Full_Dataset/valid1Data_less.RData")
load("../Datathon_Full_Dataset/valid2Data_less.RData")
load("../Datathon_Full_Dataset/testData_less.RData")
# dtTestFeatures <- fread("../data_files/semi_and_final_features.csv")
dtSampleSubmit <- fread("../data_files/sample_submission_bet_size.csv")
dtSampleSubmit[, Prediction := NULL]
setnames(dtSampleSubmit, "Account_ID")

#####################################################################
## 1. xgboost on more random 3in1 ###################################
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
## 1.2 model - xgboost
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
write.csv(dt.submit, "submit/18_021215_2150_7_xgboost_binary_logits_with_more_random_3in1_preprocess_valid1_valid2_.csv", row.names = F) # 0.63120

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
## 2.2 model
##############################
set.seed(1)
md.rf <- randomForest(x = x.train
                      , y = y.train
                      , xtest = x.valid2
                      , ytest = y.valid2
                      , ntree = 1000
                      , mtry = floor(sqrt(ncol(x.train)))
                      , replace = T
                      , nodesize = 100
                      , importance = T
                      )

















