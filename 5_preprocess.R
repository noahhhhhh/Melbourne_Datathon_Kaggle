###########################################################################################
## preprocess #############################################################################
###########################################################################################
setwd("/Volumes/Data Science/Google Drive/data_science_competition/melbourne_datathon/Melbourne_Datathon_Kaggle/")
rm(list = ls()); gc()
require(data.table)
require(dplyr)
require(caret)
load("../Datathon_Full_Dataset/transformedData.RData")
#######################################
## 1.0 centre and scale ###############
#######################################
prep.centreScale <- preProcess(dt.3in1[, !c("ACCOUNT_ID"), with = F]
                           , verbose = T)
dt.centreScale <- predict(prep.centreScale, newdata = dt.3in1)
dim(dt.centreScale)
# [1] 333999    133

#######################################
## 2.0 simple feature cleansing #######
#######################################
###########
## 2.1 zero and near zero variance
###########
nzv <- nearZeroVar(dt.centreScale, saveMetrics= TRUE)
nzv[nzv$nzv, ]
#                                   freqRatio percentUnique  zeroVarnzv
# THIS_MAX_BET_SIZE_INPLAY_N        57.28617  8.1027188704   FALSE TRUE
# THIS_MIN_BET_SIZE_INPLAY_N        57.88065  6.0518145264   FALSE TRUE
# THIS_TOP_1                       126.30569  0.0011976084   FALSE TRUE
# THIS_TOP_2                       126.30569  0.0011976084   FALSE TRUE
# THIS_TOP_5                       126.30569  0.0011976084   FALSE TRUE
# THIS_TOP_10                      126.30569  0.0011976084   FALSE TRUE
# THIS_TOP_15                      126.30569  0.0011976084   FALSE TRUE
# THIS_TOP_20                      126.30569  0.0011976084   FALSE TRUE
# THIS_TOP_25                      126.30569  0.0011976084   FALSE TRUE
# THIS_BOTTOM_1                     74.28043  0.0008982063   FALSE TRUE
# THIS_BOTTOM_2                     35.52128  0.0011976084   FALSE TRUE
# PRE_MAX_PROFIT_LOSS              113.14260  9.0221827011   FALSE TRUE
# PRE_MIN_PROFIT_LOSS               42.00202  8.3664921152   FALSE TRUE
# LAST_STDEV_BET_SIZE               62.78602  0.8203617376   FALSE TRUE
# LAST_MAX_BET_SIZE_INPLAY_BET_N    53.57723  1.7284482888   FALSE TRUE
# LAST_MIN_BET_SIZE_INPLAY_BET_N    38.59447  0.8595834119   FALSE TRUE
# LAST_STDEV_BET_SIZE_INPLAY_BET_Y  60.64102  0.8161701083   FALSE TRUE
# LAST_STDEV_BET_SIZE_INPLAY_BET_N  60.64102  0.8161701083   FALSE TRUE
# LAST_TOP_1                       149.45000  0.0005988042   FALSE TRUE
# LAST_TOP_2                        68.78667  0.0005988042   FALSE TRUE
# LAST_TOP_5                        25.74131  0.0005988042   FALSE TRUE
# LAST_BOTTOM_1                    128.91015  0.0005988042   FALSE TRUE
# LAST_BOTTOM_2                     61.26678  0.0005988042   FALSE TRUE
# LAST_BOTTOM_5                     23.20458  0.0005988042   FALSE TRUE
# LAST_ODDS_1                       38.47118  0.0008982063   FALSE TRUE
# LAST_IS_FROM_WIN                  29.70690  0.0005988042   FALSE TRUE
# before
dim(dt.centreScale)
# [1] 333999    133
nzv <- nearZeroVar(dt.centreScale)
dt.centreScale.nzv <- dt.centreScale[, -nzv, with = F]
# after
dim(dt.centreScale.nzv)
# [1] 333999    107

###########
## 2.2 hight correlation
###########
numCols <- names(dt.centreScale.nzv)[sapply(dt.centreScale.nzv, class) %in% c("numeric", "integer")]
cor <- cor(dt.centreScale.nzv[, numCols, with = F])
highCor <- findCorrelation(cor, cutoff = .85, verbose = T, names = T)
highCor
# [1] "THIS_MAX_BET_SIZE"                   "THIS_AVG_TRANSACTION_COUNT"         
# [3] "THIS_MAX_TRANSACTION_COUNT"          "THIS_AVG_TRANSACTION_COUNT_INPLAY_Y"
# [5] "THIS_MAX_TRANSACTION_COUNT_INPLAY_Y" "LAST_MAX_BET_SIZE"                  
# [7] "THIS_MAX_STDEV_BET_SIZE_INPLAY_Y"    "THIS_MAX_STDEV_BET_SIZE_INPLAY_N"   
# [9] "THIS_AVG_STDEV_BET_SIZE_INPLAY_Y"    "THIS_STDEV_TRANSACTION_COUNT"       
# [11] "THIS_NO_OF_BID_TYPE"                 "THIS_MAX_STDEV_BET_SIZE"            
# [13] "THIS_AVG_BET_SIZE"                   "LAST_TRANSACTION_COUNT"             
# [15] "THIS_MAX_TRANSACTION_COUNT_INPLAY_N" "THIS_MIN_TRANSACTION_COUNT"         
# [17] "THIS_STDEV_STDEV_BET_SIZE_INPLAY_Y"  "THIS_AVG_TRANSACTION_COUNT_INPLAY_N"
# [19] "THIS_MIN_STDEV_BET_SIZE_INPLAY_Y"    "LAST_AVG_BET_SIZE"                  
# [21] "THIS_MAX_TIME_DIFF"                  "THIS_AVG_TIME_DIFF"                 
# [23] "THIS_BOTTOM_25"                      "THIS_BOTTOM_20"                     
# [25] "THIS_MAX_ODDS_2"                     "THIS_AVG_ODDS_1"                    
# [27] "THIS_STDEV_ODDS_1"                   "THIS_RESULT_SUPRISED"               
# before
dim(dt.centreScale.nzv)
# [1] 333999    107
dt.centreScale.nzv.highCor <- dt.centreScale.nzv[, -highCor, with = F]
# afrer
dim(dt.centreScale.nzv.highCor)
# [1] 333999     79
numCols <- names(dt.centreScale.nzv.highCor)[sapply(dt.centreScale.nzv.highCor, class) %in% c("numeric", "integer")]

###########
## 2.3 linear dependencies
###########
linearInfo <- findLinearCombos(dt.centreScale.nzv.highCor[, numCols, with = F])
numCols[linearInfo$remove]
# [1] "THIS_IN_AND_OUT_PLAY"              "PRE_PERC_ATTENDING_SUPRISED_EVENT"
# before
dim(dt.centreScale.nzv.highCor)
# [1] 333999     79
dt.centreScale.nzv.highCor.linearDep <- dt.centreScale.nzv.highCor[, -numCols[linearInfo$remove], with = F]
# after
dim(dt.centreScale.nzv.highCor.linearDep)
# [1] 333999     77

###########
## 2.4 treat as original set
###########
dt.centreScale <- dt.centreScale.nzv.highCor.linearDep
dim(dt.centreScale)
# [1] 333999     77
#######################################
## 3.0 train, valid, and test set #####
#######################################
###########
## train ##
###########
dt.train <- dt.centreScale[!UNIT %in% c("38_39_40" # valid set 1 related
                                        , "12_13_14" # valid set 2 related
                                        , "42_43_44", "43_44_45", "44_45_46") # test set related
                         , with = T]
dt.train[, UNIT := NULL]
# dt.train[, ACCOUNT_ID := NULL]
dt.train[, THIS_PROFIT_LOSS := NULL]
str(dt.train)
dim(dt.train)
# [1] 283477     75

############
## valid1 ##
############
dt.valid1 <- dt.centreScale[UNIT == c("38_39_40"), with = T]
dt.valid1[, UNIT := NULL]
# dt.valid[, ACCOUNT_ID := NULL]
dt.valid1[, THIS_PROFIT_LOSS := NULL]
str(dt.valid1)
dim(dt.valid1)
# [1] 6791   75

############
## valid2 ##
############
dt.valid2 <- dt.centreScale[UNIT == c("12_13_14"), with = T]
dt.valid2[, UNIT := NULL]
# dt.valid[, ACCOUNT_ID := NULL]
dt.valid2[, THIS_PROFIT_LOSS := NULL]
str(dt.valid2)
dim(dt.valid2)
# [1] 7780   75
# str(dt.valid2)

##########
## test ##
##########
dt.test <- dt.centreScale[UNIT == c("44_45_46"), with = T]
dt.test[, UNIT := NULL]
# dt.test[, ACCOUNT_ID := NULL]
dt.test[, THIS_PROFIT_LOSS := NULL]
dt.test[, PRED := NULL]
str(dt.test)
dim(dt.test)
# [1] 12935    74

#######################################
## 3,0 save ###########################
#######################################
save(dt.train, file = "../Datathon_Full_Dataset/trainData.RData")
save(dt.valid1, file = "../Datathon_Full_Dataset/valid1Data.RData")
save(dt.valid2, file = "../Datathon_Full_Dataset/valid2Data.RData")
save(dt.test, file = "../Datathon_Full_Dataset/testData.RData")




