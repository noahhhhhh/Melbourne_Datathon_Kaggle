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

##############################
## 1.1 pca ###################
##############################

##############################
## 1.3 train, valid, and test 
##############################
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
# [1] 283477    131

############
## valid1 ##
############
dt.valid1 <- dt.centreScale[UNIT == c("38_39_40"), with = T]
dt.valid1[, UNIT := NULL]
# dt.valid[, ACCOUNT_ID := NULL]
dt.valid1[, THIS_PROFIT_LOSS := NULL]
str(dt.valid1)
dim(dt.valid1)
# [1] 6791  131

############
## valid2 ##
############
dt.valid2 <- dt.centreScale[UNIT == c("12_13_14"), with = T]
dt.valid2[, UNIT := NULL]
# dt.valid[, ACCOUNT_ID := NULL]
dt.valid2[, THIS_PROFIT_LOSS := NULL]
str(dt.valid2)
dim(dt.valid2)
# [1] 7780  131

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
# [1] 12935   130





