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
# [1] 558941    124

##############################
## 1.1 pca ###################
##############################

##############################
## 1.3 train, valid, and test 
##############################
###########
## train ##
###########
dt.train <- dt.centreScale[!UNIT %in% c("36_37_38", "37_38_39", "38_39_40" # valid set 1 related
                                      , "39_40_41", "40_41_42", "41_42_43" # valide set 2 related
                                      , "42_43_44", "43_44_45", "44_45_46") # test set related
                         , with = T]
dt.train[, UNIT := NULL]
# dt.train[, ACCOUNT_ID := NULL]
dt.train[, THIS_PROFIT_LOSS := NULL]
str(dt.train)
dim(dt.train)
# [1] 427339    122

############
## valid1 ##
############
dt.valid1 <- dt.centreScale[UNIT == c("38_39_40"), with = T]
dt.valid1[, UNIT := NULL]
# dt.valid[, ACCOUNT_ID := NULL]
dt.valid1[, THIS_PROFIT_LOSS := NULL]
str(dt.valid1)
dim(dt.valid1)
# [1] 10785   122

############
## valid2 ##
############
dt.valid2 <- dt.centreScale[UNIT == c("41_42_43"), with = T]
dt.valid2[, UNIT := NULL]
# dt.valid[, ACCOUNT_ID := NULL]
dt.valid2[, THIS_PROFIT_LOSS := NULL]
str(dt.valid2)
dim(dt.valid2)
# [1] 15164   122

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
# [1] 22976   121





