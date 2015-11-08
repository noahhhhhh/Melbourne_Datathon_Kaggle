###########################################################################################
## Transform, Training and Testing sets ###################################################
###########################################################################################
setwd("/Volumes/Data Science/Google Drive/data_science_competition/melbourne_datathon/Melbourne_Datathon_Kaggle/")
rm(list = ls()); gc()
load("../Datathon_Full_Dataset/processedData.RData")
dtTestFeatures <- fread("../data_files/semi_and_final_features.csv")
dtSampleSubmit <- fread("../data_files/sample_submission_bet_size.csv")
require(data.table)
require(dplyr)
str(dt)
str(dtTestFeatures)
#######################################
## 1.0 transform ######################
#######################################
# 1.1 reproduce the same format as that in the dtTestFeatures
dt1.1 <- dt %>%
    group_by(ACCOUNT_ID
             , EVENT_ID
             , BID_TYP
             , STATUS_ID
             , INPLAY_BET) %>%
    summarise(PROFIT_LOSS = sum(PROFIT_LOSS)
              , TRANSACTION_COUNT = n()
              , AVG_BET_SIZE = mean(BET_SIZE)
              , MAX_BET_SIZE = max(BET_SIZE)
              , MIN_BET_SIZE = min(BET_SIZE)
              , STDEV_BET_SIZE = sd(BET_SIZE))

# NAs
apply(dt1.1, 2, function(x) mean(is.na(x)))
dt1.1$STDEV_BET_SIZE[is.na(dt1.1$STDEV_BET_SIZE)] <- 0
dt1.1$PROFIT_LOSS[is.na(dt1.1$PROFIT_LOSS)] <- 0

dt1.1
# Source: local data table [416,113 x 11]
# Groups: ACCOUNT_ID, EVENT_ID, BID_TYP, STATUS_ID
# 
# ACCOUNT_ID  EVENT_ID BID_TYP STATUS_ID INPLAY_BET   PROFIT_LOSS TRANSACTION_COUNT AVG_BET_SIZE MAX_BET_SIZE
# 1     1009306 101093076       L         S          Y -145485.00491               219   1366.93133  32190.82533
# 2     1005119 101093076       L         S          N    -130.91197                 1    214.60979    214.60979
# 3     1018472 101093076       L         S          N     -95.43292                 2     78.22371    104.94209
# 4     1018768 101093076       L         S          N     -47.12737                 1     77.25798     77.25798
# 5     1022046 101093076       L         S          N   -7501.69753                27    438.85204   4599.75991
# 6     1021576 101093076       L         S          N    -490.91009                 1    804.77063    804.77063
# 7     1017887 101093076       L         S          N    -157.09123                 1    257.52660    257.52660
# 8     1017887 101093076       L         S          Y   -1145.80281                12    354.09908   1287.63301
# 9     1018690 101093076       L         S          N     -31.41825                 1     51.50532     51.50532
# 10    1005922 101093076       L         S          Y   -1776.93356                 7    735.79029   2575.26603
# ..        ...       ...     ...       ...        ...           ...               ...          ...          ...
# Variables not shown: MIN_BET_SIZE (dbl), STDEV_BET_SIZE (dbl)
dim(dt1.1)
# [1] 416113     10

# validate if this dt1.1 is correct, take an ACCOUNT_ID, EVENT_ID and calculate PL, BET_SIZE
# develop new features
