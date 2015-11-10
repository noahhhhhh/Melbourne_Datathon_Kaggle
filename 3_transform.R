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
# first thing first, remove EVETN_ID 101149752 as this is cancelled, the data is no use for training and testing purpose
dt <- dt[EVENT_ID != 101149752, with = T]
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
# ACCOUNT_ID          EVENT_ID           BID_TYP         STATUS_ID        INPLAY_BET       PROFIT_LOSS 
# 0.0000000         0.0000000         0.0000000         0.0000000         0.0000000         0.2983494 
# TRANSACTION_COUNT      AVG_BET_SIZE      MAX_BET_SIZE      MIN_BET_SIZE    STDEV_BET_SIZE 
# 0.0000000         0.0000000         0.0000000         0.0000000         0.4062912 
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
dt1.1[ACCOUNT_ID == 1009306 & EVENT_ID == 101093076 & BID_TYP == "B" & STATUS_ID == "S" & INPLAY_BET == "Y", with = T]$STDEV_BET_SIZE
sd(dt[ACCOUNT_ID == 1009306 & EVENT_ID == 101093076 & BID_TYP == "B" & STATUS_ID == "S" & INPLAY_BET == "Y", with = T]$BET_SIZE)

# 2.2 adding new features
####################
## EVENT_SEQ #######
####################
unique(dt[, c("EVENT_ID", "EVENT_DT", "MATCH"), with = F])[order(EVENT_DT)]
# EVENT_ID            EVENT_DT                               MATCH
# 1: 101093194 2015-02-15 01:00:00             South Africa v Zimbabwe 3
# 2: 101093076 2015-02-15 03:30:00                    India v Pakistan 4
# 3: 101093312 2015-02-15 10:00:00               Ireland v West Indies 5
# 4: 101128269 2015-02-16 10:00:00              New Zealand v Scotland 6
# 5: 101128387 2015-02-18 03:30:00            Bangladesh v Afghanistan 7
# 6: 101149398 2015-02-18 10:00:00     Zimbabwe v United Arab Emirates 8
# 7: 101149516 2015-02-20 01:00:00               England v New Zealand 9
# 8: 101149634 2015-02-20 10:00:00              Pakistan v West Indies 10
# 9: 101149752 2015-02-21 03:30:00              Australia v Bangladesh 11
# 10: 101149870 2015-02-21 10:00:00             Sri Lanka v Afghanistan 12
# 11: 101149988 2015-02-22 03:30:00                South Africa v India 13
# 12: 101150106 2015-02-22 10:00:00                  England v Scotland 14
# 13: 101150224 2015-02-24 03:30:00              West Indies v Zimbabwe 15
# 14: 101150348 2015-02-25 03:30:00      Ireland v United Arab Emirates 16
# 15: 101150480 2015-02-25 10:00:00              Afghanistan v Scotland 17
# 16: 101150598 2015-02-26 03:30:00              Sri Lanka v Bangladesh 18
# 17: 101150716 2015-02-27 03:30:00          South Africa v West Indies 19
# 18: 101150834 2015-02-28 01:00:00             Australia v New Zealand 20
# 19: 101150952 2015-02-28 06:30:00        India v United Arab Emirates 21
# 20: 101151090 2015-02-28 10:00:00                 England v Sri Lanka 22
# 21: 101151214 2015-03-01 03:30:00                 Pakistan v Zimbabwe 23
# 22: 101151342 2015-03-03 03:30:00              South Africa v Ireland 24
# 23: 101151486 2015-03-04 01:00:00     Pakistan v United Arab Emirates 25
# 24: 101151606 2015-03-04 06:30:00             Australia v Afghanistan 26
# 25: 101151748 2015-03-04 10:00:00               Bangladesh v Scotland 27
# 26: 101151878 2015-03-06 06:30:00                 India v West Indies 28
# 27: 101152014 2015-03-07 01:00:00             South Africa v Pakistan 29
# 28: 101152132 2015-03-07 03:30:00                  Zimbabwe v Ireland 30
# 29: 101152275 2015-03-07 10:00:00           New Zealand v Afghanistan 31
# 30: 101152395 2015-03-08 03:30:00               Australia v Sri Lanka 32
# 31: 101152576 2015-03-09 03:30:00                England v Bangladesh 33
# 32: 101152715 2015-03-10 01:00:00                     India v Ireland 34
# 33: 101152836 2015-03-11 03:30:00                Sri Lanka v Scotland 35
# 34: 101152954 2015-03-12 01:00:00 South Africa v United Arab Emirates 36
# 35: 101153072 2015-03-13 01:00:00            Bangladesh v New Zealand 37
# 36: 101153190 2015-03-13 03:30:00               England v Afghanistan 38
# 37: 101153308 2015-03-14 01:00:00                    India v Zimbabwe 39
# 38: 101153426 2015-03-14 03:30:00                Australia v Scotland 40
# 39: 101153544 2015-03-14 10:00:00  West Indies v United Arab Emirates 41
# 40: 101153662 2015-03-15 03:30:00                  Pakistan v Ireland 42
# 41: 101183757 2015-03-18 03:30:00            Sri Lanka v South Africa 43
# 42: 101183237 2015-03-19 03:30:00                  India v Bangladesh 44
# 43: 101183885 2015-03-20 03:30:00                Australia v Pakistan 45
# 44: 101184013 2015-03-21 01:00:00           New Zealand v West Indies 46
dt_temp <- unique(dt[, c("EVENT_ID", "EVENT_DT", "MATCH"), with = F])[order(EVENT_DT)]
dt_temp$EVENT_SEQ <- 1:43
dt_temp <- dt_temp[, c("EVENT_ID", "EVENT_SEQ"), with = F]
dt1.1 <- merge(dt1.1, dt_temp, by = "EVENT_ID")

# add it to the dtTestFeatures
test_seq <- data.table(EVENT_ID = sort(unique(dtTestFeatures$EVENT_ID)), EVENT_SEQ = 44:46)
dtTestFeatures <- merge(dtTestFeatures, test_seq, by = "EVENT_ID")

####################
## MED_PRICE_TAKEN #
####################
# MED_PRICE_TAKEN <- dt %>%
#     filter(STATUS_ID == "S") %>%
#     group_by(EVENT_ID) %>%
#     summarise(MED_PRICE_TAKEN = median(PRICE_TAKEN, na.rm = T))
# 
# dt1.1 <- merge(dt1.1, MED_PRICE_TAKEN, by = "EVENT_ID", all.x = T)

####################
## ODDS ############
####################
ODDS_1 <- c(1.28
            , 1.19
            , 1.15
            , 1.43
            , 1.32
            , 1.06
            , 1.01
            , 1.10
            , 1.09
            , 6.54
            , 1.01
            , 1.03
            , 1.13
            , 1.25
            , 1.26
            , 1.01
            , 1.74
            , 1.21
            , 1.46
            , 1.25
            , 1.01
            , 1.07
            , 1.05
            , 1.25
            , 1.89
            , 1.02
            , 1.60
            , 1.32
            , 1.21
            , 1.64
            , 1.20
            , 1.21
            , 1.10
            , 1.45
            , 1.04
            , 1.60
            , 2.59
            , 1.15
            , 1.25
            , 1.03
            , 1.19
            , 1.56
            , 1.04
            )

ODDS_2 <- c(3.68
            , 4.82
            , 5.62
            , 2.81
            , 3.45
            , 10.15
            , 19.87
            , 7.22
            , 7.73
            , 1.12
            , 22.18
            , 13.35
            , 6.53
            , 4.01
            , 3.95
            , 18.70
            , 2.10
            , 4.56
            , 2.78
            , 4.01
            , 21.60
            , 9.22
            , 10.53
            , 4.12
            , 1.92
            , 15.00
            , 2.36
            , 3.46
            , 4.48
            , 2.33
            , 4.59
            , 4.50
            , 7.44
            , 2.78
            , 12.22
            , 2.36
            , 1.51
            , 5.61
            , 4.01
            , 13.97
            , 4.81
            , 2.45
            , 12.58)

ODDS <- data.table(EVENT_SEQ = 43:1, ODDS_1 = ODDS_1, ODDS_2 = ODDS_2)
dt1.1 <- merge(dt1.1, ODDS, by = "EVENT_SEQ")

# add them to the dtTestFeatures
test_odds_1 <- c(2.88, 1.38, 2.10)
test_odds_2 <- c(1.44, 3.08, 1.75)
test_odds <- data.table(EVENT_SEQ = 46:44, ODDS_1 = test_odds_1, ODDS_2 = test_odds_2)
dtTestFeatures <- merge(dtTestFeatures, test_odds, by = "EVENT_SEQ")
####################
## RESULT ##########
####################
RESULT <- c("AS_EXPECTED"
            , "AS_EXPECTED"
            , "AS_EXPECTED"
            , "AS_EXPECTED"
            , "AS_EXPECTED"
            , "AS_EXPECTED"
            , "AS_EXPECTED"
            , "AS_EXPECTED"
            , "AS_EXPECTED"
            , "AS_EXPECTED"
            , "AS_EXPECTED"
            , "AS_EXPECTED"
            , "AS_EXPECTED"
            , "SUPRISED"
            , "AS_EXPECTED"
            , "AS_EXPECTED"
            , "SUPRISED"
            , "SUPRISED"
            , "AS_EXPECTED"
            , "AS_EXPECTED"
            , "AS_EXPECTED"
            , "AS_EXPECTED"
            , "AS_EXPECTED"
            , "AS_EXPECTED"
            , "SUPRISED"
            , "AS_EXPECTED"
            , "SUPRISED"
            , "AS_EXPECTED"
            , "AS_EXPECTED"
            , "AS_EXPECTED"
            , "AS_EXPECTED"
            , "AS_EXPECTED"
            , "AS_EXPECTED"
            , "SUPRISED"
            , "AS_EXPECTED"
            , "SUPRISED"
            , "AS_EXPECTED"
            , "AS_EXPECTED"
            , "AS_EXPECTED"
            , "AS_EXPECTED"
            , "SUPRISED"
            , "AS_EXPECTED"
            , "AS_EXPECTED")

RESULT <- data.table(EVENT_SEQ = 43:1, RESULT = RESULT)
dt1.1 <- merge(dt1.1, RESULT, by = "EVENT_SEQ")

# add it to the dtTestFeatures
test_result <- c("AS_EXPECTED", "AS_EXPECTED", "SUPRISED")
test_result <- data.table(EVENT_SEQ = 46:44, RESULT = test_result)
dtTestFeatures <- merge(dtTestFeatures, test_result, by = "EVENT_SEQ")






