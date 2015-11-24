###########################################################################################
## Feature Engineering (Add) ##############################################################
###########################################################################################
setwd("/Volumes/Data Science/Google Drive/data_science_competition/melbourne_datathon/Melbourne_Datathon_Kaggle/")
rm(list = ls()); gc()
require(data.table)
require(dplyr)
load("../Datathon_Full_Dataset/processedData.RData")
dtTestFeatures <- fread("../data_files/semi_and_final_features.csv")
dtSampleSubmit <- fread("../data_files/sample_submission_bet_size.csv")
str(dt)
str(dtTestFeatures)
#######################################
## 1.0 transform ######################
#######################################
##############################
## 1.1 format train data #####
##############################
# first thing first, remove EVETN_ID 101149752 as this is cancelled, the data is no use for training and testing purpose
dt <- dt[EVENT_ID != 101149752, with = T]
# reproduce the same format as that in the dtTestFeatures (a little bit more aggregated)
dt0.9 <- dt %>%
    group_by(ACCOUNT_ID
             , COUNTRY_OF_RESIDENCE_NAME
             , EVENT_ID
             , MATCH
             , STATUS_ID
             , BID_TYP
             , INPLAY_BET) %>%
    summarise(PROFIT_LOSS = sum(PROFIT_LOSS)
              , TRANSACTION_COUNT = n()
              , AVG_BET_SIZE = mean(BET_SIZE)
              , MAX_BET_SIZE = max(BET_SIZE)
              , MIN_BET_SIZE = min(BET_SIZE)
              , STDEV_BET_SIZE = sd(BET_SIZE)
              )

# try only consider the S transactions
dt0.9 <- dt0.9[STATUS_ID == "S", with = T]
# ignore the BID_TYP
dt0.9[, BID_TYP := NULL]
dim(dt0.9)
# [1] 290978     12
dim(dtTestFeatures)
# [1] 53070     9
setdiff(names(dt0.9), names(dtTestFeatures))
# [1] "ACCOUNT_ID"                "COUNTRY_OF_RESIDENCE_NAME" "MATCH"                    
# [4] "PROFIT_LOSS" 
# get the duplicated rows (duplicated because forgetting to remove BID_TYP from Group in SQL)
duplicatedCozBID_TYP <- duplicated(dt0.9[, c("ACCOUNT_ID", "COUNTRY_OF_RESIDENCE_NAME"
                                             , "EVENT_ID", "MATCH"
                                             , "STATUS_ID", "INPLAY_BET"), with = F])
# used to esitmate the NO_OF_BID_TYPE
dt0.9$CNT_BID_TYPE <- duplicatedCozBID_TYP
dim(dt0.9)
# [1] 290978     13

##############################
## 1.2 format test data ######
##############################
# modify dtTestFeatures name
setnames(dtTestFeatures, names(dtTestFeatures), c("ACCOUNT_ID", names(dtTestFeatures)[-1]))
# try only consider the S transactions
dtTestFeatures <- dtTestFeatures[STATUS_ID == "S", with = T]
# get the duplicated rows (duplicated because forgetting to remove BID_TYP from Group in SQL)
duplicatedCozBID_TYP <- duplicated(dtTestFeatures[, c("ACCOUNT_ID", "EVENT_ID", "INPLAY_BET"), with = F])
# used to esitmate the NO_OF_BID_TYPE
dtTestFeatures$CNT_BID_TYPE <- duplicatedCozBID_TYP
# used to populate the COUNTRY_OF_RESIDENCE_NAME
dtTempAccountCountry <- unique(dt[, c("ACCOUNT_ID", "COUNTRY_OF_RESIDENCE_NAME"), with = F])
dtTempAccountCountry$ACCOUNT_ID <- as.integer(dtTempAccountCountry$ACCOUNT_ID)
dtTestFeatures <- merge(dtTestFeatures, dtTempAccountCountry, by = "ACCOUNT_ID", all.x = T)
# used to populate the MATCH
testMatch <- data.table(EVENT_ID = sort(unique(dtTestFeatures$EVENT_ID))
                        , MATCH = c("New Zealand v South Africa", "Australia v India", "New Zealand v Australia"))
dtTestFeatures <- merge(dtTestFeatures, testMatch, by = "EVENT_ID", all.x = T)
# used to mockup the PROFIT_LOSS
dtTestFeatures$PROFIT_LOSS <- 0
dim(dtTestFeatures)
# [1] 38020    13

##############################
## 1.3 combine train and test so they can process together ######
##############################
dt0.9 <- dt0.9[, c("ACCOUNT_ID"
                   , "COUNTRY_OF_RESIDENCE_NAME"
                   , "EVENT_ID"
                   , "MATCH"
                   , "STATUS_ID"
                   , "INPLAY_BET"
                   , "TRANSACTION_COUNT"
                   , "AVG_BET_SIZE"
                   , "MAX_BET_SIZE"
                   , "MIN_BET_SIZE"
                   , "STDEV_BET_SIZE"
                   , "CNT_BID_TYPE"
                   , "PROFIT_LOSS")
               , with = F]
dtTestFeatures <- dtTestFeatures[, c("ACCOUNT_ID"
                                     , "COUNTRY_OF_RESIDENCE_NAME"
                                     , "EVENT_ID"
                                     , "MATCH"
                                     , "STATUS_ID"
                                     , "INPLAY_BET"
                                     , "TRANSACTION_COUNT"
                                     , "AVG_BET_SIZE"
                                     , "MAX_BET_SIZE"
                                     , "MIN_BET_SIZE"
                                     , "STDEV_BET_SIZE"
                                     , "CNT_BID_TYPE"
                                     , "PROFIT_LOSS")
                                 , with = F]
# ensure column sequence is the same
identical(names(dt0.9), names(dtTestFeatures))
# ensure class of columns is the same
# before
str(dt0.9)
str(dtTestFeatures)
dt0.9$ACCOUNT_ID <- as.integer(dt0.9$ACCOUNT_ID)
dt0.9$EVENT_ID <- as.integer(dt0.9$EVENT_ID)
dt0.9$STATUS_ID <- as.character(dt0.9$STATUS_ID)
dt0.9$INPLAY_BET <- as.character(dt0.9$INPLAY_BET)
# after
str(dt0.9)
str(dtTestFeatures)
# combine
dt1.0 <- rbind(dt0.9, dtTestFeatures)
dim(dt0.9); dim(dtTestFeatures); dim(dt1.0)
# [1] 290978     13
# [1] 38020    13
# [1] 328998     13

# # further aggregate
# dt1.0 <- dt0.9 %>%
#     group_by(ACCOUNT_ID
#              , COUNTRY_OF_RESIDENCE_NAME
#              , EVENT_ID
#              , MATCH
#              # , BID_TYP # B, L
#              # , INPLAY_BET # Y, N
#              ) %>%
#     summarise(PROFIT_LOSS = sum(PROFIT_LOSS)
#               , TRANSACTION_COUNT = sum(TRANSACTION_COUNT)
#               , AVG_BET_SIZE = mean(BET_SIZE)
#               , MAX_BET_SIZE = max(BET_SIZE)
#               , MIN_BET_SIZE = min(BET_SIZE)
#               , STDEV_BET_SIZE = sd(BET_SIZE)
#               
#               , TRANSACTION_COUNT_INPLAY_BET_Y = n_distinct(BET_TRANS_ID[INPLAY_BET == "Y"])
#               , TRANSACTION_COUNT_INPLAY_BET_N = n_distinct(BET_TRANS_ID[INPLAY_BET == "N"])
#               , AVG_BET_SIZE_INPLAY_BET_Y = mean(BET_SIZE[INPLAY_BET == "Y"])
#               , AVG_BET_SIZE_INPLAY_BET_N = mean(BET_SIZE[INPLAY_BET == "N"])
#               , MAX_BET_SIZE_INPLAY_BET_Y = max(BET_SIZE[INPLAY_BET == "Y"])
#               , MAX_BET_SIZE_INPLAY_BET_N = max(BET_SIZE[INPLAY_BET == "N"])
#               , MIN_BET_SIZE_INPLAY_BET_Y = min(BET_SIZE[INPLAY_BET == "Y"])
#               , MIN_BET_SIZE_INPLAY_BET_N = min(BET_SIZE[INPLAY_BET == "N"])
#               , STDEV_BET_SIZE_INPLAY_BET_Y = sd(BET_SIZE[INPLAY_BET == "Y"])
#               , STDEV_BET_SIZE_INPLAY_BET_N = sd(BET_SIZE[INPLAY_BET == "N"])
#               
#               , NO_OF_BID_TYPE = n_distinct(BID_TYP)
#               , NO_OF_INPLAY_BET = n_distinct(INPLAY_BET))
# dim(dt1.0)
# # [1] 174226     22

# aggregate
dt1.1 <- dt1.0 %>%
    group_by(ACCOUNT_ID
             , COUNTRY_OF_RESIDENCE_NAME
             , EVENT_ID
             , MATCH
             # , BID_TYP # B, L
             # , INPLAY_BET # Y, N
    ) %>%
    summarise(PROFIT_LOSS = sum(PROFIT_LOSS)
              , TRANSACTION_COUNT = sum(TRANSACTION_COUNT)
              , AVG_BET_SIZE = sum(AVG_BET_SIZE * TRANSACTION_COUNT) / sum(TRANSACTION_COUNT)
              , MAX_BET_SIZE = max(MAX_BET_SIZE)
              , MIN_BET_SIZE = min(MIN_BET_SIZE)
              , STDEV_BET_SIZE = mean(STDEV_BET_SIZE) / sqrt(sum(TRANSACTION_COUNT)) # this is just an estimate
              
              , TRANSACTION_COUNT_INPLAY_BET_Y = sum(TRANSACTION_COUNT[INPLAY_BET == "Y"])
              , TRANSACTION_COUNT_INPLAY_BET_N = sum(TRANSACTION_COUNT[INPLAY_BET == "N"])
              , AVG_BET_SIZE_INPLAY_BET_Y = sum(AVG_BET_SIZE[INPLAY_BET == "Y"] * TRANSACTION_COUNT[INPLAY_BET == "Y"]) / sum(TRANSACTION_COUNT[INPLAY_BET == "Y"])
              , AVG_BET_SIZE_INPLAY_BET_N = sum(AVG_BET_SIZE[INPLAY_BET == "N"] * TRANSACTION_COUNT[INPLAY_BET == "N"]) / sum(TRANSACTION_COUNT[INPLAY_BET == "N"])
              , MAX_BET_SIZE_INPLAY_BET_Y = max(MAX_BET_SIZE[INPLAY_BET == "Y"])
              , MAX_BET_SIZE_INPLAY_BET_N = max(MAX_BET_SIZE[INPLAY_BET == "N"])
              , MIN_BET_SIZE_INPLAY_BET_Y = min(MIN_BET_SIZE[INPLAY_BET == "Y"])
              , MIN_BET_SIZE_INPLAY_BET_N = min(MIN_BET_SIZE[INPLAY_BET == "N"])
              , STDEV_BET_SIZE_INPLAY_BET_Y = mean(STDEV_BET_SIZE[INPLAY_BET == "Y"]) / sqrt(sum(TRANSACTION_COUNT[INPLAY_BET == "Y"]))
              , STDEV_BET_SIZE_INPLAY_BET_N = mean(STDEV_BET_SIZE[INPLAY_BET == "Y"]) / sqrt(sum(TRANSACTION_COUNT[INPLAY_BET == "Y"]))
              
              , NO_OF_BID_TYPE = sum(CNT_BID_TYPE) # this is just an estimate as we dont have B and L visible
              , NO_OF_INPLAY_BET = n_distinct(INPLAY_BET))

# modify NO_OF_BID_TYPE in line with the train
dt1.1[, NO_OF_BID_TYPE := ifelse(NO_OF_BID_TYPE > 0, 2, 1)]
dim(dt1.1)
# [1] 328998     22
##############################
## 1.4 feature cleansing #####
##############################
####################
## INPLAY_BET ######
####################
dt1.1$INPLAY_BET <- ifelse(dt1.1$MAX_BET_SIZE_INPLAY_BET_Y == -Inf, "N"
                         , ifelse(dt1.1$MAX_BET_SIZE_INPLAY_BET_N == -Inf, "Y", "YN"))
####################
## NAs #############
####################
# before
apply(dt1.1, 2, function(x) sum(is.na(x)))
# ACCOUNT_ID      COUNTRY_OF_RESIDENCE_NAME                       EVENT_ID 
# 0.00000000                     0.01507304                     0.00000000 
# MATCH                    PROFIT_LOSS              TRANSACTION_COUNT 
# 0.00000000                     0.00000000                     0.00000000 
# AVG_BET_SIZE                   MAX_BET_SIZE                   MIN_BET_SIZE 
# 0.00000000                     0.00000000                     0.00000000 
# STDEV_BET_SIZE TRANSACTION_COUNT_INPLAY_BET_Y TRANSACTION_COUNT_INPLAY_BET_N 
# 0.33665250                     0.00000000                     0.00000000 
# AVG_BET_SIZE_INPLAY_BET_Y      AVG_BET_SIZE_INPLAY_BET_N      MAX_BET_SIZE_INPLAY_BET_Y 
# 0.12586399                     0.56085143                     0.00000000 
# MAX_BET_SIZE_INPLAY_BET_N      MIN_BET_SIZE_INPLAY_BET_Y      MIN_BET_SIZE_INPLAY_BET_N 
# 0.00000000                     0.00000000                     0.00000000 
# STDEV_BET_SIZE_INPLAY_BET_Y    STDEV_BET_SIZE_INPLAY_BET_N                 NO_OF_BID_TYPE 
# 0.35023313                     0.35023313                     0.00000000 
# NO_OF_INPLAY_BET                     INPLAY_BET 
# 0.00000000                     0.00000000
dt1.1$COUNTRY_OF_RESIDENCE_NAME[is.na(dt1.1$COUNTRY_OF_RESIDENCE_NAME)] <- "Unknown"
dt1.1$STDEV_BET_SIZE[is.na(dt1.1$STDEV_BET_SIZE)] <- 0
dt1.1$AVG_BET_SIZE_INPLAY_BET_Y[is.na(dt1.1$AVG_BET_SIZE_INPLAY_BET_Y)] <- 0
dt1.1$AVG_BET_SIZE_INPLAY_BET_N[is.na(dt1.1$AVG_BET_SIZE_INPLAY_BET_N)] <- 0
dt1.1$STDEV_BET_SIZE_INPLAY_BET_Y[is.na(dt1.1$STDEV_BET_SIZE_INPLAY_BET_Y)] <- 0
dt1.1$STDEV_BET_SIZE_INPLAY_BET_N[is.na(dt1.1$STDEV_BET_SIZE_INPLAY_BET_N)] <- 0
# after
apply(dt1.1, 2, function(x) sum(is.na(x)))
# ACCOUNT_ID      COUNTRY_OF_RESIDENCE_NAME                       EVENT_ID 
# 0                              0                              0 
# MATCH                    PROFIT_LOSS              TRANSACTION_COUNT 
# 0                              0                              0 
# AVG_BET_SIZE                   MAX_BET_SIZE                   MIN_BET_SIZE 
# 0                              0                              0 
# STDEV_BET_SIZE TRANSACTION_COUNT_INPLAY_BET_Y TRANSACTION_COUNT_INPLAY_BET_N 
# 0                              0                              0 
# AVG_BET_SIZE_INPLAY_BET_Y      AVG_BET_SIZE_INPLAY_BET_N      MAX_BET_SIZE_INPLAY_BET_Y 
# 0                              0                              0 
# MAX_BET_SIZE_INPLAY_BET_N      MIN_BET_SIZE_INPLAY_BET_Y      MIN_BET_SIZE_INPLAY_BET_N 
# 0                              0                              0 
# STDEV_BET_SIZE_INPLAY_BET_Y    STDEV_BET_SIZE_INPLAY_BET_N                 NO_OF_BID_TYPE 
# 0                              0                              0 
# NO_OF_INPLAY_BET                     INPLAY_BET 
# 0                              0 

####################
## Inf, -Inf #######
####################
dt1.1$AVG_BET_SIZE_INPLAY_BET_Y[dt1.1$AVG_BET_SIZE_INPLAY_BET_Y %in% c(Inf, -Inf)] <- 0
dt1.1$AVG_BET_SIZE_INPLAY_BET_N[dt1.1$AVG_BET_SIZE_INPLAY_BET_N %in% c(Inf, -Inf)] <- 0
dt1.1$MAX_BET_SIZE_INPLAY_BET_Y[dt1.1$MAX_BET_SIZE_INPLAY_BET_Y %in% c(Inf, -Inf)] <- 0
dt1.1$MAX_BET_SIZE_INPLAY_BET_N[dt1.1$MAX_BET_SIZE_INPLAY_BET_N %in% c(Inf, -Inf)] <- 0
dt1.1$MIN_BET_SIZE_INPLAY_BET_Y[dt1.1$MIN_BET_SIZE_INPLAY_BET_Y %in% c(Inf, -Inf)] <- 0
dt1.1$MIN_BET_SIZE_INPLAY_BET_N[dt1.1$MIN_BET_SIZE_INPLAY_BET_N %in% c(Inf, -Inf)] <- 0
dt1.1$STDEV_BET_SIZE_INPLAY_BET_Y[dt1.1$STDEV_BET_SIZE_INPLAY_BET_Y %in% c(Inf, -Inf)] <- 0
dt1.1$STDEV_BET_SIZE_INPLAY_BET_N[dt1.1$STDEV_BET_SIZE_INPLAY_BET_N %in% c(Inf, -Inf)] <- 0

##############################
## 1.5 feature engineering (add)
##############################
####################
## ME2ME ###########
####################
dt1.1[, ME2ME := ifelse(NO_OF_BID_TYPE == 2, 1, 0)]

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
dtTempSeqTrain <- unique(dt[, c("EVENT_ID", "EVENT_DT", "MATCH"), with = F])[order(EVENT_DT)]
dtTempSeqTrain$EVENT_SEQ <- 1:43
dtTempSeqTrain <- dtTempSeqTrain[, c("EVENT_ID", "EVENT_SEQ"), with = F]
dtTempSeqTest <- data.table(EVENT_ID = sort(unique(dtTestFeatures$EVENT_ID)), EVENT_SEQ = 44:46)
dtTempSeq <- rbind(dtTempSeqTrain, dtTempSeqTest)
dtTempSeq$EVENT_ID <- as.integer(dtTempSeq$EVENT_ID)
dt1.1 <- merge(dt1.1, dtTempSeq, by = "EVENT_ID")

####################
## ODDS ############
####################
ODDS_1 <- c(2.88
            , 1.38
            , 2.10
            , 1.28
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

ODDS_2 <- c(1.44
            , 3.08
            , 1.75
            , 3.68
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

ODDS <- data.table(EVENT_SEQ = 46:1, ODDS_1 = ODDS_1, ODDS_2 = ODDS_2)
dt1.1 <- merge(dt1.1, ODDS, by = "EVENT_SEQ")

####################
## INPLAY_Y AND INPLAY_N
####################
dt1.1[, IND_INPLAY_Y := ifelse(INPLAY_BET == "Y", 1, 0)]
dt1.1[, IND_INPLAY_N := ifelse(INPLAY_BET == "N", 1, 0)]
dt1.1[, CUM_INPLAY_Y := cumsum(IND_INPLAY_Y), by = ACCOUNT_ID]
dt1.1[, CUM_INPLAY_N := cumsum(IND_INPLAY_N), by = ACCOUNT_ID]

dt1.1[, TIMES_INPLAY_Y := shift(CUM_INPLAY_Y, fill = 0, type = "lag"), by = ACCOUNT_ID]
dt1.1[, TIMES_INPLAY_N := shift(CUM_INPLAY_N, fill = 0, type = "lag"), by = ACCOUNT_ID]

####################
## RESULT ##########
####################
RESULT <- c("AS_EXPECTED"
            , "AS_EXPECTED"
            , "SUPRISED"
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

RESULT <- data.table(EVENT_SEQ = 46:1, RESULT = RESULT)
dt1.1 <- merge(dt1.1, RESULT, by = "EVENT_SEQ")

dt1.1 <- dt1.1[order(EVENT_SEQ)]
dt1.1 <- dt1.1[, IND_RESULT_EXPECTED := ifelse(RESULT == "AS_EXPECTED", 1, 0)]
dt1.1 <- dt1.1[, IND_RESULT_SUPRISED := ifelse(RESULT == "SUPRISED", 1, 0)]
dt1.1[, CUM_RESULT_EXPECTED := cumsum(IND_RESULT_EXPECTED), by = ACCOUNT_ID]
dt1.1[, CUM_RESULT_SUPRISED := cumsum(IND_RESULT_SUPRISED), by = ACCOUNT_ID]

dt1.1[, TIMES_ATTENDING_EXPECTED_EVENT := shift(CUM_RESULT_EXPECTED, fill = 0, type = "lag"), by = ACCOUNT_ID]
dt1.1[, TIMES_ATTENDING_SUPRISED_EVENT := shift(CUM_RESULT_SUPRISED, fill = 0, type = "lag"), by = ACCOUNT_ID]

####################
## SCORE_DIFF ######
####################
SCORE_DIFF <- c(abs(138-136),
                abs(328-233),
                -abs(299-281),
                abs(393-250),
                abs(216-213),
                abs(302-193),
                abs(134-133),
                abs(241-237),
                abs(176-175),
                abs(133-130),
                abs(288-287),
                abs(101-111),
                abs(288-290),
                abs(341-195),
                abs(363-215),
                abs(260-259),
                -abs(260-275),
                abs(376-312),
                abs(188-186),
                -abs(326-331),
                -abs(202-222),
                abs(185-182),
                abs(322-318),
                abs(417-142),
                abs(339-210),
                abs(411-210),
                abs(235-215),
                -abs(309-312),
                abs(104-102),
                -abs(151-152),
                abs(408-151),
                abs(332-240),
                abs(211-210),
                abs(279-278),
                abs(372-289),
                abs(303-184),
                -abs(177-307),
                abs(236-232),
                -abs(160-310),
                abs(123-125),
                abs(286-285),
                abs(267-162),
                abs(146-142),
                -abs(304-307),
                abs(300-224),
                abs(339-277))

SCORE_DIFF <- data.table(EVENT_SEQ = 46:1, SCORE_DIFF = SCORE_DIFF)
dt1.1 <- merge(dt1.1, SCORE_DIFF, by = "EVENT_SEQ")

####################
## IND_WIN #########
####################
dt1.1[, IND_WIN := ifelse(PROFIT_LOSS > 0, 1, 0)]

####################
## PRE_IND_LAST_WIN 
####################
dt1.1 <- dt1.1[order(EVENT_SEQ)]
dt1.1[, PRE_IND_LAST_WIN := shift(IND_WIN, fill = 0, type = "lag"), by = ACCOUNT_ID]

####################
## IND_LOSE ########
####################
dt1.1[, IND_LOSE := ifelse(PROFIT_LOSS <= 0, -1, 0)]

####################
## PRE_IND_LAST_LOSE
####################
dt1.1 <- dt1.1[order(EVENT_SEQ)]
dt1.1[, PRE_IND_LAST_LOSE := shift(IND_LOSE, fill = 0, type = "lag"), by = ACCOUNT_ID]

####################
## NO_OF_EVENT_ATTENDED
####################
dt1.1[, ATTENDED := 1]
dt1.1 <- dt1.1[order(EVENT_SEQ)]
dt1.1[, CUM_ATTENDED := cumsum(ATTENDED), by = ACCOUNT_ID]
dt1.1[, NO_OF_EVENT_ATTENDED := shift(CUM_ATTENDED, fill = 0, type = "lag"), by = ACCOUNT_ID]

####################
## NO_OF_EVENT_ABSENT
####################
dt1.1[, NO_OF_EVENT_ABSENT := EVENT_SEQ - NO_OF_EVENT_ATTENDED - 1]

####################
## ATTENDED_ABSENT #
####################
dt1.1[, ATTENDED_ABSENT := NO_OF_EVENT_ATTENDED - NO_OF_EVENT_ABSENT]

####################
## NO_OF_WIN #######
####################
dt1.1 <- dt1.1[order(EVENT_SEQ)]
dt1.1[, CUM_WIN := cumsum(IND_WIN), by = ACCOUNT_ID]
dt1.1[, NO_OF_WIN := shift(CUM_WIN, fill = 0, type = "lag"), by = ACCOUNT_ID]

####################
## NO_OF_LOSE ######
####################
dt1.1 <- dt1.1[order(EVENT_SEQ)]
dt1.1[, CUM_LOSE := cumsum(IND_LOSE), by = ACCOUNT_ID]
dt1.1[, NO_OF_LOSE := shift(CUM_LOSE, fill = 0, type = "lag"), by = ACCOUNT_ID]

####################
## RATE_WIN ########
####################
dt1.1[, RATE_WIN := NO_OF_WIN / NO_OF_EVENT_ATTENDED]
dt1.1$RATE_WIN[is.nan(dt1.1$RATE_WIN)] <- .5

####################
## WIN_LOSE ########
####################
dt1.1[, WIN_LOSE := CUM_WIN - CUM_LOSE]

####################
## TTL_PROFIT_LOSS #
####################
dt1.1 <- dt1.1[order(EVENT_SEQ)]
dt1.1[, CUM_PROFIT_LOSS := cumsum(PROFIT_LOSS), by = ACCOUNT_ID]
dt1.1[, TTL_PROFIT_LOSS := shift(CUM_PROFIT_LOSS, fill = 0, type = "lag"), by = ACCOUNT_ID]

####################
## AVG_PROFIT_LOSS #
####################
dt1.1[, CUM_AVG_PROFIT_LOSS := ifelse(CUM_ATTENDED == 0, 0, CUM_PROFIT_LOSS / CUM_ATTENDED), by = ACCOUNT_ID]
dt1.1[, AVG_PROFIT_LOSS := shift(CUM_AVG_PROFIT_LOSS, fill = 0, type = "lag"), by = ACCOUNT_ID]

####################
## MAX_PROFIT_LOSS #
####################
dt1.1[, CUM_MAX_PROFIT_LOSS := cummax(PROFIT_LOSS), by = ACCOUNT_ID]
dt1.1[, MAX_PROFIT_LOSS := shift(CUM_MAX_PROFIT_LOSS, fill = 0, type = "lag"), by = ACCOUNT_ID]

####################
## MIN_PROFIT_LOSS #
####################
dt1.1[, CUM_MIN_PROFIT_LOSS := cummin(PROFIT_LOSS), by = ACCOUNT_ID]
dt1.1[, MIN_PROFIT_LOSS := shift(CUM_MIN_PROFIT_LOSS, fill = 0, type = "lag"), by = ACCOUNT_ID]

####################
## TIMES_BEING_A_ME2ME
####################
dt1.1 <- dt1.1[order(EVENT_SEQ)]
dt1.1[, CUM_ME2ME := cumsum(ME2ME), by = ACCOUNT_ID]
dt1.1[, TIMES_BEING_A_ME2ME := shift(CUM_ME2ME, fill = 0, type = "lag"), by = ACCOUNT_ID]

########################
## TIMES_IN_AND_OUT_PLAY
########################
dt1.1 <- dt1.1[order(EVENT_SEQ)]
dt1.1[, IND_IN_AND_OUT_PAY := ifelse(NO_OF_INPLAY_BET == 2, 1, 0)]
dt1.1[, CUM_IN_AND_OUT_PLAY := cumsum(IND_IN_AND_OUT_PAY), by = ACCOUNT_ID]
dt1.1[, TIMES_IN_AND_OUT_PLAY := shift(CUM_IN_AND_OUT_PLAY, fill = 0, type = "lag"), by = ACCOUNT_ID]

####################
## IS_FROM_WIN, IS_FROM_LOSE, IS_FROM_NEITHER
####################
country <- sort(unique(dt1.1$COUNTRY_OF_RESIDENCE_NAME)) # COUNTRY
team <- sort(unique(dt$SELECTION_NAME)) # TEAM
match <- sort(unique(dt1.1$MATCH)) # MATCH

setdiff(team, country) # below teams are not in country
# [1] "Afghanistan"          "England"              "Scotland"             "South Africa"        
# [5] "United Arab Emirates" "West Indies"          "Zimbabwe"  

# England and Scotland <- United Kingdom
dt1.1$MATCH <- gsub("England", "United Kingdom", dt1.1$MATCH)
dt1.1$MATCH <- gsub("Scotland", "United Kingdom", dt1.1$MATCH)
dt$SELECTION_NAME <- gsub("England", "United Kingdom", dt$SELECTION_NAME)
dt$SELECTION_NAME <- gsub("Scotland", "United Kingdom", dt$SELECTION_NAME)


# UAE <- United Arab Emirates
dt1.1$COUNTRY_OF_RESIDENCE_NAME <- gsub("UAE", "United Arab Emirates", dt1.1$COUNTRY_OF_RESIDENCE_NAME)

country <- sort(unique(dt1.1$COUNTRY_OF_RESIDENCE_NAME)) # COUNTRY
team <- sort(unique(dt$SELECTION_NAME)) # TEAM
setdiff(team, country) # below teams are not in country
# [1] "Afghanistan"  "South Africa" "West Indies"  "Zimbabwe"

# WIN_TEAM
WIN_TEAM <- c("Australia"
              , "Australia"
              , "New Zealand"
              , "New Zealand"
              , "Australia"
              , "India"
              , "South Africa"
              , "Pakistan"
              , "West Indies"
              , "Australia"
              , "India"
              , "United Kingdom"
              , "New Zealand"
              , "South Africa"
              , "Sri Lanka"
              , "India"
              , "Bangladesh"
              , "Australia"
              , "New Zealand"
              , "Ireland"
              , "Pakistan"
              , "India"
              , "Bangladesh"
              , "Australia"
              , "Pakistan"
              , "South Africa"
              , "Pakistan"
              , "Sri Lanka"
              , "India"
              , "New Zealand"
              , "South Africa"
              , "Sri Lanka"
              , "Afghanistan"
              , "Ireland"
              , "West Indies"
              , "United Kingdom"
              , "India"
              , "Sri Lanka"
              , "West Indies"
              , "New Zealand"
              , "Zimbabwe"
              , "Bangladesh"
              , "New Zealand"
              , "Ireland"
              , "India"
              , "South Africa")
WIN_TEAM <- data.table(EVENT_SEQ = 46:1, WIN_TEAM = WIN_TEAM)
dt1.1 <- merge(dt1.1, WIN_TEAM, by = "EVENT_SEQ")

# LOSE_TEAM
LOSE_TEAM <- c("New Zealand"
              , "India"
              , "South Africa"
              , "West Indies"
              , "Pakistan"
              , "Bangladesh"
              , "Sri Lanka"
              , "Ireland"
              , "United Arab Emirates"
              , "United Kingdom"
              , "Zimbabwe"
              , "Afghanistan"
              , "Bangladesh"
              , "United Arab Emirates"
              , "United Kingdom"
              , "Ireland"
              , "United Kingdom"
              , "Sri Lanka"
              , "Afghanistan"
              , "Zimbabwe"
              , "South Africa"
              , "West Indies"
              , "United Kingdom"
              , "Afghanistan"
              , "United Arab Emirates"
              , "Ireland"
              , "Zimbabwe"
              , "United Kingdom"
              , "United Arab Emirates"
              , "Australia"
              , "West Indies"
              , "Bangladesh"
              , "United Kingdom"
              , "United Arab Emirates"
              , "Zimbabwe"
              , "United Kingdom"
              , "South Africa"
              , "Afghanistan"
              , "Pakistan"
              , "United Kingdom"
              , "United Arab Emirates"
              , "Afghanistan"
              , "United Kingdom"
              , "West Indies"
              , "Pakistan"
              , "Zimbabwe")

# LOSE_TEAM <- as.character()
# for (i in 1:dim(dt1.1)[1]){
#     LOSE_TEAM[i] <- gsub(dt1.1[i]$WIN_TEAM, "", dt1.1[i]$MATCH)
# }
# 
# LOSE_TEAM <- gsub(" v ", "", LOSE_TEAM)

dt1.1$LOSE_TEAM <- LOSE_TEAM
# this game is England v Scotland
dt1.1$LOSE_TEAM[dt1.1$EVENT_SEQ == 11] <- "United Kingdom"

dt1.1[, IS_FROM_WIN := ifelse(COUNTRY_OF_RESIDENCE_NAME == WIN_TEAM, 1, 0)]
dt1.1[, IS_FROM_LOSE := ifelse(COUNTRY_OF_RESIDENCE_NAME == LOSE_TEAM, 1, 0)]
dt1.1[, IS_FROM_NEITHER := ifelse(COUNTRY_OF_RESIDENCE_NAME != WIN_TEAM & COUNTRY_OF_RESIDENCE_NAME != LOSE_TEAM , 1, 0)]

##############################
## save it ###################
##############################
save(dt1.1, file = "../Datathon_Full_Dataset/engineeredData.RData")




















