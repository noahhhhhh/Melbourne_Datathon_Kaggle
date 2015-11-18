###########################################################################################
## Transform, Training and Testing sets ###################################################
###########################################################################################
setwd("/Volumes/Data Science/Google Drive/data_science_competition/melbourne_datathon/Melbourne_Datathon_Kaggle/")
rm(list = ls()); gc()
require(data.table)
require(dplyr)
require(stringr)
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
# try only consider the S transactions
dt <- dt[STATUS_ID == "S", with = T]
# reproduce the same format as that in the dtTestFeatures (a little bit more aggregated)
dt1.0 <- dt %>%
    group_by(ACCOUNT_ID
             , COUNTRY_OF_RESIDENCE_NAME
             , EVENT_ID
             , MATCH
             # , BID_TYP # B, L
             # , INPLAY_BET # Y, N
             ) %>%
    summarise(PROFIT_LOSS = sum(PROFIT_LOSS)
              , TRANSACTION_COUNT = n()
              , AVG_BET_SIZE = mean(BET_SIZE)
              , MAX_BET_SIZE = max(BET_SIZE)
              , MIN_BET_SIZE = min(BET_SIZE)
              , STDEV_BET_SIZE = sd(BET_SIZE)
              
              , TRANSACTION_COUNT_INPLAY_BET_Y = n_distinct(BET_TRANS_ID[INPLAY_BET == "Y"])
              , TRANSACTION_COUNT_INPLAY_BET_N = n_distinct(BET_TRANS_ID[INPLAY_BET == "N"])
              , AVG_BET_SIZE_INPLAY_BET_Y = mean(BET_SIZE[INPLAY_BET == "Y"])
              , AVG_BET_SIZE_INPLAY_BET_N = mean(BET_SIZE[INPLAY_BET == "N"])
              , MAX_BET_SIZE_INPLAY_BET_Y = max(BET_SIZE[INPLAY_BET == "Y"])
              , MAX_BET_SIZE_INPLAY_BET_N = max(BET_SIZE[INPLAY_BET == "N"])
              , MIN_BET_SIZE_INPLAY_BET_Y = min(BET_SIZE[INPLAY_BET == "Y"])
              , MIN_BET_SIZE_INPLAY_BET_N = min(BET_SIZE[INPLAY_BET == "N"])
              , STDEV_BET_SIZE_INPLAY_BET_Y = sd(BET_SIZE[INPLAY_BET == "Y"])
              , STDEV_BET_SIZE_INPLAY_BET_N = sd(BET_SIZE[INPLAY_BET == "N"])
              
              , NO_OF_BID_TYPE = n_distinct(BID_TYP)
              , NO_OF_INPLAY_BET = n_distinct(INPLAY_BET))
dim(dt1.0)
# [1] 174226     22

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

# aggregate into a dtTest set
dtTest <- dtTestFeatures %>%
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
              , STDEV_BET_SIZE = sqrt(sum((STDEV_BET_SIZE ^ 2) * TRANSACTION_COUNT) / sum(TRANSACTION_COUNT))
              
              , TRANSACTION_COUNT_INPLAY_BET_Y = sum(TRANSACTION_COUNT[INPLAY_BET == "Y"])
              , TRANSACTION_COUNT_INPLAY_BET_N = sum(TRANSACTION_COUNT[INPLAY_BET == "N"])
              , AVG_BET_SIZE_INPLAY_BET_Y = sum(AVG_BET_SIZE[INPLAY_BET == "Y"] * TRANSACTION_COUNT[INPLAY_BET == "Y"]) / sum(TRANSACTION_COUNT[INPLAY_BET == "Y"])
              , AVG_BET_SIZE_INPLAY_BET_N = sum(AVG_BET_SIZE[INPLAY_BET == "N"] * TRANSACTION_COUNT[INPLAY_BET == "N"]) / sum(TRANSACTION_COUNT[INPLAY_BET == "N"])
              , MAX_BET_SIZE_INPLAY_BET_Y = max(MAX_BET_SIZE[INPLAY_BET == "Y"])
              , MAX_BET_SIZE_INPLAY_BET_N = max(MAX_BET_SIZE[INPLAY_BET == "N"])
              , MIN_BET_SIZE_INPLAY_BET_Y = min(MIN_BET_SIZE[INPLAY_BET == "Y"])
              , MIN_BET_SIZE_INPLAY_BET_N = min(MIN_BET_SIZE[INPLAY_BET == "N"])
              , STDEV_BET_SIZE_INPLAY_BET_Y = sqrt(sum((STDEV_BET_SIZE[INPLAY_BET == "Y"] ^ 2) * TRANSACTION_COUNT[INPLAY_BET == "Y"]) / sum(TRANSACTION_COUNT[INPLAY_BET == "Y"]))
              , STDEV_BET_SIZE_INPLAY_BET_N = sqrt(sum((STDEV_BET_SIZE[INPLAY_BET == "N"] ^ 2) * TRANSACTION_COUNT[INPLAY_BET == "N"]) / sum(TRANSACTION_COUNT[INPLAY_BET == "N"]))
              
              , NO_OF_BID_TYPE = sum(CNT_BID_TYPE) # this is just an estimate as we dont have B and L visible
              , NO_OF_INPLAY_BET = n_distinct(INPLAY_BET))

# modify NO_OF_BID_TYPE in line with the train
dtTest[, NO_OF_BID_TYPE := ifelse(NO_OF_BID_TYPE > 0, 2, 1)]

##############################
## 1.3 combine train and test so they can process together ######
##############################
identical(names(dt1.0), names(dtTest))
dt1.1 <- rbind(dt1.0, dtTest)
dim(dt1.0); dim(dtTest); dim(dt1.1)
# [1] 174226     22
# [1] 22976    22
# [1] 197202     22
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
apply(dt1.1, 2, function(x) mean(is.na(x)))
# ACCOUNT_ID      COUNTRY_OF_RESIDENCE_NAME                       EVENT_ID 
# 0.00000000                     0.02005051                     0.00000000 
# MATCH                    PROFIT_LOSS              TRANSACTION_COUNT 
# 0.00000000                     0.00000000                     0.00000000 
# AVG_BET_SIZE                   MAX_BET_SIZE                   MIN_BET_SIZE 
# 0.00000000                     0.00000000                     0.00000000 
# STDEV_BET_SIZE TRANSACTION_COUNT_INPLAY_BET_Y TRANSACTION_COUNT_INPLAY_BET_N 
# 0.25923672                     0.00000000                     0.00000000 
# AVG_BET_SIZE_INPLAY_BET_Y      AVG_BET_SIZE_INPLAY_BET_N      MAX_BET_SIZE_INPLAY_BET_Y 
# 0.19691484                     0.61267127                     0.00000000 
# MAX_BET_SIZE_INPLAY_BET_N      MIN_BET_SIZE_INPLAY_BET_Y      MIN_BET_SIZE_INPLAY_BET_N 
# 0.00000000                     0.00000000                     0.00000000 
# STDEV_BET_SIZE_INPLAY_BET_Y    STDEV_BET_SIZE_INPLAY_BET_N                 NO_OF_BID_TYPE 
# 0.36730358                     0.82137098                     0.00000000 
# NO_OF_INPLAY_BET                     INPLAY_BET 
# 0.00000000                     0.00000000 
dt1.1$COUNTRY_OF_RESIDENCE_NAME[is.na(dt1.1$COUNTRY_OF_RESIDENCE_NAME)] <- "Unknown"
dt1.1$STDEV_BET_SIZE[is.na(dt1.1$STDEV_BET_SIZE)] <- 0
dt1.1$AVG_BET_SIZE_INPLAY_BET_Y[is.na(dt1.1$AVG_BET_SIZE_INPLAY_BET_Y)] <- 0
dt1.1$AVG_BET_SIZE_INPLAY_BET_N[is.na(dt1.1$AVG_BET_SIZE_INPLAY_BET_N)] <- 0
dt1.1$STDEV_BET_SIZE_INPLAY_BET_Y[is.na(dt1.1$STDEV_BET_SIZE_INPLAY_BET_Y)] <- 0
dt1.1$STDEV_BET_SIZE_INPLAY_BET_N[is.na(dt1.1$STDEV_BET_SIZE_INPLAY_BET_N)] <- 0
# after
apply(dt1.1, 2, function(x) mean(is.na(x)))
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
dtTempSeqTest <- data.table(EVENT_ID = sort(unique(dtTest$EVENT_ID)), EVENT_SEQ = 44:46)
dtTempSeq <- rbind(dtTempSeqTrain, dtTempSeqTest)
dt1.1 <- merge(dt1.1, dtTempSeq, by = "EVENT_ID")

# add it to the dtTestFeatures
test_seq <- data.table(EVENT_ID = sort(unique(dtTestFeatures$EVENT_ID)), EVENT_SEQ = 44:46)
dtTestFeatures <- merge(dtTestFeatures, test_seq, by = "EVENT_ID")

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
## IND_LOSE ########
####################
dt1.1[, IND_LOSE := ifelse(PROFIT_LOSS <= 0, -1, 0)]

####################
## NO_OF_EVENT_ATTENDED
####################
dt1.1[, ATTENDED := 1]
dt1.1 <- dt1.1[order(EVENT_SEQ)]
dt1.1[, CUM_ATTENDED := cumsum(ATTENDED), by = ACCOUNT_ID]
dt1.1[, NO_OF_EVENT_ATTENDED := shift(CUM_ATTENDED, fill = 0, type = "lag"), by = ACCOUNT_ID]

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
dt1.1$RATE_WIN[is.nan(dt1.1$RATE_WIN)] <- 0

####################
## WIN_LOSE ########
####################
dt1.1[, WIN_LOSE := CUM_WIN + CUM_LOSE]

####################
## TTL_PROFIT_LOSS #
####################
dt1.1 <- dt1.1[order(EVENT_SEQ)]
dt1.1[, CUM_PROFIT_LOSS := cumsum(PROFIT_LOSS), by = ACCOUNT_ID]
dt1.1[, TTL_PROFIT_LOSS := shift(CUM_PROFIT_LOSS, fill = 0, type = "lag"), by = ACCOUNT_ID]

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

LOSE_TEAM <- as.character()
for (i in 1:dim(dt1.1)[1]){
    LOSE_TEAM[i] <- gsub(dt1.1[i]$WIN_TEAM, "", dt1.1[i]$MATCH)
}

LOSE_TEAM <- gsub(" v ", "", LOSE_TEAM)

dt1.1$LOSE_TEAM <- LOSE_TEAM
# this game is England v Scotland
dt1.1$LOSE_TEAM[dt1.1$EVENT_SEQ == 11] <- "United Kingdom"

dt1.1[, IS_FROM_WIN := ifelse(COUNTRY_OF_RESIDENCE_NAME == WIN_TEAM, 1, 0)]
dt1.1[, IS_FROM_LOSE := ifelse(COUNTRY_OF_RESIDENCE_NAME == LOSE_TEAM, 1, 0)]
dt1.1[, IS_FROM_NEITHER := ifelse(COUNTRY_OF_RESIDENCE_NAME != WIN_TEAM & COUNTRY_OF_RESIDENCE_NAME != LOSE_TEAM , 1, 0)]

#############################################
## convert into 3 events per unit ###########
#############################################
# remove some columns
dt.3 <- dt1.1[, c("EVENT_SEQ", "EVENT_ID", "ACCOUNT_ID", "PROFIT_LOSS", "TRANSACTION_COUNT", "AVG_BET_SIZE"
          , "MAX_BET_SIZE", "MIN_BET_SIZE", "STDEV_BET_SIZE", "TRANSACTION_COUNT_INPLAY_BET_Y"
          , "TRANSACTION_COUNT_INPLAY_BET_N", "AVG_BET_SIZE_INPLAY_BET_Y", "AVG_BET_SIZE_INPLAY_BET_N"
          , "MAX_BET_SIZE_INPLAY_BET_Y", "MAX_BET_SIZE_INPLAY_BET_N", "MIN_BET_SIZE_INPLAY_BET_Y"
          , "MIN_BET_SIZE_INPLAY_BET_N", "STDEV_BET_SIZE_INPLAY_BET_Y", "STDEV_BET_SIZE_INPLAY_BET_N"
          , "ME2ME", "TIMES_BEING_A_ME2ME", "IND_IN_AND_OUT_PAY", "TIMES_IN_AND_OUT_PLAY", "TIMES_INPLAY_Y", "TIMES_INPLAY_N"
          , "ODDS_1", "ODDS_2", "SCORE_DIFF", "NO_OF_EVENT_ATTENDED", "NO_OF_WIN", "NO_OF_LOSE"
          , "RATE_WIN", "WIN_LOSE", "TTL_PROFIT_LOSS", "IS_FROM_WIN", "IS_FROM_LOSE", "IS_FROM_NEITHER"
          , "TIMES_ATTENDING_EXPECTED_EVENT", "TIMES_ATTENDING_SUPRISED_EVENT"
          , "IND_RESULT_EXPECTED"), with = F]

dim(dt.3)
# [1] 174226     41

##############################
## 3 in 1 ####################
##############################
MyMode <- function(x){
    return (as.integer(names(sort(-table(x)))[1]))
}
Transform3to1 <- function(dt){
    rangeRand <- range(dt$EVENT_SEQ)
    randFrom <- rangeRand[1]
    randTo <- rangeRand[2] - 2
    
    dtSample <- data.table()
    for (i in randFrom:randTo){
        i.consecutive <- c(i, i + 1, i + 2)
        i.UNIT <- paste(str_pad(i, 2, pad = "0"), str_pad(i + 1, 2, pad = "0"), str_pad(i + 2, 2, pad = "0"), sep = "_")
        dtTemp <- dt[EVENT_SEQ %in% i.consecutive, with = T]
        dtTemp <- dtTemp[, UNIT := i.UNIT]
        dtTemp[, RANK := rank(EVENT_SEQ, ties.method = "first"), by = ACCOUNT_ID]
        
        dtTempAccountUnit <- dtTemp %>%
            group_by(ACCOUNT_ID, UNIT) %>%
            summarise(
                # THIS PROFIT_LOSS
                THIS_PROFIT_LOSS = sum(PROFIT_LOSS)
                
                # THIS TRANSACTION_COUNT
                , THIS_AVG_TRANSACTION_COUNT = sum(TRANSACTION_COUNT) / 3
                , THIS_MAX_TRANSACTION_COUNT = max(TRANSACTION_COUNT)
                , THIS_MIN_TRANSACTION_COUNT = min(TRANSACTION_COUNT)
                , THIS_STDEV_TRANSACTION_COUNT = sd(TRANSACTION_COUNT)
                
                # THIS BET_SIZE
                , THIS_AVG_BET_SIZE = sum(TRANSACTION_COUNT * AVG_BET_SIZE) / 3
                , THIS_MAX_BET_SIZE = max(MAX_BET_SIZE)
                , THIS_MIN_BET_SIZE = min(MIN_BET_SIZE)
                , THIS_STDEV_BET_SIZE = sqrt(sum((STDEV_BET_SIZE ^ 2) * TRANSACTION_COUNT) / sum(TRANSACTION_COUNT))
                
                # THIS TRANSACTION_COUNT INPLAY
                , TBIS_AVG_TRANSACTION_COUNT_INPLAY_Y = sum(TRANSACTION_COUNT_INPLAY_BET_Y) / 3
                , TBIS_AVG_TRANSACTION_COUNT_INPLAY_N = sum(TRANSACTION_COUNT_INPLAY_BET_N) / 3
                , TBIS_MAX_TRANSACTION_COUNT_INPLAY_Y = max(TRANSACTION_COUNT_INPLAY_BET_Y)
                , TBIS_MAX_TRANSACTION_COUNT_INPLAY_N = max(TRANSACTION_COUNT_INPLAY_BET_N)
                , THIS_MIN_TRANSACTION_COUNT_INPLAY_Y = min(TRANSACTION_COUNT_INPLAY_BET_Y)
                , TBIS_MIN_TRANSACTION_COUNT_INPLAY_N = min(TRANSACTION_COUNT_INPLAY_BET_N)
                , THIS_STDEV_TRANSACTION_COUNT_INPLAY_BET_Y = sd(TRANSACTION_COUNT_INPLAY_BET_Y)
                , THIS_STDEV_TRANSACTION_COUNT_INPLAY_BET_N = sd(TRANSACTION_COUNT_INPLAY_BET_N)
                
                # THIS BET_SIZE INPLAY
                , THIS_AVG_BET_SIZE_INPLAY_Y = sum(AVG_BET_SIZE_INPLAY_BET_Y * TRANSACTION_COUNT_INPLAY_BET_Y) / 3
                , THIS_AVG_BET_SIZE_INPLAY_N = sum(AVG_BET_SIZE_INPLAY_BET_N * TRANSACTION_COUNT_INPLAY_BET_N) / 3
                , THIS_MAX_BET_SIZE_INPLAY_Y = max(MAX_BET_SIZE_INPLAY_BET_Y)
                , THIS_MAX_BET_SIZE_INPLAY_N = max(MAX_BET_SIZE_INPLAY_BET_N)
                , THIS_MIN_BET_SIZE_INPLAY_Y = min(MIN_BET_SIZE_INPLAY_BET_Y)
                , THIS_MIN_BET_SIZE_INPLAY_N = min(MIN_BET_SIZE_INPLAY_BET_N)
                , THIS_STDEV_BET_SIZE_INPLAY_Y = sqrt(sum((STDEV_BET_SIZE_INPLAY_BET_Y ^ 2) * TRANSACTION_COUNT_INPLAY_BET_Y) / sum(TRANSACTION_COUNT_INPLAY_BET_Y))
                , THIS_STDEV_BET_SIZE_INPLAY_N = sqrt(sum((STDEV_BET_SIZE_INPLAY_BET_N ^ 2) * TRANSACTION_COUNT_INPLAY_BET_N) / sum(TRANSACTION_COUNT_INPLAY_BET_N))
                
                # THIS ME2ME
                , THIS_ME2ME = MyMode(ME2ME)
                
                # THIS IN_AND_OUT_PLAY
                , THIS_IN_AND_OUT_PLAY = MyMode(IND_IN_AND_OUT_PAY)
                
                # THIS ODDS (should be calculated alone, with UNIT only, not ACCOUNT_ID)
#                 , THIS_AVG_ODDS_1 = mean(ODDS_1)
#                 , THIS_AVG_ODDS_2 = mean(ODDS_2)
#                 , THIS_MAX_ODDS_1 = max(ODDS_1)
#                 , THIS_MAX_ODDS_2 = max(ODDS_2)
#                 , THIS_MIN_ODDS_1 = min(ODDS_1)
#                 , THIS_MIN_ODDS_2 = min(ODDS_2)
#                 , THIS_STDEV_ODDS_1 = sd(ODDS_1)
#                 , THIS_STDEV_ODDS_2 = sd(ODDS_2)
                
                # THIS SCORE_DIFF (should be calcualted along, with UNIT only, not ACCOUNT_ID)
#                 , THIS_AVG_SCORE_DIFF = mean(SCORE_DIFF)
#                 , THIS_MAX_SCORE_DIFF = max(SCORE_DIFF)
#                 , THIS_MIN_SCORE_DIFF = min(SCORE_DIFF)
#                 , THIS_STDEV_SCORE_DIFF = sd(SCORE_DIFF)
                
                # THIS RESULT_EXPECTED (should be calcualted along, with UNIT only, not ACCOUNT_ID)
                # , THIS_RESULT_EXPECTED = MyMode(IND_RESULT_EXPECTED) # mode
                
                # THIS IS_FROM_WIN
                , THIS_IS_FROM_WIN = MyMode(IS_FROM_WIN)
                
                # THIS IS_FROM_LOSE
                , THIS_IS_FROM_LOSE = MyMode(IS_FROM_LOSE)
                
                # THIS IS_FROM_NEITHER
                , THIS_IS_FROM_NEITHER = MyMode(IS_FROM_NEITHER)
                
                # PRE TIMES_BEING_A_ME2ME
                , PRE_TIMES_BEING_A_ME2ME = sum(TIMES_BEING_A_ME2ME[RANK == 1])
                
                # PRE TIMES_IN_AND_OUT_PLAY
                , PRE_TIMES_IN_AND_OUT_PLAY = sum(TIMES_IN_AND_OUT_PLAY[RANK == 1])
                
                # PRE TIMES_INPLAY_Y
                , PRE_TIMES_INPLAY_Y = sum(TIMES_INPLAY_Y[RANK == 1])
                
                # PRE TIMES_INPLAY_N
                , PRE_TIMES_INPLAY_N = sum(TIMES_INPLAY_N[RANK == 1])
                
                # PRE NO_OF_EVENT_ATTENDED
                , PRE_NO_OF_EVENT_ATTENDED = sum(NO_OF_EVENT_ATTENDED[RANK == 1])
                
                # PRE NO_OF_WIN
                , PRE_NO_OF_WIN = sum(NO_OF_WIN[RANK == 1])
                
                # PRE NO_OF_LOSE
                , PRE_NO_OF_LOSE = sum(NO_OF_LOSE[RANK == 1])
                
                # PRE RATE_WIN
                , PRE_RATE_WIN = sum(RATE_WIN[RANK == 1])
                
                # PRE WIN_LOSE
                , PRE_WIN_LOSE = sum(WIN_LOSE[RANK == 1])
                
                # TTL_PROFIT_LOSS
                , PRE_TTL_PROFIT_LOSS = sum(TTL_PROFIT_LOSS[RANK == 1])
                
                # PRE TIMES_ATTENDING_EXPECTED_EVENT
                , PRE_TIMES_ATTENDING_EXPECTED_EVENT = sum(TIMES_ATTENDING_EXPECTED_EVENT[RANK == 1])
                
                # PRE TIMES_ATTENDING_SUPRISED_EVENT
                , PRE_TIMES_ATTENDING_SUPRISED_EVENT = sum(TIMES_ATTENDING_SUPRISED_EVENT[RANK == 1])
            )
        
        dtTempUnit <- dtTemp %>%
            group_by(UNIT) %>%
            summarise(
                # THIS ODDS (should be calculated alone, with UNIT only, not ACCOUNT_ID)
                THIS_AVG_ODDS_1 = mean(ODDS_1)
                , THIS_AVG_ODDS_2 = mean(ODDS_2)
                , THIS_MAX_ODDS_1 = max(ODDS_1)
                , THIS_MAX_ODDS_2 = max(ODDS_2)
                , THIS_MIN_ODDS_1 = min(ODDS_1)
                , THIS_MIN_ODDS_2 = min(ODDS_2)
                , THIS_STDEV_ODDS_1 = sd(ODDS_1)
                , THIS_STDEV_ODDS_2 = sd(ODDS_2)
                
                # THIS SCORE_DIFF (should be calcualted along, with UNIT only, not ACCOUNT_ID)
                , THIS_AVG_SCORE_DIFF = mean(SCORE_DIFF)
                , THIS_MAX_SCORE_DIFF = max(SCORE_DIFF)
                , THIS_MIN_SCORE_DIFF = min(SCORE_DIFF)
                , THIS_STDEV_SCORE_DIFF = sd(SCORE_DIFF)
                
                # THIS RESULT_EXPECTED (should be calcualted along, with UNIT only, not ACCOUNT_ID)
                , THIS_RESULT_EXPECTED = MyMode(IND_RESULT_EXPECTED) # mode
            )
        
        dtTempMerge <- merge(dtTempAccountUnit, dtTempUnit, by = "UNIT", all.x = T)
        dtSample <- rbind(dtSample, dtTempMerge)
    }
    
    return(dtSample)
}
# apply on dt.3
dt.3in1 <- Transform3to1(dt.3)
dim(dt.3in1)
# NAs
# Before
apply(dt.3in1, 2, function(x) mean(is.na(x)))
# UNIT                                ACCOUNT_ID 
# 0.0000000                                 0.0000000 
# THIS_PROFIT_LOSS                THIS_AVG_TRANSACTION_COUNT 
# 0.0000000                                 0.0000000 
# THIS_MAX_TRANSACTION_COUNT                THIS_MIN_TRANSACTION_COUNT 
# 0.0000000                                 0.0000000 
# THIS_STDEV_TRANSACTION_COUNT                         THIS_AVG_BET_SIZE 
# 0.5135764                                 0.0000000 
# THIS_MAX_BET_SIZE                         THIS_MIN_BET_SIZE 
# 0.0000000                                 0.0000000 
# THIS_STDEV_BET_SIZE       TBIS_AVG_TRANSACTION_COUNT_INPLAY_Y 
# 0.0000000                                 0.0000000 
# TBIS_AVG_TRANSACTION_COUNT_INPLAY_N       TBIS_MAX_TRANSACTION_COUNT_INPLAY_Y 
# 0.0000000                                 0.0000000 
# TBIS_MAX_TRANSACTION_COUNT_INPLAY_N       THIS_MIN_TRANSACTION_COUNT_INPLAY_Y 
# 0.0000000                                 0.0000000 
# TBIS_MIN_TRANSACTION_COUNT_INPLAY_N THIS_STDEV_TRANSACTION_COUNT_INPLAY_BET_Y 
# 0.0000000                                 0.5135764 
# THIS_STDEV_TRANSACTION_COUNT_INPLAY_BET_N                THIS_AVG_BET_SIZE_INPLAY_Y 
# 0.5135764                                 0.0000000 
# THIS_AVG_BET_SIZE_INPLAY_N                THIS_MAX_BET_SIZE_INPLAY_Y 
# 0.0000000                                 0.0000000 
# THIS_MAX_BET_SIZE_INPLAY_N                THIS_MIN_BET_SIZE_INPLAY_Y 
# 0.0000000                                 0.0000000 
# THIS_MIN_BET_SIZE_INPLAY_N              THIS_STDEV_BET_SIZE_INPLAY_Y 
# 0.0000000                                 0.1748089 
# THIS_STDEV_BET_SIZE_INPLAY_N                                THIS_ME2ME 
# 0.5334148                                 0.0000000 
# THIS_IN_AND_OUT_PLAY                          THIS_IS_FROM_WIN 
# 0.0000000                                 0.0000000 
# THIS_IS_FROM_LOSE                      THIS_IS_FROM_NEITHER 
# 0.0000000                                 0.0000000 
# PRE_TIMES_BEING_A_ME2ME                 PRE_TIMES_IN_AND_OUT_PLAY 
# 0.0000000                                 0.0000000 
# PRE_TIMES_INPLAY_Y                        PRE_TIMES_INPLAY_N 
# 0.0000000                                 0.0000000 
# PRE_NO_OF_EVENT_ATTENDED                             PRE_NO_OF_WIN 
# 0.0000000                                 0.0000000 
# PRE_NO_OF_LOSE                              PRE_RATE_WIN 
# 0.0000000                                 0.0000000 
# PRE_WIN_LOSE                       PRE_TTL_PROFIT_LOSS 
# 0.0000000                                 0.0000000 
# PRE_TIMES_ATTENDING_EXPECTED_EVENT        PRE_TIMES_ATTENDING_SUPRISED_EVENT 
# 0.0000000                                 0.0000000 
# THIS_AVG_ODDS_1                           THIS_AVG_ODDS_2 
# 0.0000000                                 0.0000000 
# THIS_MAX_ODDS_1                           THIS_MAX_ODDS_2 
# 0.0000000                                 0.0000000 
# THIS_MIN_ODDS_1                           THIS_MIN_ODDS_2 
# 0.0000000                                 0.0000000 
# THIS_STDEV_ODDS_1                         THIS_STDEV_ODDS_2 
# 0.0000000                                 0.0000000 
# THIS_AVG_SCORE_DIFF                       THIS_MAX_SCORE_DIFF 
# 0.0000000                                 0.0000000 
# THIS_MIN_SCORE_DIFF                     THIS_STDEV_SCORE_DIFF 
# 0.0000000                                 0.0000000 
# THIS_RESULT_EXPECTED 
# 0.0000000 

dt.3in1$THIS_STDEV_TRANSACTION_COUNT[is.na(dt.3in1$THIS_STDEV_TRANSACTION_COUNT)] <- 0
dt.3in1$THIS_STDEV_TRANSACTION_COUNT_INPLAY_BET_N[is.na(dt.3in1$THIS_STDEV_TRANSACTION_COUNT_INPLAY_BET_N)] <- 0
dt.3in1$THIS_STDEV_TRANSACTION_COUNT_INPLAY_BET_Y[is.na(dt.3in1$THIS_STDEV_TRANSACTION_COUNT_INPLAY_BET_Y)] <- 0
dt.3in1$THIS_STDEV_BET_SIZE_INPLAY_Y[is.na(dt.3in1$THIS_STDEV_BET_SIZE_INPLAY_Y)] <- 0
dt.3in1$THIS_STDEV_BET_SIZE_INPLAY_N[is.na(dt.3in1$THIS_STDEV_BET_SIZE_INPLAY_N)] <- 0

# after
apply(dt.3in1, 2, function(x) mean(is.na(x)))
# UNIT                                ACCOUNT_ID 
# 0                                         0 
# THIS_PROFIT_LOSS                THIS_AVG_TRANSACTION_COUNT 
# 0                                         0 
# THIS_MAX_TRANSACTION_COUNT                THIS_MIN_TRANSACTION_COUNT 
# 0                                         0 
# THIS_STDEV_TRANSACTION_COUNT                         THIS_AVG_BET_SIZE 
# 0                                         0 
# THIS_MAX_BET_SIZE                         THIS_MIN_BET_SIZE 
# 0                                         0 
# THIS_STDEV_BET_SIZE       TBIS_AVG_TRANSACTION_COUNT_INPLAY_Y 
# 0                                         0 
# TBIS_AVG_TRANSACTION_COUNT_INPLAY_N       TBIS_MAX_TRANSACTION_COUNT_INPLAY_Y 
# 0                                         0 
# TBIS_MAX_TRANSACTION_COUNT_INPLAY_N       THIS_MIN_TRANSACTION_COUNT_INPLAY_Y 
# 0                                         0 
# TBIS_MIN_TRANSACTION_COUNT_INPLAY_N THIS_STDEV_TRANSACTION_COUNT_INPLAY_BET_Y 
# 0                                         0 
# THIS_STDEV_TRANSACTION_COUNT_INPLAY_BET_N                THIS_AVG_BET_SIZE_INPLAY_Y 
# 0                                         0 
# THIS_AVG_BET_SIZE_INPLAY_N                THIS_MAX_BET_SIZE_INPLAY_Y 
# 0                                         0 
# THIS_MAX_BET_SIZE_INPLAY_N                THIS_MIN_BET_SIZE_INPLAY_Y 
# 0                                         0 
# THIS_MIN_BET_SIZE_INPLAY_N              THIS_STDEV_BET_SIZE_INPLAY_Y 
# 0                                         0 
# THIS_STDEV_BET_SIZE_INPLAY_N                                THIS_ME2ME 
# 0                                         0 
# THIS_IN_AND_OUT_PLAY                          THIS_IS_FROM_WIN 
# 0                                         0 
# THIS_IS_FROM_LOSE                      THIS_IS_FROM_NEITHER 
# 0                                         0 
# PRE_TIMES_BEING_A_ME2ME                 PRE_TIMES_IN_AND_OUT_PLAY 
# 0                                         0 
# PRE_TIMES_INPLAY_Y                        PRE_TIMES_INPLAY_N 
# 0                                         0 
# PRE_NO_OF_EVENT_ATTENDED                             PRE_NO_OF_WIN 
# 0                                         0 
# PRE_NO_OF_LOSE                              PRE_RATE_WIN 
# 0                                         0 
# PRE_WIN_LOSE                       PRE_TTL_PROFIT_LOSS 
# 0                                         0 
# PRE_TIMES_ATTENDING_EXPECTED_EVENT        PRE_TIMES_ATTENDING_SUPRISED_EVENT 
# 0                                         0 
# THIS_AVG_ODDS_1                           THIS_AVG_ODDS_2 
# 0                                         0 
# THIS_MAX_ODDS_1                           THIS_MAX_ODDS_2 
# 0                                         0 
# THIS_MIN_ODDS_1                           THIS_MIN_ODDS_2 
# 0                                         0 
# THIS_STDEV_ODDS_1                         THIS_STDEV_ODDS_2 
# 0                                         0 
# THIS_AVG_SCORE_DIFF                       THIS_MAX_SCORE_DIFF 
# 0                                         0 
# THIS_MIN_SCORE_DIFF                     THIS_STDEV_SCORE_DIFF 
# 0                                         0 
# THIS_RESULT_EXPECTED 
# 0 

##############################
## 1.6 change class ##########
##############################
# before
str(dt.3in1)
# $ UNIT                                     : chr  "01_02_03" "01_02_03" "01_02_03" "01_02_03" ...
# $ ACCOUNT_ID                               : chr  "1017744" "1005972" "1017103" "1009855" ...
# $ THIS_PROFIT_LOSS                         : num  -166.2 48.5 -419.3 66.7 889 ...
# $ THIS_AVG_TRANSACTION_COUNT               : num  5 2.67 4 27 9 ...
# $ THIS_MAX_TRANSACTION_COUNT               : int  7 3 6 37 15 19 56 6 12 22 ...
# $ THIS_MIN_TRANSACTION_COUNT               : int  2 2 2 9 5 2 13 1 12 21 ...
# $ THIS_STDEV_TRANSACTION_COUNT             : num  2.646 0.577 2 15.62 5.292 ...
# $ THIS_AVG_BET_SIZE                        : num  785 505 305 3155 9789 ...
# $ THIS_MAX_BET_SIZE                        : num  644 303 161 1032 12876 ...
# $ THIS_MIN_BET_SIZE                        : num  2.56 73.31 6.44 9.4 5.15 ...
# $ THIS_STDEV_BET_SIZE                      : num  153.2 80.3 64.3 152.5 2359.2 ...
# $ TBIS_AVG_TRANSACTION_COUNT_INPLAY_Y      : num  4.33 2.67 3 22 9 ...
# $ TBIS_AVG_TRANSACTION_COUNT_INPLAY_N      : num  0.667 0 1 5 0 ...
# $ TBIS_MAX_TRANSACTION_COUNT_INPLAY_Y      : int  7 3 5 31 15 19 56 6 12 22 ...
# $ TBIS_MAX_TRANSACTION_COUNT_INPLAY_N      : int  2 0 1 9 0 0 1 0 0 0 ...
# $ THIS_MIN_TRANSACTION_COUNT_INPLAY_Y      : int  2 2 1 9 5 2 13 1 12 21 ...
# $ TBIS_MIN_TRANSACTION_COUNT_INPLAY_N      : int  0 0 1 0 0 0 0 0 0 0 ...
# $ THIS_STDEV_TRANSACTION_COUNT_INPLAY_BET_Y: num  2.517 0.577 2 11.533 5.292 ...
# $ THIS_STDEV_TRANSACTION_COUNT_INPLAY_BET_N: num  1.15 0 0 4.58 0 ...
# $ THIS_AVG_BET_SIZE_INPLAY_Y               : num  768 505 208 2382 9789 ...
# $ THIS_AVG_BET_SIZE_INPLAY_N               : num  17.2 0 96.6 772.6 0 ...
# $ THIS_MAX_BET_SIZE_INPLAY_Y               : num  644 303 161 1032 12876 ...
# $ THIS_MAX_BET_SIZE_INPLAY_N               : num  31.7 0 161 257.6 0 ...
# $ THIS_MIN_BET_SIZE_INPLAY_Y               : num  2.56 73.31 6.44 10.51 5.15 ...
# $ THIS_MIN_BET_SIZE_INPLAY_N               : num  0 0 64.4 0 0 ...
# $ THIS_STDEV_BET_SIZE_INPLAY_Y             : num  163.5 80.3 68.7 163.6 2359.2 ...
# $ THIS_STDEV_BET_SIZE_INPLAY_N             : num  8.38 0 0 83.33 0 ...
# $ THIS_ME2ME                               : int  1 0 1 1 1 1 1 1 1 1 ...
# $ THIS_IN_AND_OUT_PLAY                     : int  0 0 1 1 0 0 0 0 0 0 ...
# $ THIS_IS_FROM_WIN                         : int  0 0 0 0 0 0 0 0 0 0 ...
# $ THIS_IS_FROM_LOSE                        : int  0 0 0 0 0 0 0 0 0 0 ...
# $ THIS_IS_FROM_NEITHER                     : int  1 1 1 1 1 1 1 1 1 1 ...
# $ PRE_TIMES_BEING_A_ME2ME                  : num  0 0 0 0 0 0 0 0 0 0 ...
# $ PRE_TIMES_IN_AND_OUT_PLAY                : num  0 0 0 0 0 0 0 0 0 0 ...
# $ PRE_TIMES_INPLAY_Y                       : num  0 0 0 0 0 0 0 0 0 0 ...
# $ PRE_TIMES_INPLAY_N                       : num  0 0 0 0 0 0 0 0 0 0 ...
# $ PRE_NO_OF_EVENT_ATTENDED                 : num  0 0 0 0 0 0 0 0 0 0 ...
# $ PRE_NO_OF_WIN                            : num  0 0 0 0 0 0 0 0 0 0 ...
# $ PRE_NO_OF_LOSE                           : num  0 0 0 0 0 0 0 0 0 0 ...
# $ PRE_RATE_WIN                             : num  0 0 0 0 0 0 0 0 0 0 ...
# $ PRE_WIN_LOSE                             : num  -1 1 1 -1 1 -1 1 1 -1 1 ...
# $ PRE_TTL_PROFIT_LOSS                      : num  0 0 0 0 0 0 0 0 0 0 ...
# $ PRE_TIMES_ATTENDING_EXPECTED_EVENT       : num  0 0 0 0 0 0 0 0 0 0 ...
# $ PRE_TIMES_ATTENDING_SUPRISED_EVENT       : num  0 0 0 0 0 0 0 0 0 0 ...
# $ THIS_AVG_ODDS_1                          : num  1.31 1.31 1.31 1.31 1.31 ...
# $ THIS_AVG_ODDS_2                          : num  5.53 5.53 5.53 5.53 5.53 ...
# $ THIS_MAX_ODDS_1                          : num  1.56 1.56 1.56 1.56 1.56 1.56 1.56 1.56 1.56 1.56 ...
# $ THIS_MAX_ODDS_2                          : num  12.6 12.6 12.6 12.6 12.6 ...
# $ THIS_MIN_ODDS_1                          : num  1.04 1.04 1.04 1.04 1.04 1.04 1.04 1.04 1.04 1.04 ...
# $ THIS_MIN_ODDS_2                          : num  2.45 2.45 2.45 2.45 2.45 2.45 2.45 2.45 2.45 2.45 ...
# $ THIS_STDEV_ODDS_1                        : num  0.218 0.218 0.218 0.218 0.218 ...
# $ THIS_STDEV_ODDS_2                        : num  3.89 3.89 3.89 3.89 3.89 ...
# $ THIS_AVG_SCORE_DIFF                      : num  44.5 44.5 44.5 44.5 44.5 ...
# $ THIS_MAX_SCORE_DIFF                      : num  76 76 76 76 76 76 76 76 76 76 ...
# $ THIS_MIN_SCORE_DIFF                      : num  -3 -3 -3 -3 -3 -3 -3 -3 -3 -3 ...
# $ THIS_STDEV_SCORE_DIFF                    : num  36 36 36 36 36 ...
# $ THIS_RESULT_EXPECTED                     : int  1 1 1 1 1 1 1 1 1 1 ...

dt.3in1$THIS_ME2ME <- as.factor(dt.3in1$THIS_ME2ME)
dt.3in1$THIS_IN_AND_OUT_PLAY <- as.factor(dt.3in1$THIS_IN_AND_OUT_PLAY)
dt.3in1$THIS_IS_FROM_WIN <- as.factor(dt.3in1$THIS_IS_FROM_WIN)
dt.3in1$THIS_IS_FROM_LOSE <- as.factor(dt.3in1$THIS_IS_FROM_LOSE)
dt.3in1$THIS_IS_FROM_NEITHER <- as.factor(dt.3in1$THIS_IS_FROM_NEITHER)
dt.3in1$THIS_RESULT_EXPECTED <- as.factor(dt.3in1$THIS_RESULT_EXPECTED)
# after
str(dt.3in1)
# $ UNIT                                     : chr  "01_02_03" "01_02_03" "01_02_03" "01_02_03" ...
# $ ACCOUNT_ID                               : chr  "1017744" "1005972" "1017103" "1009855" ...
# $ THIS_PROFIT_LOSS                         : num  -166.2 48.5 -419.3 66.7 889 ...
# $ THIS_AVG_TRANSACTION_COUNT               : num  5 2.67 4 27 9 ...
# $ THIS_MAX_TRANSACTION_COUNT               : int  7 3 6 37 15 19 56 6 12 22 ...
# $ THIS_MIN_TRANSACTION_COUNT               : int  2 2 2 9 5 2 13 1 12 21 ...
# $ THIS_STDEV_TRANSACTION_COUNT             : num  2.646 0.577 2 15.62 5.292 ...
# $ THIS_AVG_BET_SIZE                        : num  785 505 305 3155 9789 ...
# $ THIS_MAX_BET_SIZE                        : num  644 303 161 1032 12876 ...
# $ THIS_MIN_BET_SIZE                        : num  2.56 73.31 6.44 9.4 5.15 ...
# $ THIS_STDEV_BET_SIZE                      : num  153.2 80.3 64.3 152.5 2359.2 ...
# $ TBIS_AVG_TRANSACTION_COUNT_INPLAY_Y      : num  4.33 2.67 3 22 9 ...
# $ TBIS_AVG_TRANSACTION_COUNT_INPLAY_N      : num  0.667 0 1 5 0 ...
# $ TBIS_MAX_TRANSACTION_COUNT_INPLAY_Y      : int  7 3 5 31 15 19 56 6 12 22 ...
# $ TBIS_MAX_TRANSACTION_COUNT_INPLAY_N      : int  2 0 1 9 0 0 1 0 0 0 ...
# $ THIS_MIN_TRANSACTION_COUNT_INPLAY_Y      : int  2 2 1 9 5 2 13 1 12 21 ...
# $ TBIS_MIN_TRANSACTION_COUNT_INPLAY_N      : int  0 0 1 0 0 0 0 0 0 0 ...
# $ THIS_STDEV_TRANSACTION_COUNT_INPLAY_BET_Y: num  2.517 0.577 2 11.533 5.292 ...
# $ THIS_STDEV_TRANSACTION_COUNT_INPLAY_BET_N: num  1.15 0 0 4.58 0 ...
# $ THIS_AVG_BET_SIZE_INPLAY_Y               : num  768 505 208 2382 9789 ...
# $ THIS_AVG_BET_SIZE_INPLAY_N               : num  17.2 0 96.6 772.6 0 ...
# $ THIS_MAX_BET_SIZE_INPLAY_Y               : num  644 303 161 1032 12876 ...
# $ THIS_MAX_BET_SIZE_INPLAY_N               : num  31.7 0 161 257.6 0 ...
# $ THIS_MIN_BET_SIZE_INPLAY_Y               : num  2.56 73.31 6.44 10.51 5.15 ...
# $ THIS_MIN_BET_SIZE_INPLAY_N               : num  0 0 64.4 0 0 ...
# $ THIS_STDEV_BET_SIZE_INPLAY_Y             : num  163.5 80.3 68.7 163.6 2359.2 ...
# $ THIS_STDEV_BET_SIZE_INPLAY_N             : num  8.38 0 0 83.33 0 ...
# $ THIS_ME2ME                               : Factor w/ 2 levels "0","1": 2 1 2 2 2 2 2 2 2 2 ...
# $ THIS_IN_AND_OUT_PLAY                     : Factor w/ 2 levels "0","1": 1 1 2 2 1 1 1 1 1 1 ...
# $ THIS_IS_FROM_WIN                         : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
# $ THIS_IS_FROM_LOSE                        : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
# $ THIS_IS_FROM_NEITHER                     : Factor w/ 2 levels "0","1": 2 2 2 2 2 2 2 2 2 2 ...
# $ PRE_TIMES_BEING_A_ME2ME                  : num  0 0 0 0 0 0 0 0 0 0 ...
# $ PRE_TIMES_IN_AND_OUT_PLAY                : num  0 0 0 0 0 0 0 0 0 0 ...
# $ PRE_TIMES_INPLAY_Y                       : num  0 0 0 0 0 0 0 0 0 0 ...
# $ PRE_TIMES_INPLAY_N                       : num  0 0 0 0 0 0 0 0 0 0 ...
# $ PRE_NO_OF_EVENT_ATTENDED                 : num  0 0 0 0 0 0 0 0 0 0 ...
# $ PRE_NO_OF_WIN                            : num  0 0 0 0 0 0 0 0 0 0 ...
# $ PRE_NO_OF_LOSE                           : num  0 0 0 0 0 0 0 0 0 0 ...
# $ PRE_RATE_WIN                             : num  0 0 0 0 0 0 0 0 0 0 ...
# $ PRE_WIN_LOSE                             : num  -1 1 1 -1 1 -1 1 1 -1 1 ...
# $ PRE_TTL_PROFIT_LOSS                      : num  0 0 0 0 0 0 0 0 0 0 ...
# $ PRE_TIMES_ATTENDING_EXPECTED_EVENT       : num  0 0 0 0 0 0 0 0 0 0 ...
# $ PRE_TIMES_ATTENDING_SUPRISED_EVENT       : num  0 0 0 0 0 0 0 0 0 0 ...
# $ THIS_AVG_ODDS_1                          : num  1.31 1.31 1.31 1.31 1.31 ...
# $ THIS_AVG_ODDS_2                          : num  5.53 5.53 5.53 5.53 5.53 ...
# $ THIS_MAX_ODDS_1                          : num  1.56 1.56 1.56 1.56 1.56 1.56 1.56 1.56 1.56 1.56 ...
# $ THIS_MAX_ODDS_2                          : num  12.6 12.6 12.6 12.6 12.6 ...
# $ THIS_MIN_ODDS_1                          : num  1.04 1.04 1.04 1.04 1.04 1.04 1.04 1.04 1.04 1.04 ...
# $ THIS_MIN_ODDS_2                          : num  2.45 2.45 2.45 2.45 2.45 2.45 2.45 2.45 2.45 2.45 ...
# $ THIS_STDEV_ODDS_1                        : num  0.218 0.218 0.218 0.218 0.218 ...
# $ THIS_STDEV_ODDS_2                        : num  3.89 3.89 3.89 3.89 3.89 ...
# $ THIS_AVG_SCORE_DIFF                      : num  44.5 44.5 44.5 44.5 44.5 ...
# $ THIS_MAX_SCORE_DIFF                      : num  76 76 76 76 76 76 76 76 76 76 ...
# $ THIS_MIN_SCORE_DIFF                      : num  -3 -3 -3 -3 -3 -3 -3 -3 -3 -3 ...
# $ THIS_STDEV_SCORE_DIFF                    : num  36 36 36 36 36 ...
# $ THIS_RESULT_EXPECTED                     : Factor w/ 2 levels "0","1": 2 2 2 2 2 2 2 2 2 2 ...

##############################
## 1.6 add prediction column #
##############################
dt.3in1[, PRED := ifelse(THIS_PROFIT_LOSS > 0, 1, 0)]

##############################
## 1.7 save it ###############
##############################
save(dt.3in1, file = "../Datathon_Full_Dataset/transformedData.RData")






















