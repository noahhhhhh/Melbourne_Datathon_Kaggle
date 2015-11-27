###########################################################################################
## Transform ##############################################################################
###########################################################################################
setwd("/Volumes/Data Science/Google Drive/data_science_competition/melbourne_datathon/Melbourne_Datathon_Kaggle/")
rm(list = ls()); gc()
require(data.table)
require(dplyr)
require(stringr)
load("../Datathon_Full_Dataset/engineeredData.RData")
#############################################
## 1. convert into 3 events per unit ########
#############################################
# remove some columns
dt.3 <- dt1.1[, !c("EVENT_ID", "COUNTRY_OF_RESIDENCE_NAME", "MATCH"
                   , "OFF_DT", "MIN_PLCAED_DT_PER_MATCH", "RANK", "NO_OF_ACCT", "INPLAY_BET", "RESULT"
                   , "CUM_RESULT_EXPECTED", "CUM_RESULT_SUPRISED", "TIMES_ATTENDING_EXPECTED_EVENT"
                   , "TIMES_ATTENDING_SUPRISED_EVENT", "ATTENDED", "CUM_ATTENDED", "NO_OF_EVENT_ATTENDED"
                   , "NO_OF_EVENT_ATTENDED", "NO_OF_EVENT_ABSENT", "CUM_INPLAY_Y" 
                   , "CUM_INPLAY_N", "TIMES_INPLAY_Y", "TIMES_INPLAY_N", "IND_WIN", "IND_LOSE", "CUM_WIN"
                   , "NO_OF_WIN", "CUM_LOSE", "NO_OF_LOSE", "CUM_PROFIT_LOSS", "TTL_PROFIT_LOSS"
                   , "CUM_AVG_PROFIT_LOSS", "CUM_MAX_PROFIT_LOSS", "CUM_MIN_PROFIT_LOSS"
                   , "CUM_ME2ME", "TIMES_BEING_A_ME2ME", "CUM_IN_AND_OUT_PLAY", "TIMES_IN_AND_OUT_PLAY"
                   , "WIN_TEAM", "LOSE_TEAM"), with = F]

dim(dt.3)
# [1] 197202    106

##############################
## 1.1 3 in 1 ################
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
                ##############################
                ## THIS ######################
                ##############################
                # THIS PROFIT_LOSS
                THIS_PROFIT_LOSS = sum(PROFIT_LOSS)
                
                # THIS TRANSACTION_COUNT
                , THIS_AVG_TRANSACTION_COUNT = mean(TRANSACTION_COUNT, na.rm = T)
                , THIS_MAX_TRANSACTION_COUNT = max(TRANSACTION_COUNT, na.rm = T)
                , THIS_MIN_TRANSACTION_COUNT = min(TRANSACTION_COUNT, na.rm = T)
                , THIS_STDEV_TRANSACTION_COUNT = sd(TRANSACTION_COUNT, na.rm = T)
                
                # THIS BET_SIZE
                , THIS_AVG_BET_SIZE = mean(TRANSACTION_COUNT * AVG_BET_SIZE, na.rm = T)
                , THIS_MAX_BET_SIZE = max(MAX_BET_SIZE, na.rm = T)
                , THIS_MIN_BET_SIZE = min(MIN_BET_SIZE, na.rm = T)
                , THIS_AVG_STDEV_BET_SIZE = mean(STDEV_BET_SIZE, na.rm = T)
                , THIS_MAX_STDEV_BET_SIZE = max(STDEV_BET_SIZE, na.rm = T)
                , THIS_MIN_STDEV_BET_SIZE = min(STDEV_BET_SIZE, na.rm = T)
                , THIS_STDEV_STDEV_BET_SIZE = sd(STDEV_BET_SIZE, na.rm = T)
                
                # THIS TRANSACTION_COUNT INPLAY
                , THIS_AVG_TRANSACTION_COUNT_INPLAY_Y = mean(TRANSACTION_COUNT_INPLAY_BET_Y, na.rm = T)
                , THIS_AVG_TRANSACTION_COUNT_INPLAY_N = mean(TRANSACTION_COUNT_INPLAY_BET_N, na.rm = T)
                , THIS_MAX_TRANSACTION_COUNT_INPLAY_Y = max(TRANSACTION_COUNT_INPLAY_BET_Y, na.rm = T)
                , THIS_MAX_TRANSACTION_COUNT_INPLAY_N = max(TRANSACTION_COUNT_INPLAY_BET_N, na.rm = T)
                , THIS_MIN_TRANSACTION_COUNT_INPLAY_Y = min(TRANSACTION_COUNT_INPLAY_BET_Y, na.rm = T)
                , TBIS_MIN_TRANSACTION_COUNT_INPLAY_N = min(TRANSACTION_COUNT_INPLAY_BET_N, na.rm = T)
                , THIS_STDEV_TRANSACTION_COUNT_INPLAY_BET_Y = sd(TRANSACTION_COUNT_INPLAY_BET_Y, na.rm = T)
                , THIS_STDEV_TRANSACTION_COUNT_INPLAY_BET_N = sd(TRANSACTION_COUNT_INPLAY_BET_N, na.rm = T)
                
                # THIS BET_SIZE INPLAY
                , THIS_AVG_BET_SIZE_INPLAY_Y = mean(AVG_BET_SIZE_INPLAY_BET_Y * TRANSACTION_COUNT_INPLAY_BET_Y, na.rm = T)
                , THIS_AVG_BET_SIZE_INPLAY_N = mean(AVG_BET_SIZE_INPLAY_BET_N * TRANSACTION_COUNT_INPLAY_BET_N, na.rm = T)
                , THIS_MAX_BET_SIZE_INPLAY_Y = max(MAX_BET_SIZE_INPLAY_BET_Y, na.rm = T)
                , THIS_MAX_BET_SIZE_INPLAY_N = max(MAX_BET_SIZE_INPLAY_BET_N, na.rm = T)
                , THIS_MIN_BET_SIZE_INPLAY_Y = min(MIN_BET_SIZE_INPLAY_BET_Y, na.rm = T)
                , THIS_MIN_BET_SIZE_INPLAY_N = min(MIN_BET_SIZE_INPLAY_BET_N, na.rm = T)
                , THIS_AVG_STDEV_BET_SIZE_INPLAY_Y = mean(STDEV_BET_SIZE_INPLAY_BET_Y, na.rm = T)
                , THIS_MIN_STDEV_BET_SIZE_INPLAY_Y = min(STDEV_BET_SIZE_INPLAY_BET_Y, na.rm = T)
                , THIS_MAX_STDEV_BET_SIZE_INPLAY_Y = max(STDEV_BET_SIZE_INPLAY_BET_Y, na.rm = T)
                , THIS_STDEV_STDEV_BET_SIZE_INPLAY_Y = sd(STDEV_BET_SIZE_INPLAY_BET_Y, na.rm = T)
                , THIS_AVG_STDEV_BET_SIZE_INPLAY_N = mean(STDEV_BET_SIZE_INPLAY_BET_N, na.rm = T)
                , THIS_MIN_STDEV_BET_SIZE_INPLAY_N = min(STDEV_BET_SIZE_INPLAY_BET_N, na.rm = T)
                , THIS_MAX_STDEV_BET_SIZE_INPLAY_N = max(STDEV_BET_SIZE_INPLAY_BET_N, na.rm = T)
                , THIS_STDEV_STDEV_BET_SIZE_INPLAY_N = sd(STDEV_BET_SIZE_INPLAY_BET_N, na.rm = T)
                
                # THIS NO_OF_BID_TYPE
                , THIS_NO_OF_BID_TYPE = sum(NO_OF_BID_TYPE, na.rm = T)
                
                # THIS NO_OF_INPLAY_BET
                , THIS_NO_OF_INPLAY_BET = sum(NO_OF_INPLAY_BET, na.rm = T)
                
                # THIS TIME_DIFF
                , THIS_AVG_TIME_DIFF = mean(TIME_DIFF, na.rm = T)
                , THIS_MAX_TIME_DIFF = max(TIME_DIFF, na.rm = T)
                , THIS_MIN_TIME_DIFF = min(TIME_DIFF, na.rm = T)
                , THIS_STDEV_TIME_DIFF = sd(TIME_DIFF, na.rm = T)
                
                # THIS TOP
                , THIS_TOP_1 = sum(ifelse(as.integer(TOP_1) == 1, 0, 1), na.rm = T)
                , THIS_TOP_2 = sum(ifelse(as.integer(TOP_2) == 1, 0, 1), na.rm = T)
                , THIS_TOP_5 = sum(ifelse(as.integer(TOP_5) == 1, 0, 1), na.rm = T)
                , THIS_TOP_10 = sum(ifelse(as.integer(TOP_10) == 1, 0, 1), na.rm = T)
                , THIS_TOP_15 = sum(ifelse(as.integer(TOP_15) == 1, 0, 1), na.rm = T)
                , THIS_TOP_20 = sum(ifelse(as.integer(TOP_20) == 1, 0, 1), na.rm = T)
                , THIS_TOP_25 = sum(ifelse(as.integer(TOP_25) == 1, 0, 1), na.rm = T)
                
                # THIS BOTTOM
                , THIS_BOTTOM_1 = sum(ifelse(as.integer(BOTTOM_1) == 1, 0, 1), na.rm = T)
                , THIS_BOTTOM_2 = sum(ifelse(as.integer(BOTTOM_2) == 1, 0, 1), na.rm = T)
                , THIS_BOTTOM_5 = sum(ifelse(as.integer(BOTTOM_5) == 1, 0, 1), na.rm = T)
                , THIS_BOTTOM_10 = sum(ifelse(as.integer(BOTTOM_10) == 1, 0, 1), na.rm = T)
                , THIS_BOTTOM_15 = sum(ifelse(as.integer(BOTTOM_15) == 1, 0, 1), na.rm = T)
                , THIS_BOTTOM_20 = sum(ifelse(as.integer(BOTTOM_20) == 1, 0, 1), na.rm = T)
                , THIS_BOTTOM_25 = sum(ifelse(as.integer(BOTTOM_25) == 1, 0, 1), na.rm = T)
                
                # THIS ME2ME
                , THIS_ME2ME = sum(ifelse(as.integer(ME2ME) == 1, 0, 1), na.rm = T)
                
                # THIS IN_PLAY
                , THIS_INPLAY_Y = sum(ifelse(as.integer(IND_INPLAY_Y) == 1, 0, 1), na.rm = T)
                , THIS_INPLAY_N = sum(ifelse(as.integer(IND_INPLAY_N) == 1, 0, 1), na.rm = T)
                
                # THIS IN_AND_OUT_PLAY
                , THIS_IN_AND_OUT_PLAY = sum(ifelse(as.integer(IND_IN_AND_OUT_PAY) == 1, 0, 1), na.rm = T)
                
                # THIS IS_FROM_WIN
                , THIS_IS_FROM_WIN = sum(ifelse(as.integer(IS_FROM_WIN) == 1, 0, 1), na.rm = T)
                
                # THIS IS_FROM_LOSE
                , THIS_IS_FROM_LOSE = sum(ifelse(as.integer(IS_FROM_LOSE) == 1, 0, 1), na.rm = T)
                
                # THIS IS_FROM_NEITHER
                , THIS_IS_FROM_NEITHER = sum(ifelse(as.integer(IS_FROM_NEITHER) == 1, 0, 1), na.rm = T)
                
                ##############################
                ## PRE #######################
                ##############################
                # PRE ATTENDED_ABSENT
                , RPE_ATTENDED_ABSENT = sum(ATTENDED_ABSENT[RANK == 1])
                
                # PRE PERC_BEING_A_ME2ME
                , PRE_PERC_BEING_A_ME2ME = sum(PERC_BEING_A_ME2ME[RANK == 1])
                
                # PRE PERC_INPLAY_Y
                , PRE_PERC_INPLAY_Y = sum(PERC_INPLAY_Y[RANK == 1])
                
                # PRE PERC_INPLAY_N
                , PRE_PERC_INPLAY_N = sum(PERC_INPLAY_N[RANK == 1])
                
                # PRE PERC_IN_AND_OUT_PLAY
                , PRE_PERC_IN_AND_OUT_PLAY = sum(PERC_IN_AND_OUT_PLAY[RANK == 1])
                
                # PRE RATE_WIN
                , PRE_RATE_WIN = sum(RATE_WIN[RANK == 1])
                
                # PRE WIN_LOSE
                , PRE_WIN_LOSE = sum(WIN_LOSE[RANK == 1])
                
                # PRE AVG_PROFIT_LOSS
                , PRE_AVG_PROFIT_LOSS = sum(AVG_PROFIT_LOSS[RANK == 1])
                
                # PRE MAX_PROFIT_LOSS
                , PRE_MAX_PROFIT_LOSS = sum(MAX_PROFIT_LOSS[RANK == 1])
                
                # PRE MIN_PROFIT_LOSS
                , PRE_MIN_PROFIT_LOSS = sum(MIN_PROFIT_LOSS[RANK == 1])
                
                # PRE PERC_ATTENDING_EXPECTED_EVENT
                , PRE_PERC_ATTENDING_EXPECTED_EVENT = sum(PERC_ATTENDING_EXPECTED_EVENT[RANK == 1])
                
                # PRE PERC_ATTENDING_SUPRISED_EVENT
                , PRE_PERC_ATTENDING_SUPRISED_EVENT = sum(PERC_ATTENDING_SUPRISED_EVENT[RANK == 1])
                
                ##############################
                ## LAST ######################
                ##############################
                # LAST LAST_PROFIT_LOSS
                , LAST_PROFIT_LOSS = MyMode(LAST_PROFIT_LOSS[RANK == 1])
                
                # LAST LAST_TRANSACTION_COUNT
                , LAST_TRANSACTION_COUNT = MyMode(LAST_TRANSACTION_COUNT[RANK == 1])
                
                # LAST LAST_AVG_BET_SIZE
                , LAST_AVG_BET_SIZE = MyMode(LAST_AVG_BET_SIZE[RANK == 1])
                
                # LAST LAST_MAX_BET_SIZE
                , LAST_MAX_BET_SIZE = MyMode(LAST_MAX_BET_SIZE[RANK == 1])
                
                # LAST LAST_MIN_BET_SIZE
                , LAST_MIN_BET_SIZE = MyMode(LAST_MIN_BET_SIZE[RANK == 1])
                
                # LAST LAST_STDEV_BET_SIZE
                , LAST_STDEV_BET_SIZE = MyMode(LAST_STDEV_BET_SIZE[RANK == 1])
                
                # LAST LAST_TRANSACTION_COUNT_INPLAY_BET_Y
                , LAST_TRANSACTION_COUNT_INPLAY_BET_Y = MyMode(LAST_TRANSACTION_COUNT_INPLAY_BET_Y[RANK == 1])
                
                # LAST LAST_TRANSACTION_COUNT_INPLAY_BET_N
                , LAST_TRANSACTION_COUNT_INPLAY_BET_N = MyMode(LAST_TRANSACTION_COUNT_INPLAY_BET_N[RANK == 1])
                
                # LAST LAST_MAX_BET_SIZE_INPLAY_BET_Y
                , LAST_MAX_BET_SIZE_INPLAY_BET_Y = MyMode(LAST_MAX_BET_SIZE_INPLAY_BET_Y[RANK == 1])
                
                # LAST LAST_MAX_BET_SIZE_INPLAY_BET_N
                , LAST_MAX_BET_SIZE_INPLAY_BET_N = MyMode(LAST_MAX_BET_SIZE_INPLAY_BET_N[RANK == 1])
                
                # LAST LAST_MIN_BET_SIZE_INPLAY_BET_Y
                , LAST_MIN_BET_SIZE_INPLAY_BET_Y = MyMode(LAST_MIN_BET_SIZE_INPLAY_BET_Y[RANK == 1])
                
                # LAST LAST_MIN_BET_SIZE_INPLAY_BET_N
                , LAST_MIN_BET_SIZE_INPLAY_BET_N = MyMode(LAST_MIN_BET_SIZE_INPLAY_BET_N[RANK == 1])
                
                # LAST LAST_STDEV_BET_SIZE_INPLAY_BET_Y
                , LAST_STDEV_BET_SIZE_INPLAY_BET_Y = MyMode(LAST_STDEV_BET_SIZE_INPLAY_BET_Y[RANK == 1])
                
                # LAST LAST_STDEV_BET_SIZE_INPLAY_BET_N
                , LAST_STDEV_BET_SIZE_INPLAY_BET_N = MyMode(LAST_STDEV_BET_SIZE_INPLAY_BET_N[RANK == 1])
                
                # LAST LAST_NO_OF_BID_TYPE
                , LAST_NO_OF_BID_TYPE = MyMode(LAST_NO_OF_BID_TYPE[RANK == 1])
                
                # LAST LAST_NO_OF_INPLAY_BET
                , LAST_NO_OF_INPLAY_BET = MyMode(LAST_NO_OF_INPLAY_BET[RANK == 1])
                
                # LAST_TIME_DIFF
                , LAST_TIME_DIFF = MyMode(LAST_TIME_DIFF[RANK == 1])
                
                # LAST TOP
                , LAST_TOP_1 = MyMode(LAST_TOP_1[RANK == 1])
                , LAST_TOP_2 = MyMode(LAST_TOP_2[RANK == 1])
                , LAST_TOP_5 = MyMode(LAST_TOP_5[RANK == 1])
                , LAST_TOP_10 = MyMode(LAST_TOP_10[RANK == 1])
                , LAST_TOP_15 = MyMode(LAST_TOP_15[RANK == 1])
                , LAST_TOP_20 = MyMode(LAST_TOP_20[RANK == 1])
                , LAST_TOP_25 = MyMode(LAST_TOP_25[RANK == 1])
                
                # LAST BOTTOM
                , LAST_BOTTOM_1 = MyMode(LAST_BOTTOM_1[RANK == 1])
                , LAST_BOTTOM_2 = MyMode(LAST_BOTTOM_2[RANK == 1])
                , LAST_BOTTOM_5 = MyMode(LAST_BOTTOM_5[RANK == 1])
                , LAST_BOTTOM_10 = MyMode(LAST_BOTTOM_10[RANK == 1])
                , LAST_BOTTOM_15 = MyMode(LAST_BOTTOM_15[RANK == 1])
                , LAST_BOTTOM_20 = MyMode(LAST_BOTTOM_20[RANK == 1])
                , LAST_BOTTOM_25 = MyMode(LAST_BOTTOM_25[RANK == 1])
                
                # LAST LAST_ME2ME
                , LAST_ME2ME = MyMode(LAST_ME2ME[RANK == 1])
                
                # LAST LAST_ODDS_1
                , LAST_ODDS_1 = MyMode(LAST_ODDS_1[RANK == 1])
                
                # LAST LAST_ODDS_2
                , LAST_ODDS_2 = MyMode(LAST_ODDS_2[RANK == 1])
                
                # LAST LAST_IND_INPLAY_Y
                , LAST_IND_INPLAY_Y = MyMode(LAST_IND_INPLAY_Y[RANK == 1])
                
                # LAST LAST_IND_INPLAY_N
                , LAST_IND_INPLAY_N = MyMode(LAST_IND_INPLAY_N[RANK == 1])
                
                # LAST LAST_IND_IN_AND_OUT_PAY
                , LAST_IND_IN_AND_OUT_PAY = MyMode(LAST_IND_IN_AND_OUT_PAY[RANK == 1])
                
                # LAST LAST_IND_RESULT_EXPECTED
                , LAST_IND_RESULT_EXPECTED = MyMode(LAST_IND_RESULT_EXPECTED[RANK == 1])
                
                # LAST LAST_IND_RESULT_SUPRISED
                , LAST_IND_RESULT_SUPRISED = MyMode(LAST_IND_RESULT_SUPRISED[RANK == 1])
                
                # LAST LAST_SCORE_DIFF
                , LAST_SCORE_DIFF = MyMode(LAST_SCORE_DIFF[RANK == 1])
                
                # LAST LAST_IS_FROM_WIN
                , LAST_IS_FROM_WIN = MyMode(LAST_IS_FROM_WIN[RANK == 1])
                
                # LAST LAST_IS_FROM_LOSE
                , LAST_IS_FROM_LOSE = MyMode(LAST_IS_FROM_LOSE[RANK == 1])
                
                # LAST LAST_IS_FROM_NEITHER
                , LAST_IS_FROM_NEITHER = MyMode(LAST_IS_FROM_NEITHER[RANK == 1])
            )
        
        dtTempUnit <- dtTemp %>%
            group_by(UNIT) %>%
            summarise(
                # THIS ODDS (should be calculated alone, with UNIT only, not ACCOUNT_ID)
                THIS_AVG_ODDS_1 = mean(ODDS_1, na.rm = T)
                , THIS_AVG_ODDS_2 = mean(ODDS_2, na.rm = T)
                , THIS_MAX_ODDS_1 = max(ODDS_1, na.rm = T)
                , THIS_MAX_ODDS_2 = max(ODDS_2, na.rm = T)
                , THIS_MIN_ODDS_1 = min(ODDS_1, na.rm = T)
                , THIS_MIN_ODDS_2 = min(ODDS_2, na.rm = T)
                , THIS_STDEV_ODDS_1 = sd(ODDS_1, na.rm = T)
                , THIS_STDEV_ODDS_2 = sd(ODDS_2, na.rm = T)
                
                # THIS SCORE_DIFF (should be calcualted along, with UNIT only, not ACCOUNT_ID)
                , THIS_AVG_SCORE_DIFF = mean(SCORE_DIFF, na.rm = T)
                , THIS_MAX_SCORE_DIFF = max(SCORE_DIFF, na.rm = T)
                , THIS_MIN_SCORE_DIFF = min(SCORE_DIFF, na.rm = T)
                , THIS_STDEV_SCORE_DIFF = sd(SCORE_DIFF, na.rm = T)
                
                # THIS RESULT_EXPECTED (should be calcualted along, with UNIT only, not ACCOUNT_ID)
                , THIS_RESULT_EXPECTED = MyMode(ifelse(as.integer(IND_RESULT_EXPECTED) == 1, 0, 1))
                
                # THIS RESULT_SUPRISED (should be calcualted along, with UNIT only, not ACCOUNT_ID)
                , THIS_RESULT_SUPRISED = MyMode(ifelse(as.integer(IND_RESULT_SUPRISED) == 1, 0, 1))
            )
        
        dtTempMerge <- merge(dtTempAccountUnit, dtTempUnit, by = "UNIT", all.x = T)
        dtSample <- rbind(dtSample, dtTempMerge)
    }
    
    return(dtSample)
}
# apply on dt.3
dt.3in1 <- Transform3to1(dt.3)
dim(dt.3in1)
# [1] 333999    132

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
# THIS_AVG_STDEV_BET_SIZE                   THIS_MAX_STDEV_BET_SIZE 
# 0.0000000                                 0.0000000 
# THIS_MIN_STDEV_BET_SIZE                 THIS_STDEV_STDEV_BET_SIZE 
# 0.0000000                                 0.5135764 
# THIS_AVG_TRANSACTION_COUNT_INPLAY_Y       THIS_AVG_TRANSACTION_COUNT_INPLAY_N 
# 0.0000000                                 0.0000000 
# THIS_MAX_TRANSACTION_COUNT_INPLAY_Y       THIS_MAX_TRANSACTION_COUNT_INPLAY_N 
# 0.0000000                                 0.0000000 
# THIS_MIN_TRANSACTION_COUNT_INPLAY_Y       TBIS_MIN_TRANSACTION_COUNT_INPLAY_N 
# 0.0000000                                 0.0000000 
# THIS_STDEV_TRANSACTION_COUNT_INPLAY_BET_Y THIS_STDEV_TRANSACTION_COUNT_INPLAY_BET_N 
# 0.5135764                                 0.5135764 
# THIS_AVG_BET_SIZE_INPLAY_Y                THIS_AVG_BET_SIZE_INPLAY_N 
# 0.0000000                                 0.0000000 
# THIS_MAX_BET_SIZE_INPLAY_Y                THIS_MAX_BET_SIZE_INPLAY_N 
# 0.0000000                                 0.0000000 
# THIS_MIN_BET_SIZE_INPLAY_Y                THIS_MIN_BET_SIZE_INPLAY_N 
# 0.0000000                                 0.0000000 
# THIS_AVG_STDEV_BET_SIZE_INPLAY_Y          THIS_MIN_STDEV_BET_SIZE_INPLAY_Y 
# 0.0000000                                 0.0000000 
# THIS_MAX_STDEV_BET_SIZE_INPLAY_Y        THIS_STDEV_STDEV_BET_SIZE_INPLAY_Y 
# 0.0000000                                 0.5135764 
# THIS_AVG_STDEV_BET_SIZE_INPLAY_N          THIS_MIN_STDEV_BET_SIZE_INPLAY_N 
# 0.0000000                                 0.0000000 
# THIS_MAX_STDEV_BET_SIZE_INPLAY_N        THIS_STDEV_STDEV_BET_SIZE_INPLAY_N 
# 0.0000000                                 0.5135764 
# THIS_NO_OF_BID_TYPE                     THIS_NO_OF_INPLAY_BET 
# 0.0000000                                 0.0000000 
# THIS_AVG_TIME_DIFF                        THIS_MAX_TIME_DIFF 
# 0.0000000                                 0.0000000 
# THIS_MIN_TIME_DIFF                      THIS_STDEV_TIME_DIFF 
# 0.0000000                                 0.5135764 
# THIS_TOP_1                                THIS_TOP_2 
# 0.0000000                                 0.0000000 
# THIS_TOP_5                               THIS_TOP_10 
# 0.0000000                                 0.0000000 
# THIS_TOP_15                               THIS_TOP_20 
# 0.0000000                                 0.0000000 
# THIS_TOP_25                             THIS_BOTTOM_1 
# 0.0000000                                 0.0000000 
# THIS_BOTTOM_2                             THIS_BOTTOM_5 
# 0.0000000                                 0.0000000 
# THIS_BOTTOM_10                            THIS_BOTTOM_15 
# 0.0000000                                 0.0000000 
# THIS_BOTTOM_20                            THIS_BOTTOM_25 
# 0.0000000                                 0.0000000 
# THIS_ME2ME                             THIS_INPLAY_Y 
# 0.0000000                                 0.0000000 
# THIS_INPLAY_N                      THIS_IN_AND_OUT_PLAY 
# 0.0000000                                 0.0000000 
# THIS_IS_FROM_WIN                         THIS_IS_FROM_LOSE 
# 0.0000000                                 0.0000000 
# THIS_IS_FROM_NEITHER                       RPE_ATTENDED_ABSENT 
# 0.0000000                                 0.0000000 
# PRE_PERC_BEING_A_ME2ME                         PRE_PERC_INPLAY_Y 
# 0.0000000                                 0.0000000 
# PRE_PERC_INPLAY_N                  PRE_PERC_IN_AND_OUT_PLAY 
# 0.0000000                                 0.0000000 
# PRE_RATE_WIN                              PRE_WIN_LOSE 
# 0.0000000                                 0.0000000 
# PRE_AVG_PROFIT_LOSS                       PRE_MAX_PROFIT_LOSS 
# 0.0000000                                 0.0000000 
# PRE_MIN_PROFIT_LOSS         PRE_PERC_ATTENDING_EXPECTED_EVENT 
# 0.0000000                                 0.0000000 
# PRE_PERC_ATTENDING_SUPRISED_EVENT                          LAST_PROFIT_LOSS 
# 0.0000000                                 0.0000000 
# LAST_TRANSACTION_COUNT                         LAST_AVG_BET_SIZE 
# 0.0000000                                 0.0000000 
# LAST_MAX_BET_SIZE                         LAST_MIN_BET_SIZE 
# 0.0000000                                 0.0000000 
# LAST_STDEV_BET_SIZE       LAST_TRANSACTION_COUNT_INPLAY_BET_Y 
# 0.0000000                                 0.0000000 
# LAST_TRANSACTION_COUNT_INPLAY_BET_N            LAST_MAX_BET_SIZE_INPLAY_BET_Y 
# 0.0000000                                 0.0000000 
# LAST_MAX_BET_SIZE_INPLAY_BET_N            LAST_MIN_BET_SIZE_INPLAY_BET_Y 
# 0.0000000                                 0.0000000 
# LAST_MIN_BET_SIZE_INPLAY_BET_N          LAST_STDEV_BET_SIZE_INPLAY_BET_Y 
# 0.0000000                                 0.0000000 
# LAST_STDEV_BET_SIZE_INPLAY_BET_N                       LAST_NO_OF_BID_TYPE 
# 0.0000000                                 0.0000000 
# LAST_NO_OF_INPLAY_BET                            LAST_TIME_DIFF 
# 0.0000000                                 0.0000000 
# LAST_TOP_1                                LAST_TOP_2 
# 0.0000000                                 0.0000000 
# LAST_TOP_5                               LAST_TOP_10 
# 0.0000000                                 0.0000000 
# LAST_TOP_15                               LAST_TOP_20 
# 0.0000000                                 0.0000000 
# LAST_TOP_25                             LAST_BOTTOM_1 
# 0.0000000                                 0.0000000 
# LAST_BOTTOM_2                             LAST_BOTTOM_5 
# 0.0000000                                 0.0000000 
# LAST_BOTTOM_10                            LAST_BOTTOM_15 
# 0.0000000                                 0.0000000 
# LAST_BOTTOM_20                            LAST_BOTTOM_25 
# 0.0000000                                 0.0000000 
# LAST_ME2ME                               LAST_ODDS_1 
# 0.0000000                                 0.0000000 
# LAST_ODDS_2                         LAST_IND_INPLAY_Y 
# 0.0000000                                 0.0000000 
# LAST_IND_INPLAY_N                   LAST_IND_IN_AND_OUT_PAY 
# 0.0000000                                 0.0000000 
# LAST_IND_RESULT_EXPECTED                  LAST_IND_RESULT_SUPRISED 
# 0.0000000                                 0.0000000 
# LAST_SCORE_DIFF                          LAST_IS_FROM_WIN 
# 0.0000000                                 0.0000000 
# LAST_IS_FROM_LOSE                      LAST_IS_FROM_NEITHER 
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
# THIS_RESULT_EXPECTED                      THIS_RESULT_SUPRISED 
# 0.0000000                                 0.0000000 
dt.3in1$THIS_STDEV_TRANSACTION_COUNT[is.na(dt.3in1$THIS_STDEV_TRANSACTION_COUNT)] <- 0
dt.3in1$THIS_STDEV_STDEV_BET_SIZE[is.na(dt.3in1$THIS_STDEV_STDEV_BET_SIZE)] <- 0
dt.3in1$THIS_STDEV_TRANSACTION_COUNT_INPLAY_BET_Y[is.na(dt.3in1$THIS_STDEV_TRANSACTION_COUNT_INPLAY_BET_Y)] <- 0
dt.3in1$THIS_STDEV_TRANSACTION_COUNT_INPLAY_BET_N[is.na(dt.3in1$THIS_STDEV_TRANSACTION_COUNT_INPLAY_BET_N)] <- 0
dt.3in1$THIS_STDEV_STDEV_BET_SIZE_INPLAY_Y[is.na(dt.3in1$THIS_STDEV_STDEV_BET_SIZE_INPLAY_Y)] <- 0
dt.3in1$THIS_STDEV_STDEV_BET_SIZE_INPLAY_N[is.na(dt.3in1$THIS_STDEV_STDEV_BET_SIZE_INPLAY_N)] <- 0
dt.3in1$THIS_STDEV_TIME_DIFF[is.na(dt.3in1$THIS_STDEV_TIME_DIFF)] <- 0

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
# THIS_AVG_STDEV_BET_SIZE                   THIS_MAX_STDEV_BET_SIZE 
# 0                                         0 
# THIS_MIN_STDEV_BET_SIZE                 THIS_STDEV_STDEV_BET_SIZE 
# 0                                         0 
# THIS_AVG_TRANSACTION_COUNT_INPLAY_Y       THIS_AVG_TRANSACTION_COUNT_INPLAY_N 
# 0                                         0 
# THIS_MAX_TRANSACTION_COUNT_INPLAY_Y       THIS_MAX_TRANSACTION_COUNT_INPLAY_N 
# 0                                         0 
# THIS_MIN_TRANSACTION_COUNT_INPLAY_Y       TBIS_MIN_TRANSACTION_COUNT_INPLAY_N 
# 0                                         0 
# THIS_STDEV_TRANSACTION_COUNT_INPLAY_BET_Y THIS_STDEV_TRANSACTION_COUNT_INPLAY_BET_N 
# 0                                         0 
# THIS_AVG_BET_SIZE_INPLAY_Y                THIS_AVG_BET_SIZE_INPLAY_N 
# 0                                         0 
# THIS_MAX_BET_SIZE_INPLAY_Y                THIS_MAX_BET_SIZE_INPLAY_N 
# 0                                         0 
# THIS_MIN_BET_SIZE_INPLAY_Y                THIS_MIN_BET_SIZE_INPLAY_N 
# 0                                         0 
# THIS_AVG_STDEV_BET_SIZE_INPLAY_Y          THIS_MIN_STDEV_BET_SIZE_INPLAY_Y 
# 0                                         0 
# THIS_MAX_STDEV_BET_SIZE_INPLAY_Y        THIS_STDEV_STDEV_BET_SIZE_INPLAY_Y 
# 0                                         0 
# THIS_AVG_STDEV_BET_SIZE_INPLAY_N          THIS_MIN_STDEV_BET_SIZE_INPLAY_N 
# 0                                         0 
# THIS_MAX_STDEV_BET_SIZE_INPLAY_N        THIS_STDEV_STDEV_BET_SIZE_INPLAY_N 
# 0                                         0 
# THIS_NO_OF_BID_TYPE                     THIS_NO_OF_INPLAY_BET 
# 0                                         0 
# THIS_AVG_TIME_DIFF                        THIS_MAX_TIME_DIFF 
# 0                                         0 
# THIS_MIN_TIME_DIFF                      THIS_STDEV_TIME_DIFF 
# 0                                         0 
# THIS_TOP_1                                THIS_TOP_2 
# 0                                         0 
# THIS_TOP_5                               THIS_TOP_10 
# 0                                         0 
# THIS_TOP_15                               THIS_TOP_20 
# 0                                         0 
# THIS_TOP_25                             THIS_BOTTOM_1 
# 0                                         0 
# THIS_BOTTOM_2                             THIS_BOTTOM_5 
# 0                                         0 
# THIS_BOTTOM_10                            THIS_BOTTOM_15 
# 0                                         0 
# THIS_BOTTOM_20                            THIS_BOTTOM_25 
# 0                                         0 
# THIS_ME2ME                             THIS_INPLAY_Y 
# 0                                         0 
# THIS_INPLAY_N                      THIS_IN_AND_OUT_PLAY 
# 0                                         0 
# THIS_IS_FROM_WIN                         THIS_IS_FROM_LOSE 
# 0                                         0 
# THIS_IS_FROM_NEITHER                       RPE_ATTENDED_ABSENT 
# 0                                         0 
# PRE_PERC_BEING_A_ME2ME                         PRE_PERC_INPLAY_Y 
# 0                                         0 
# PRE_PERC_INPLAY_N                  PRE_PERC_IN_AND_OUT_PLAY 
# 0                                         0 
# PRE_RATE_WIN                              PRE_WIN_LOSE 
# 0                                         0 
# PRE_AVG_PROFIT_LOSS                       PRE_MAX_PROFIT_LOSS 
# 0                                         0 
# PRE_MIN_PROFIT_LOSS         PRE_PERC_ATTENDING_EXPECTED_EVENT 
# 0                                         0 
# PRE_PERC_ATTENDING_SUPRISED_EVENT                          LAST_PROFIT_LOSS 
# 0                                         0 
# LAST_TRANSACTION_COUNT                         LAST_AVG_BET_SIZE 
# 0                                         0 
# LAST_MAX_BET_SIZE                         LAST_MIN_BET_SIZE 
# 0                                         0 
# LAST_STDEV_BET_SIZE       LAST_TRANSACTION_COUNT_INPLAY_BET_Y 
# 0                                         0 
# LAST_TRANSACTION_COUNT_INPLAY_BET_N            LAST_MAX_BET_SIZE_INPLAY_BET_Y 
# 0                                         0 
# LAST_MAX_BET_SIZE_INPLAY_BET_N            LAST_MIN_BET_SIZE_INPLAY_BET_Y 
# 0                                         0 
# LAST_MIN_BET_SIZE_INPLAY_BET_N          LAST_STDEV_BET_SIZE_INPLAY_BET_Y 
# 0                                         0 
# LAST_STDEV_BET_SIZE_INPLAY_BET_N                       LAST_NO_OF_BID_TYPE 
# 0                                         0 
# LAST_NO_OF_INPLAY_BET                            LAST_TIME_DIFF 
# 0                                         0 
# LAST_TOP_1                                LAST_TOP_2 
# 0                                         0 
# LAST_TOP_5                               LAST_TOP_10 
# 0                                         0 
# LAST_TOP_15                               LAST_TOP_20 
# 0                                         0 
# LAST_TOP_25                             LAST_BOTTOM_1 
# 0                                         0 
# LAST_BOTTOM_2                             LAST_BOTTOM_5 
# 0                                         0 
# LAST_BOTTOM_10                            LAST_BOTTOM_15 
# 0                                         0 
# LAST_BOTTOM_20                            LAST_BOTTOM_25 
# 0                                         0 
# LAST_ME2ME                               LAST_ODDS_1 
# 0                                         0 
# LAST_ODDS_2                         LAST_IND_INPLAY_Y 
# 0                                         0 
# LAST_IND_INPLAY_N                   LAST_IND_IN_AND_OUT_PAY 
# 0                                         0 
# LAST_IND_RESULT_EXPECTED                  LAST_IND_RESULT_SUPRISED 
# 0                                         0 
# LAST_SCORE_DIFF                          LAST_IS_FROM_WIN 
# 0                                         0 
# LAST_IS_FROM_LOSE                      LAST_IS_FROM_NEITHER 
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
# THIS_RESULT_EXPECTED                      THIS_RESULT_SUPRISED 
# 0                                         0 
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

dt.3in1[, LAST_TOP_1 := as.factor(LAST_TOP_1)]
dt.3in1[, LAST_TOP_2 := as.factor(LAST_TOP_2)]
dt.3in1[, LAST_TOP_5 := as.factor(LAST_TOP_5)]
dt.3in1[, LAST_TOP_10 := as.factor(LAST_TOP_10)]
dt.3in1[, LAST_TOP_15 := as.factor(LAST_TOP_15)]
dt.3in1[, LAST_TOP_20 := as.factor(LAST_TOP_20)]
dt.3in1[, LAST_TOP_25 := as.factor(LAST_TOP_25)]

dt.3in1[, LAST_BOTTOM_1 := as.factor(LAST_BOTTOM_1)]
dt.3in1[, LAST_BOTTOM_2 := as.factor(LAST_BOTTOM_2)]
dt.3in1[, LAST_BOTTOM_5 := as.factor(LAST_BOTTOM_5)]
dt.3in1[, LAST_BOTTOM_10 := as.factor(LAST_BOTTOM_10)]
dt.3in1[, LAST_BOTTOM_15 := as.factor(LAST_BOTTOM_15)]
dt.3in1[, LAST_BOTTOM_20 := as.factor(LAST_BOTTOM_20)]
dt.3in1[, LAST_BOTTOM_25 := as.factor(LAST_BOTTOM_25)]

dt.3in1[, LAST_ME2ME := as.factor(LAST_ME2ME)]
dt.3in1[, LAST_IND_INPLAY_Y := as.factor(LAST_IND_INPLAY_Y)]
dt.3in1[, LAST_IND_INPLAY_N := as.factor(LAST_IND_INPLAY_N)]
dt.3in1[, LAST_IND_IN_AND_OUT_PAY := as.factor(LAST_IND_IN_AND_OUT_PAY)]
dt.3in1[, LAST_IND_RESULT_EXPECTED := as.factor(LAST_IND_RESULT_EXPECTED)]
dt.3in1[, LAST_IND_RESULT_SUPRISED := as.factor(LAST_IND_RESULT_SUPRISED)]
dt.3in1[, LAST_IS_FROM_WIN := as.factor(LAST_IS_FROM_WIN)]
dt.3in1[, LAST_IS_FROM_LOSE := as.factor(LAST_IS_FROM_LOSE)]
dt.3in1[, LAST_IS_FROM_NEITHER := as.factor(LAST_IS_FROM_NEITHER)]


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
## 1.2 add prediction column #
##############################
dt.3in1[, PRED := ifelse(THIS_PROFIT_LOSS > 0, 1, 0)]
dt.3in1$PRED <- as.factor(dt.3in1$PRED)

##############################
## 1.3 save it ###############
##############################
save(dt.3in1, file = "../Datathon_Full_Dataset/transformedData.RData")
