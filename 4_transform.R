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
dt.3 <- dt1.1[, c("EVENT_SEQ", "EVENT_ID", "ACCOUNT_ID", "PROFIT_LOSS", "TRANSACTION_COUNT", "AVG_BET_SIZE"
                  , "MAX_BET_SIZE", "MIN_BET_SIZE", "STDEV_BET_SIZE", "TRANSACTION_COUNT_INPLAY_BET_Y"
                  , "TRANSACTION_COUNT_INPLAY_BET_N", "AVG_BET_SIZE_INPLAY_BET_Y", "AVG_BET_SIZE_INPLAY_BET_N"
                  , "MAX_BET_SIZE_INPLAY_BET_Y", "MAX_BET_SIZE_INPLAY_BET_N", "MIN_BET_SIZE_INPLAY_BET_Y"
                  , "MIN_BET_SIZE_INPLAY_BET_N", "STDEV_BET_SIZE_INPLAY_BET_Y", "STDEV_BET_SIZE_INPLAY_BET_N"
                  , "ME2ME", "TIMES_BEING_A_ME2ME", "IND_IN_AND_OUT_PAY", "TIMES_IN_AND_OUT_PLAY", "TIMES_INPLAY_Y", "TIMES_INPLAY_N"
                  , "ODDS_1", "ODDS_2", "SCORE_DIFF", "NO_OF_EVENT_ATTENDED", "NO_OF_WIN", "NO_OF_LOSE"
                  , "RATE_WIN", "WIN_LOSE", "TTL_PROFIT_LOSS", "AVG_PROFIT_LOSS", "MAX_PROFIT_LOSS", "MIN_PROFIT_LOSS"
                  , "IS_FROM_WIN", "IS_FROM_LOSE", "IS_FROM_NEITHER"
                  , "TIMES_ATTENDING_EXPECTED_EVENT", "TIMES_ATTENDING_SUPRISED_EVENT"
                  , "IND_RESULT_EXPECTED"), with = F]

dim(dt.3)
# [1] 174226     41

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
                , THIS_STDEV_BET_SIZE = mean(STDEV_BET_SIZE) / sqrt(sum(TRANSACTION_COUNT))
                
                # THIS TRANSACTION_COUNT INPLAY
                , THIS_AVG_TRANSACTION_COUNT_INPLAY_Y = sum(TRANSACTION_COUNT_INPLAY_BET_Y) / 3
                , THIS_AVG_TRANSACTION_COUNT_INPLAY_N = sum(TRANSACTION_COUNT_INPLAY_BET_N) / 3
                , THIS_MAX_TRANSACTION_COUNT_INPLAY_Y = max(TRANSACTION_COUNT_INPLAY_BET_Y)
                , THIS_MAX_TRANSACTION_COUNT_INPLAY_N = max(TRANSACTION_COUNT_INPLAY_BET_N)
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
                , THIS_STDEV_BET_SIZE_INPLAY_Y = mean(STDEV_BET_SIZE_INPLAY_BET_Y) / sqrt(sum(TRANSACTION_COUNT_INPLAY_BET_Y))
                , THIS_STDEV_BET_SIZE_INPLAY_N = mean(STDEV_BET_SIZE_INPLAY_BET_N) / sqrt(sum(TRANSACTION_COUNT_INPLAY_BET_N))
                
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
                
                # PRE TTL_PROFIT_LOSS
                , PRE_TTL_PROFIT_LOSS = sum(TTL_PROFIT_LOSS[RANK == 1])
                
                # PRE AVG_PROFIT_LOSS
                , PRE_AVG_PROFIT_LOSS = sum(AVG_PROFIT_LOSS[RANK == 1])
                
                # PRE MAX_PROFIT_LOSS
                , PRE_MAX_PROFIT_LOSS = sum(MAX_PROFIT_LOSS[RANK == 1])
                
                # PRE MIN_PROFIT_LOSS
                , PRE_MIN_PROFIT_LOSS = sum(MIN_PROFIT_LOSS[RANK == 1])
                
                # PRE PRE TIMES_ATTENDING_EXPECTED_EVENT
                , PRE_TIMES_ATTENDING_EXPECTED_EVENT = sum(TIMES_ATTENDING_EXPECTED_EVENT[RANK == 1])
                
                # PRE PRE TIMES_ATTENDING_SUPRISED_EVENT
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

# inf, -inf
# before
sum(is.finite(dt.3in1$THIS_STDEV_BET_SIZE_INPLAY_N))
dt.3in1$THIS_STDEV_BET_SIZE_INPLAY_N[dt.3in1$THIS_STDEV_BET_SIZE_INPLAY_N %in% c(Inf, -Inf)] <- 0

##############################
## 1.2 add prediction column #
##############################
dt.3in1[, PRED := ifelse(THIS_PROFIT_LOSS > 0, 1, 0)]
dt.3in1$PRED <- as.factor(dt.3in1$PRED)

##############################
## 1.3 save it ###############
##############################
save(dt.3in1, file = "../Datathon_Full_Dataset/transformedData.RData")
