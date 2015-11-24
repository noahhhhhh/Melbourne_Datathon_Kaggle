###########################################################################################
## inspect ################################################################################
###########################################################################################
setwd("/Volumes/Data Science/Google Drive/data_science_competition/melbourne_datathon/Melbourne_Datathon_Kaggle/")
rm(list = ls()); gc()
require(data.table)
require(dplyr)
require(caret)
require(caTools)
load("../Datathon_Full_Dataset/transformedData.RData")
# dtTestFeatures <- fread("../data_files/semi_and_final_features.csv")
dtSampleSubmit <- fread("../data_files/sample_submission_bet_size.csv")
dtSampleSubmit[, Prediction := NULL]
setnames(dtSampleSubmit, "Account_ID")

dt.scaledAll <- as.data.table(scale(dt.3in1[, !c("UNIT", "ACCOUNT_ID"
                                                 , "THIS_ME2ME", "THIS_IN_AND_OUT_PLAY", "THIS_IS_FROM_WIN"
                                                 , "THIS_IS_FROM_LOSE", "THIS_IS_FROM_NEITHER", "THIS_RESULT_EXPECTED"
                                                 , "PRED"), with = F]))

dt.scaledAll <- cbind(dt.3in1[, c("UNIT", "ACCOUNT_ID"
                                  , "THIS_ME2ME", "THIS_IN_AND_OUT_PLAY", "THIS_IS_FROM_WIN"
                                  , "THIS_IS_FROM_LOSE", "THIS_IS_FROM_NEITHER", "THIS_RESULT_EXPECTED"
                                  , "PRED"), with = F]
                      , dt.scaledAll)

str(dt.scaledAll)
#####################################################################
## 1. why previous 6 submissions suck? ##############################
#####################################################################
require(ggplot2)
require(vioplot)
g.violin <- ggplot(dt.scaledAll[UNIT == "03_04_05", with = T], aes(factor(PRED), THIS_STDEV_SCORE_DIFF))
g.violin + geom_boxplot()

dt1 <- dt.scaledAll[UNIT == "43_44_45", !c("PRED", "ACCOUNT_ID", "THIS_PROFIT_LOSS", "ACCOUNT_ID", "UNIT"), with = F]
dt2 <- dt.scaledAll[UNIT == "13_14_15", !c("PRED", "ACCOUNT_ID", "THIS_PROFIT_LOSS", "ACCOUNT_ID", "UNIT"), with = F]
# dt2 <- dt.test[, !c("ACCOUNT_ID"), with = F]
# p = dim(dt1)[2]
classList <- unlist(lapply(dt1, class))
isNotFactor <- classList != "factor"
p <- sum(isNotFactor)

dt1 <- as.data.frame(dt1[, isNotFactor, with = F])
dt2 <- as.data.frame(dt2[, isNotFactor, with = F])

col <- floor(sqrt(p))
row <- ceiling(p / col)
par(mfrow = c(row, col), mar = c(1, 1, 1, 1))

for(i in 1 : p){
    
    vioplot(dt1[, i], dt2[, i]
            , names = c("dt1", "dt2")
            , col = "salmon")
    par(font.main = 10)
    # par(ps = 7)
    title(names(dt1)[i])
}
############
## answer ##
############
# because the PRE features are misleading!

#####################################################################
## 2. what might be a good feature? #################################
#####################################################################
curPar <- par()
par(mfrow = c(7, 7))
par(mar = c(1, 1, 1, 1))

dt1 <- dt.scaledAll[UNIT == "38_39_40" & PRED == 0, !c("PRED", "ACCOUNT_ID", "THIS_PROFIT_LOSS", "ACCOUNT_ID", "UNIT"), with = F]
dt2 <- dt.scaledAll[UNIT == "38_39_40" & PRED == 1, !c("PRED", "ACCOUNT_ID", "THIS_PROFIT_LOSS", "ACCOUNT_ID", "UNIT"), with = F]
# dt2 <- dt.test[, !c("ACCOUNT_ID"), with = F]
# p = dim(dt1)[2]
classList <- unlist(lapply(dt1, class))
isNotFactor <- classList != "factor"
p <- sum(isNotFactor)

dt1 <- as.data.frame(dt1[, isNotFactor, with = F])
dt2 <- as.data.frame(dt2[, isNotFactor, with = F])

col <- floor(sqrt(p))
row <- ceiling(p / col)

for(i in 1 : p){
    
    vioplot(dt1[, i], dt2[, i]
            , names = c("0", "1")
            , col = "salmon")
    par(font.main = 7)
    # par(ps = 7)
    title(names(dt1)[i])
}

par() <- curPar
############
## answer ##
############
# to be continued!

#####################################################################
## 3. why valid set is not consistent with test set #################
#####################################################################













