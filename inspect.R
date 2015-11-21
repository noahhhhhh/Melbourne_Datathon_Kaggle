###########################################################################################
## inspect ################################################################################
###########################################################################################
#####################################################################
## 1. why previous 6 submissions suck? ##############################
#####################################################################
require(ggplot2)
require(vioplot)
g.violin <- ggplot(dt.train, aes(factor(PRED), THIS_ME2ME))
g.violin + geom_violin()

dt1 <- dt.3in1[UNIT == "38_39_40", !c("PRED", "ACCOUNT_ID", "THIS_PROFIT_LOSS", "ACCOUNT_ID", "UNIT"), with = F]
dt2 <- dt.3in1[UNIT == "03_04_05", !c("PRED", "ACCOUNT_ID", "THIS_PROFIT_LOSS", "ACCOUNT_ID", "UNIT"), with = F]
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