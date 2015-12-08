rm(list = ls())
gc()
load("trainEnsemble.RData")
load("valid1Ensemble.RData")
load("valid2Ensemble.RData")
load("testEnsemble.RData")

dt.test.ensemble[, rf:= 1 - rf]

# try simple blending with rf[, 2]
pred.bagging <- rowSums(dt.test.ensemble)

dt.submit <- data.table(Account_ID = dt.test$ACCOUNT_ID, Prediction = pred.bagging)
dim(dt.submit)
# [1] 12935     2
dt.submit <- merge(dtSampleSubmit, dt.submit, by = "Account_ID", all.x = T, sort = F)
# [1] 7374    2

# set the scores of new accts to .437 * 2 * original prediction
trainAcct <- unique(dt.train$ACCOUNT_ID)
valid1Acct <- unique(dt.valid1$ACCOUNT_ID)
valid2Acct <- unique(dt.valid2$ACCOUNT_ID)

existingAcct <- c(trainAcct, valid1Acct, valid2Acct)
existingAcct <- unique(existingAcct)
length(existingAcct)
# [1] 20841

newAcct <- setdiff(dt.submit$Account_ID, existingAcct)

# Prediction_New <- rnorm(1001, mean = .43, sd = .1)
Prediction_New <- .437
dt.newAcct <- data.table(Account_ID = newAcct, Prediction_New = Prediction_New)
# bestSub[, Prediction := Prediction/14]

newSub <- merge(dt.submit, dt.newAcct, by = "Account_ID", all.x = T)
newSub[, Prediction_New := Prediction * 2 * Prediction_New]
newSub[, Prediction := ifelse(is.na(newSub$Prediction_New), newSub$Prediction, newSub$Prediction_New)]
newSub[, Prediction_New := NULL]
newSub[Prediction == .437, with = T]
