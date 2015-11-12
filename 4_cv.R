dt.cv <- dt1.1[, !c("EVENT_ID", "ACCOUNT_ID", "COUNTRY_OF_RESIDENCE_NAME"
                    , "MATCH", "NO_OF_BID_TYPE", "NO_OF_INPLAY_BET"
                    , "CUM_ATTENDED", "CUM_WIN", "CUM_LOSE", "CUM_PROFIT_LOSS"
                    , "ATTENDED", "IND_WIN", "IND_LOSE"), with = F]
dt.cv$INPLAY_BET <- as.factor(dt.cv$INPLAY_BET)
dt.cv$ME2ME <- as.factor(dt.cv$ME2ME)
dt.cv$RESULT <- as.factor(dt.cv$RESULT)
str(dt.cv)

dt.train <- dt.cv[EVENT_SEQ %in% 1:39, with = T]
dt.test <- dt.cv[EVENT_SEQ %in% 40:43, with = T]

dt.train <- dt.train[, !c("EVENT_SEQ"), with = F]
dt.test <- dt.test[, !c("EVENT_SEQ"), with = F]

dt.train.scale <- scale(dt.train[, !c("PROFIT_LOSS", "INPLAY_BET", "ME2ME", "RESULT"), with = F])
dt.test.scale <- scale(dt.test[, !c("PROFIT_LOSS", "INPLAY_BET", "ME2ME", "RESULT"), with = F])

dt.train.scale <- cbind(dt.train.scale, dt.train[, c("PROFIT_LOSS", "INPLAY_BET", "ME2ME", "RESULT"), with = F])
dt.test.scale <- cbind(dt.test.scale, dt.test[, c("INPLAY_BET", "ME2ME", "RESULT"), with = F])

fit <- lm(PROFIT_LOSS ~ ., data = dt.train.scale)
cv <- cv.lm(data = dt.train.scale, fit, m = 10)
predictions <- predict(fit, newdata = dt.test.scale)
class.test <- ifelse(dt.test$PROFIT_LOSS > 0, 1, 0)
train_predictions <- predict(fit, newdata = dt.train.scale)
class.train <- ifelse(dt.train$PROFIT_LOSS > 0, 1, 0)

dtTest <- data.table(ACCOUNT_ID = dt1.1[EVENT_SEQ %in% 40:43, with = T]$ACCOUNT_ID, PRED = predictions)
dtTest <- dtTest %>%
    group_by(ACCOUNT_ID) %>%
    summarise(PRED = sum(PRED))

dtConfirm <- dt1.1[EVENT_SEQ %in% 40:43, with = T] %>%
                       group_by(ACCOUNT_ID) %>%
                       summarise(PROFIT_LOSS = sum(PROFIT_LOSS))

class <- ifelse(dtConfirm$PROFIT_LOSS > 0, 1, 0)

require(caTools)
colAUC(predictions, class.test, plotROC = T)

