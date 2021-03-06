# Melbourne_Datathon_Kaggle (We won the 1st prize!!!)
[Melbourne Datathon Kaggle Competetion](https://inclass.kaggle.com/c/melbourne-datathon-2015/)  
This competition is for those who entered the Data Science Melbourne 2015 Datathon.  

You will already have the data for all games upto the semi-finals and finals. The task is to use this historical data to rank order the punters on their profit for the final 3 games of the tournament (which is why we didn't give you this data).  

We provide the list of Account_IDs to make predictions for, along with some limited features for the final 3 games that you may make use of.  

The objective is to determine if betting is just guessing, or if past performance can be indicative of future performance. We expect this to be very hard, and will be impressed if anyone can come up with an algorithm that is better than a random number generator!  
### Lessons learnt from this competition
1. Feature engineering worths taking more time (70%), and it is the key to win the competition.
2. It would be very useful to have a few sessions of brain storming on what features are needed, how the data should be transformed.
3. The benchmark did help a lot, especially the -5 new punters one.
4. It would be best to start the competetion as early as possible, and adapt the idea, strategy, and key technics along the way.
5. Nice documentation is a great way to keep track of your idea, progress, and process.
6. A well organised workspace makes things go more efficiently.
7. Have a great team leader (like Ivan) is necessary.
8. Start the competetion individually, and later combine the work together as a team will defintely improve the score significantly (it is a better strategy than the one that enter the competetion as a team). Ivan and I both have different ideas and choices on the feature engineering and modelling. When our work is combined (bagged), the publich LB score improved from .652 to .660 (this is a huge jump).
9. Communication is always the No.1 (Ivan and I did share a lot of ideas along the way (most of the times it was me asking bunches of questions to him)).
10. Data exploration worths taking the second biggest proportion of the time.
11. Always learning!


###Todo/Memo
1. **(09/11/15) DONE -** As the features are accumulated, needs to give an EVENT_SEQ to each game based on the MATCH (EVENT_ID does not alway reflect the sequence).
2. **(10/11/15) DONE -** Remove MED_PRICE_TAKEN as the offical odds of Win/Lose/Draw of each EVENT_ID makes more sense.
3. **(10/11/15) DONE -** Add the offical odds of Win/Lose of each EVENT_ID.
4. **(10/11/15) DONE -** Add the event result, RESULT (Suprised, As Expected).
5. **(11/11/15) DONE -** add the EVENT SCORE_DIFF (AS_EXPECTEDs are +ve diff and SUPRISEDs are -ve diff).
6. **(11/11/15) DONE -** implement Feature Engineering 8 - 17.
7. **(12/11/15) DONE -** start to implement the window function (e.g. dt[, CUMSUM := cumsum(C), by = A]; dt[, LAGCUMSUM := shift(CUMSUM, fill = 0, type = "lag"), by = A]) (pay attention to B's order, this needs to be incremental).
8. **(13/11/15) DONE -** IS_FROM_WIN, IS_FROM_LOSE
9. **(13/11/15) DONE -** TIMES_BEING_A_ME2ME
10. **(13/11/15) DONE -** Transform to 3 EVENTS per UNIT.  
11. **(18/11/15) DONE -** combine the bonus feature csv into dt1.1 and transform all together, use duplicated() to check NO_OF_BID_TYPE.
12. **(18/11/15) DONE -** convert the class of each column
13. **(18/11/15) DONE -** aggregating stdev is problematic
14. **(18/11/15) DONE -** get the item 6 done!
15. **(20/11/15) DONE -** correct the train and valid set!
16. **(20/11/15) not yet done -** verify the ME2ME logic.
17. **(21/11/15) not yet done -** play around with the cv and validation set to make them consistent with the test set.
18. **(21/11/15) not yet done -** consider the other STATUS_ID.
19. **(22/11/15) DONE -** consider PRE_NO_EVENT_ATTEND + 1 for attended, - 1 for not.
20. **(22/11/15) DONE -** consider PRE_LAST ... features.
21. **(25/11/15) DONE -** time dimension
22. **(28/11/15) DONE -** try non-overlapping 3in1
23. **(29/11/15) DONE -** try non-overlapping 3in1 and randomly generate non-overlapping 3in1 (137, 245, ...)
24. **(30/11/15) DONE -** cv and ensemble with xgb regression
24. **(01/12/15) DONE -** cv and ensemble with logistic regression
25. **(09/12/15) DONE -** blended 6 models with weights (rf: .10; nnet: .30; lasso: .05; lr : .05; xbgtree: .35; xgblinear: .15), also applied (0.4367089 * 2 * original prediction) to new accts. -- AUC: 0.64596

###Feature Engineering
1. **(09/11/15) not done yet -** 2 Features indicating whether the COUNTRY_OF_RESIDENCE is the same as where either of the opponent is from would be useful. E.g. China vs Japan would have two features like **IS_CHINESE** and **IS_JAPANESE**. Also, additional similar features like  **TIMES_PREV_BET_ON_CHINA_AND_WON / LOSS** and  **TIMES_PREV_BET_ON_JAPAN_AND_WON / LOSS**, and  **TIMES_PREV_BET_ON_CHINA_GAMES** and  **TIMES_PREV_BET_ON_JAPAN_GAMES**.
  * IS_CHINESE & IS_JAPANESE
  * TIMES_PREV_BET_ON_CHINA_AND_WON / LOSS & TIMES_PREV_BET_ON_JAPAN_AND_WON / LOSS
  * TIMES_PREV_BET_ON_CHINA_GAMES & TIMES_PREV_BET_ON_JAPAN_AND_WON
2. **(09/11/15) DONE -** Since ME2ME is a good indicator, then **TIMES_BEING_A_ME2ME** and **IS_ME2ME** would be useful.
  * TIMES_BEING_A_ME2ME
  * IS_ME2ME
3. **(09/11/15) not done yet -** **TIMES_BEING_EARLY_5%** and **TIMES_BEING_EARLY_10%** would be useful.
  * TIMES_BEING_EARLY_5
  * TIMES_BEING_EARLY_10
4. **(09/11/15) DONE -** add the PRICE_TAKEN into the test set (as the previously mentioned, PRICE_TAKEN should be very close to the official odds) **(SHOULD BE REMOVED AS NOT REASONABLE)**.
5. **(10/11/15) DONE -** Since we are predicting P/L and PRICE_TAKEN is said to be close to the official Odds, then add **ODDS_WIN**, **ODDS_LOSE** must be useful.
  * ODDS_WIN
  * ODDS_LOSE
6. **(10/11/15) DONE -** is the **RESULT** of EVENT useful? (SUPRISED and AS_EXPECTED)
  * RESULT
7. **(11/11/15) DONE -** how about adding the EVENT SCORE_DIFF? (AS_EXPECTEDs are +ve diff and SUPRISEDs are -ve diff)
  * SCORE_DIFF
8. **(11/11/15)DONE -** **WIN_RATE**.
9. **(11/11/15) not done yet -** **WIN_LAST_ATTENDED_EVENT**.
10. **(11/11/15) DONE -** **NO_OF_EVENT_ATTENDED**
11. **(11/11/15) DONE -** **NO_OF_EVENT_WON**
12. **(11/11/15) DONE -** **NO_OF_EVENT_LOSE**
13. **(11/11/15) DONE -** **TTL_PROFIT_LOSS**
14. **(11/11/15) DONE -** **MAX_PROFIT_LOSS_PER_EVENT**
15. **(11/11/15) DONE -** **MIN_PROFIT_LOSS_PER_EVENT**
16. **(11/11/15) DONE -** **AVG_PROFIT_LOSS_PER_EVENT**
17. **(11/11/15) DONE -** **AVG_PROFIT_LOSS_LAST_ATTENDED_EVENT**
18. **(12/11/15) DONE -** Phil's **WIN_LOSE** (WIN: +1; LOSE: -1)

###Initial Thoughts
1. **(07/11/15)** Duplicates exist in the bonus features csv due to a mistake. BID_TYP can actually be inferred: B followed by L. This could potentially let us play a trick: we know the results of these 3 games, so with B and L known for some records, we could know if they won or lost (setting them to the maximum or minimum rank). Wait ... WE DONT KNOW SELECTION_NAME.  
2. **(07/11/15)** AUC can take ranks (any values) and use them to compare with 1s and 0s, ranks can be thought of as probabilities (e.g. results of logistic regression and LDA), profit-loss amount (this needs to be scaled by the bet size and odss I think), winrate (also needs to be scaled by no. of games played), or no. of games won (ref. Phil).  
3. **(08/11/15)** 13,087 distinct Settled Account IDs in Semi and Final Bonus Features.csv, 3,169 Account IDs are new punters, 7,374 distinct Account IDs in sample submit (a subset of 13,087). 
4. **(08/11/15)** We can see from the bonus features that, B and L are hidden by got grouped by, so duplicates were generated. How to utilise this info? without knowing SELECTION_NAME, we cannot directly know who won or lost. But, ME2ME are more likely to win .55 win rate vs .5 and statistically significant. We may predict this kind of user to win!
5. **(09/11/15)** As the prediction is based on Events, so setting training set to be the prior 40 games from file 1-4, and the later 4 games from file qtr final to be the validating set is a reasonable choice.
6. **(09/11/15)** Thanks to Ivan in confirming the below scenarios: I won in the 1st game, lost in the 2nd, and the result of 3rd is unknown. If there is a WINRATE feature, and if I want to use the 1st game to predict the 2nd, then WINRATE = 1; If I want to use 1st and 2nd game to predict 3rd, then WINRATE = .5.
7. **(10/11/15)** Adding PRICE_TAKEN to the bonus feature is a good move, but, not sufficient as you do not know "B" and "L". 3 types of PRICE_TAKEN are there, for Win, for Lose, and for Draw. They all share the parameter, probability of win/lose. It is said they are close to the official odds, so we can add the official odds of Win/Lose/Draw into each game and hide the "B" and "L". Good idea! then do you need MED_PRICE_TAKEN now? no, as we cannot have it in the bonus feature.
8. **(10/11/15)** Use the similar EVENT as testing set when validating model would be useful, We can compare the odds to find similar EVENTS.
9. **(11/11/15)** Bag and Boosting could be used to stablise the outcome of the model.
10. **(11/11/15)** The Lasso to shrink predictors --> SVM --> Bag and Boosting.
11. **(12/11/15)** **3 EVENTS TO COMBINE INTO 1 EVENT TO TRAIN AND TEST!**
12. **(21/11/15)** why the previous 6 submissions suck? because the **PRE features** are misleading. Suggest to change them to **PRE_IND**, **PRE_AVG**, etc. Also, **LAST_EVENT** features would be considered, such **LAST_EVENT_WON** (1, 0), **LAST_EVENT_PROFIT**, **LAST_EVENT_ME2ME**, etc.

###Questions
1. **(07/11/15)** EVENT_ID relfects the event sequence?
  * **(07/11/15) confirmed -** Answer: no, but most of the times they do.
2. **(08/11/15)** How to split train and test sets in this kind of event baesd dataset?  
  * **(09/11/15) confirmed -** Answer: previous 40 as the train set, later 4 as the test set, and sample the users to do cross-validation.
3. **(08/11/15)** ME2ME more likely to win?  
  * **(08/11/15) confirmed -** Answer: yes, .55 win rate vs .5 and statistically significant.
4. **(09/11/15)** Are early betors more likely to win?
5. **(10/11/15)** How to use STATUS_ID "C", "V", etc. in the training of the model?
  * **(10/11/15) confirmed -** According to Ivan, we can move info about "C", "V" to the same row corresponding to ACCOUNT_ID and EVENT_ID. Genius!
6. **(10/11/15)** Can we assume "B" and "L" in the bonus feature?: Setting Australian Accounts "B", Kiwi Accounts "L", but this action is still subjected to the SELECTION_NAME, meaning we need to assume more scenarios? e.g. Australian Accounts "B" and SELECTION_NAME "Australia", Australian Accoutns "L" and SELECTION_NAME "New Zealand"? It is a bit complicated, but worthy of trying?
7. **(10/11/15)** Is the result of EVENT useful? 
  * **(10/11/15) confirmed -** Maybe, but how to represent this feature? we cannot use SELECTION_NAME so something like "AUS_WON?" is not useful. But!!! we have the offical odds, so we can have something like RESULT = "Supprised", "As Expected". For example, if ODDS_WIN is 1.5, and the team won, then it is "As Expected", 1 is a good threshold of deciding the value of RESULT.
8. **(10/11/15)** Use the similar EVENT as testing set when validating model would be useful?
  * **(10/11/15) confirmed -** We can compare the odds to find similar EVENTS
9. **(10/11/15)** Do Austrlian prefer to bet on Australia win? If so, does it apply to other countries?
