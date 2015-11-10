# Melbourne_Datathon_Kaggle
[Melbourne Datathon Kaggle Competetion](https://inclass.kaggle.com/c/melbourne-datathon-2015/)  
This competition is for those who entered the Data Science Melbourne 2015 Datathon.  

You will already have the data for all games upto the semi-finals and finals. The task is to use this historical data to rank order the punters on their profit for the final 3 games of the tournament (which is why we didn't give you this data).  

We provide the list of Account_IDs to make predictions for, along with some limited features for the final 3 games that you may make use of.  

The objective is to determine if betting is just guessing, or if past performance can be indicative of future performance. We expect this to be very hard, and will be impressed if anyone can come up with an algorithm that is better than a random number generator!  

###Todo/Memo
1. **(09/11/15) DONE -** As the features are accumulated, needs to give an EVENT_SEQ to each game based on the MATCH (EVENT_ID does not alway reflect the sequence).

###Feature Engineering
1. **(09/11/15) not done yet -** 2 Features indicating whether the COUNTRY_OF_RESIDENCE is the same as where either of the opponent is from would be useful. E.g. China vs Japan would have two features like **IS_CHINESE** and **IS_JAPANESE**. Also, additional similar features like  **TIMES_PREV_BET_ON_CHINA_AND_WON / LOSS** and  **TIMES_PREV_BET_ON_JAPAN_AND_WON / LOSS**, and  **TIMES_PREV_BET_ON_CHINA_GAMES** and  **TIMES_PREV_BET_ON_JAPAN_GAMES**.
  * IS_CHINESE & IS_JAPANESE
  * TIMES_PREV_BET_ON_CHINA_AND_WON / LOSS & TIMES_PREV_BET_ON_JAPAN_AND_WON / LOSS
  * TIMES_PREV_BET_ON_CHINA_GAMES & TIMES_PREV_BET_ON_JAPAN_AND_WON
2. **(09/11/15) not done yet -** Since ME2ME is a good indicator, then **TIMES_BEING_A_ME2ME** and **IS_ME2ME** would be useful.
  * TIMES_BEING_A_ME2ME
  * IS_ME2ME
3. **(09/11/15) not done yet -** **TIMES_BEING_EARLY_5%** and **TIMES_BEING_EARLY_10%** would be useful.
  * TIMES_BEING_EARLY_5
  * TIMES_BEING_EARLY_10
4. **(09/11/15) DONE -** add the PRICE_TAKEN into the test set (as the previously mentioned, PRICE_TAKEN should be very close to the official odds)

###Initial Thoughts
1. **(07/11/15)** Duplicates exist in the bonus features csv due to a mistake. BID_TYP can actually be inferred: B followed by L. This could potentially let us play a trick: we know the results of these 3 games, so with B and L known for some records, we could know if they won or lost (setting them to the maximum or minimum rank). Wait ... WE DONT KNOW SELECTION_NAME.  
2. **(07/11/15)** AUC can take ranks (any values) and use them to compare with 1s and 0s, ranks can be thought of as probabilities (e.g. results of logistic regression and LDA), profit-loss amount (this needs to be scaled by the bet size and odss I think), winrate (also needs to be scaled by no. of games played), or no. of games won (ref. Phil).  
3. **(08/11/15)** 13,087 distinct Settled Account IDs in Semi and Final Bonus Features.csv, 3,169 Account IDs are new punters, 7,374 distinct Account IDs in sample submit (a subset of 13,087). 
4. **(08/11/15)** We can see from the bonus features that, B and L are hidden by got grouped by, so duplicates were generated. How to utilise this info? without knowing SELECTION_NAME, we cannot directly know who won or lost. But, ME2ME are more likely to win .55 win rate vs .5 and statistically significant. We may predict this kind of user to win!
5. **(09/11/15)** As the prediction is based on Events, so setting training set to be the prior 40 games from file 1-4, and the later 4 games from file qtr final to be the validating set is a reasonable choice.
6. **(09/11/15)** Thanks to Ivan in confirming the below scenarios: I won in the 1st game, lost in the 2nd, and the result of 3rd is unknown. If there is a WINRATE feature, and if I want to use the 1st game to predict the 2nd, then WINRATE = 1; If I want to use 1st and 2nd game to predict 3rd, then WINRATE = .5.

###Questions
1. **(07/11/15)** EVENT_ID relfects the event sequence?
  * **(07/11/15) confirmed -** Answer: no, but most of the times they do.
2. **(08/11/15)** How to split train and test sets in this kind of event baesd dataset?  
  * **(09/11/15) confirmed -** Answer: previous 40 as the train set, later 4 as the test set, and sample the users to do cross-validation.
3. **(08/11/15)** ME2ME more likely to win?  
  * **(08/11/15) confirmed -** Answer: yes, .55 win rate vs .5 and statistically significant.
4. **(09/11/15)** Are early betors more likely to win?
5. **(10/11/15)** How to use STATUS_ID "C", "V", etc. in the training of the model?
6. **(10/11/15)** Can we assume "B" and "L" in the bonus feature?: Setting Australian Accounts "B", Kiwi Accounts "L", but this action is still subjected to the SELECTION_NAME, meaning we need to assume more scenarios? e.g. Australian Accounts "B" and SELECTION_NAME "Australia", Australian Accoutns "L" and SELECTION_NAME "New Zealand"? It is a bit complicated, but worthy of trying?
