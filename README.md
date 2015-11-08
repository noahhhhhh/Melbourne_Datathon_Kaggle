# Melbourne_Datathon_Kaggle
[Melbourne Datathon Kaggle Competetion](https://inclass.kaggle.com/c/melbourne-datathon-2015/)  
This competition is for those who entered the Data Science Melbourne 2015 Datathon.  

You will already have the data for all games upto the semi-finals and finals. The task is to use this historical data to rank order the punters on their profit for the final 3 games of the tournament (which is why we didn't give you this data).  

We provide the list of Account_IDs to make predictions for, along with some limited features for the final 3 games that you may make use of.  

The objective is to determine if betting is just guessing, or if past performance can be indicative of future performance. We expect this to be very hard, and will be impressed if anyone can come up with an algorithm that is better than a random number generator!  

###Thoughts
1. Duplicates exist in the bonus features csv due to a mistake. BID_TYP can actually be inferred: B followed by L. This could potentially let us play a trick: we know the results of these 3 games, so with B and L known for some records, we could know if they won or lost (setting them to the maximum or minimum rank). Wait ... WE DONT KNOW SELECTION_NAME.  
2. AUC can take ranks (any values) and use them to compare with 1s and 0s, ranks can be thought of as probabilities (e.g. results of logistic regression and LDA), profit-loss amount (this needs to be scaled by the bet size and odss I think), winrate (also needs to be scaled by no. of games played), or no. of games won (ref. Phil).  
3. 13,087 distinct Settled Account IDs in Semi and Final Bonus Features.csv, 3,169 Account IDs are new punters, 7,374 distinct Account IDs in sample submit (a subset of 13,087). 
4. As the prediction is based on Events, so setting training set to be the prior 40 games from file 1-4, and the later 4 games from file qtr final to be the validating set is a reasonable choice.
5. We can see from the bonus features that, B and L are hidden by got grouped by, so duplicates were generated. How to utilise this info? without knowing SELECTION_NAME, we cannot directly know who won or lost. However, are this kind of ME2ME more likely to win? **(yes, confirmed in 1_expore.R)** If so, we may predict this kind of user to win!