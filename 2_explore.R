###########################################################################################
## Explore and Understand #################################################################
###########################################################################################
setwd("/Volumes/Data Science/Google Drive/data_science_competition/melbourne_datathon/Melbourne_Datathon_Kaggle/")
rm(list = ls()); gc()
load("../Datathon_Full_Dataset/processedData.RData")
require(data.table)
#######################################
## 1.0 assumption #####################
#######################################
# 1.1 if a team wins, and a bettor chooses B the team or L the opponent team, then he/she wins?
sort(unique(dt$MATCH))
# [1] "Afghanistan v Scotland"              "Australia v Afghanistan"            
# [3] "Australia v Bangladesh"              "Australia v New Zealand"            
# [5] "Australia v Pakistan"                "Australia v Scotland"               
# [7] "Australia v Sri Lanka"               "Bangladesh v Afghanistan"           
# [9] "Bangladesh v New Zealand"            "Bangladesh v Scotland"              
# [11] "England v Afghanistan"               "England v Bangladesh"               
# [13] "England v New Zealand"               "England v Scotland"                 
# [15] "England v Sri Lanka"                 "India v Bangladesh"                 
# [17] "India v Ireland"                     "India v Pakistan"                   
# [19] "India v United Arab Emirates"        "India v West Indies"                
# [21] "India v Zimbabwe"                    "Ireland v United Arab Emirates"     
# [23] "Ireland v West Indies"               "New Zealand v Afghanistan"          
# [25] "New Zealand v Scotland"              "New Zealand v West Indies"          
# [27] "Pakistan v Ireland"                  "Pakistan v United Arab Emirates"    
# [29] "Pakistan v West Indies"              "Pakistan v Zimbabwe"                
# [31] "South Africa v India"                "South Africa v Ireland"             
# [33] "South Africa v Pakistan"             "South Africa v United Arab Emirates"
# [35] "South Africa v West Indies"          "South Africa v Zimbabwe"            
# [37] "Sri Lanka v Afghanistan"             "Sri Lanka v Bangladesh"             
# [39] "Sri Lanka v Scotland"                "Sri Lanka v South Africa"           
# [41] "West Indies v United Arab Emirates"  "West Indies v Zimbabwe"             
# [43] "Zimbabwe v Ireland"                  "Zimbabwe v United Arab Emirates"  

# we know Australia beat Scotland in 14/03/15
dt[which(dt$MATCH == "Australia v Scotland" & dt$SELECTION_NAME == "Australia" & dt$PROFIT_LOSS > 0 & dt$STATUS_ID == "S"), BID_TYP]
# assumption is right
# THEREFORE, SIMPLY KNOWING B AND L IN THE TESTING SET IS NOT ENOUGH

# 1.2 EVENT_ID is incremental
sort(unique(dt$EVENT_DT))
# [1] "2015-02-15 01:00:00 UTC" "2015-02-15 03:30:00 UTC" "2015-02-15 10:00:00 UTC" "2015-02-16 10:00:00 UTC"
# [5] "2015-02-18 03:30:00 UTC" "2015-02-18 10:00:00 UTC" "2015-02-20 01:00:00 UTC" "2015-02-20 10:00:00 UTC"
# [9] "2015-02-21 03:30:00 UTC" "2015-02-21 10:00:00 UTC" "2015-02-22 03:30:00 UTC" "2015-02-22 10:00:00 UTC"
# [13] "2015-02-24 03:30:00 UTC" "2015-02-25 03:30:00 UTC" "2015-02-25 10:00:00 UTC" "2015-02-26 03:30:00 UTC"
# [17] "2015-02-27 03:30:00 UTC" "2015-02-28 01:00:00 UTC" "2015-02-28 06:30:00 UTC" "2015-02-28 10:00:00 UTC"
# [21] "2015-03-01 03:30:00 UTC" "2015-03-03 03:30:00 UTC" "2015-03-04 01:00:00 UTC" "2015-03-04 06:30:00 UTC"
# [25] "2015-03-04 10:00:00 UTC" "2015-03-06 06:30:00 UTC" "2015-03-07 01:00:00 UTC" "2015-03-07 03:30:00 UTC"
# [29] "2015-03-07 10:00:00 UTC" "2015-03-08 03:30:00 UTC" "2015-03-09 03:30:00 UTC" "2015-03-10 01:00:00 UTC"
# [33] "2015-03-11 03:30:00 UTC" "2015-03-12 01:00:00 UTC" "2015-03-13 01:00:00 UTC" "2015-03-13 03:30:00 UTC"
# [37] "2015-03-14 01:00:00 UTC" "2015-03-14 03:30:00 UTC" "2015-03-14 10:00:00 UTC" "2015-03-15 03:30:00 UTC"
# [41] "2015-03-18 03:30:00 UTC" "2015-03-19 03:30:00 UTC" "2015-03-20 03:30:00 UTC" "2015-03-21 01:00:00 UTC"

sort(unique(dt$EVENT_ID))
# [1] "101093076" "101093194" "101093312" "101128269" "101128387" "101149398" "101149516" "101149634" "101149752"
# [10] "101149870" "101149988" "101150106" "101150224" "101150348" "101150480" "101150598" "101150716" "101150834"
# [19] "101150952" "101151090" "101151214" "101151342" "101151486" "101151606" "101151748" "101151878" "101152014"
# [28] "101152132" "101152275" "101152395" "101152576" "101152715" "101152836" "101152954" "101153072" "101153190"
# [37] "101153308" "101153426" "101153544" "101153662" "101183237" "101183757" "101183885" "101184013"

dt[which(as.character(dt$EVENT_DT) == "2015-02-18 03:30:00"), EVENT_ID]
# 101093194, no, not the smallest EVENT_ID, but the second

dt[which(dt$EVENT_ID == "101093076"), EVENT_DT]
# 2015-02-15 03:30:00 UTC, not the smallest EVENT_DT, but the second
# maybe on the same date, the EVENT_ID is a bit chaos, how about different date?
dt[which(as.character(dt$EVENT_DT) == "2015-03-21 01:00:00"), EVENT_ID]
# 101184013, yes, the biggest
dt[which(as.character(dt$EVENT_DT) == "2015-03-20 03:30:00"), EVENT_ID]
# 101183885, yes, the second biggest 
dt[which(as.character(dt$EVENT_DT) == "2015-03-19 03:30:00"), EVENT_ID]
# 101183237, no, no the the third biggest, but the fourth

# assumption is right for most of the times
# THEREFORE, WE CAN ASSUME (COULD BE WRONG, BUT NO HARM TO TRY) THAT THE BIGGEST EVENT_ID IS THE FINAL IN TEST SET
sort(unique(dt$OFF_DT))
#######################################
## 2.0 play around ####################
#######################################
# 2.1 ACCOUNT_ID
length(unique(dt$ACCOUNT_ID))
# 21020

# 2.2 MATCH/EVENT_ID
length(unique(dt$EVENT_ID)); length(unique(dt$MATCH))
# 44
# 44

#######################################
## 3.0 inspect test set & sample submit
#######################################
# 3.1 read
test <- fread("../data_files/semi_and_final_features.csv")
sample_submit <- fread("../data_files/sample_submission_bet_size.csv")

# 3.2 set names
setnames(test, names(test), c("ACCOUNT_ID", names(test)[-1]))
setnames(sample_submit, names(sample_submit), c("ACCOUNT_ID", names(sample_submit)[-1]))

# 3.2 explore
# 3.2.1 NAs
apply(test, 2, function(x) mean(is.na(x)))
# Account_ID          EVENT_ID TRANSACTION_COUNT         STATUS_ID        INPLAY_BET      AVG_BET_SIZE 
# 0                 0                 0                 0                 0                 0 
# MAX_BET_SIZE      MIN_BET_SIZE    STDEV_BET_SIZE 
# 0                 0                 0 
apply(sample_submit, 2, function(x) mean(is.na(x)))
# Account_ID Prediction 
# 0          0 

# 3.2.2 no. of ACCOUNT_ID
length(unique(test[test$STATUS_ID == "S"]$ACCOUNT_ID))
# 13087
length(unique(sample_submit$ACCOUNT_ID))
# 7374

# 3.2.3 no. of ACCOUNT_ID not in dt
ACCOUNT_ID_NEW <- setdiff(unique(test$ACCOUNT_ID), unique(dt$ACCOUNT_ID))
length(ACCOUNT_ID_NEW)
# 3169










