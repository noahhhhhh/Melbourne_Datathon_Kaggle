###########################################################################################
## Data cleasing ##########################################################################
###########################################################################################
setwd("/Volumes/Data Science/Google Drive/data_science_competition/melbourne_datathon/Melbourne_Datathon_Kaggle/")
rm(list = ls()); gc()
require(data.table)
require(bit64)
require(plyr)
require(lubridate)
#######################################
## 1.0 read ###########################
#######################################
# 1.1 inspect
system("ls ../Datathon_Full_Dataset/")
system("head ../Datathon_Full_Dataset/*.csv")

# 1.2 read and set class to columns
colClasses <- c(rep("interger",4),"character",rep("interger",2),rep("character",12),rep("numeric",2),"character",rep("numeric",2))
dt_1 <- fread("../Datathon_Full_Dataset/Datathon WC Data Games 1-10.csv", colClasses=colClasses, na.strings = c("NA","N/A",""))
dt_2 <- fread("../Datathon_Full_Dataset/Datathon WC Data Games 11-20.csv", colClasses=colClasses, na.strings = c("NA","N/A",""))
dt_3 <- fread("../Datathon_Full_Dataset/Datathon WC Data Games 21-30.csv", colClasses=colClasses, na.strings = c("NA","N/A",""))
dt_4 <- fread("../Datathon_Full_Dataset/Datathon WC Data Games 31-40.csv", colClasses=colClasses, na.strings = c("NA","N/A",""))
dt_5 <- fread("../Datathon_Full_Dataset/Datathon WC Data Games QTR Finals.csv",colClasses=colClasses, na.strings = c("NA","N/A",""))

#######################################
## 2.0 preprocess #####################
#######################################
# 2.1 rename columns
setnames(dt_1, names(dt_1), c("BET_ID", names(dt_1)[-1]))
setnames(dt_2, names(dt_2), c("BET_ID", names(dt_2)[-1]))
setnames(dt_3, names(dt_3), c("BET_ID", names(dt_3)[-1]))
setnames(dt_4, names(dt_4), c("BET_ID", names(dt_4)[-1]))
setnames(dt_5, names(dt_5), c("BET_ID", names(dt_5)[-1]))

# 2.2 fix PROFIT_LOSS
dt_1[,c("PROFIT_LOSS","TABLE_NUM") := list(ifelse(as.numeric(PROFIT_LOSS) > 0, ifelse(gsub(" *$", "", BID_TYP) == "B", (as.numeric(PRICE_TAKEN)-1)*as.numeric(BET_SIZE), as.numeric(BET_SIZE)),0) + 
                                               ifelse(as.numeric(PROFIT_LOSS) < 0, ifelse(gsub(" *$", "", BID_TYP) == "L", (as.numeric(PRICE_TAKEN)-1)*-as.numeric(BET_SIZE), -as.numeric(BET_SIZE)),0), 1)]
dt_2[,c("PROFIT_LOSS","TABLE_NUM") := list(ifelse(as.numeric(PROFIT_LOSS) > 0, ifelse(gsub(" *$", "", BID_TYP) == "B", (as.numeric(PRICE_TAKEN)-1)*as.numeric(BET_SIZE), as.numeric(BET_SIZE)),0) + 
                                               ifelse(as.numeric(PROFIT_LOSS) < 0, ifelse(gsub(" *$", "", BID_TYP) == "L", (as.numeric(PRICE_TAKEN)-1)*-as.numeric(BET_SIZE), -as.numeric(BET_SIZE)),0), 2)]
dt_3[,c("PROFIT_LOSS","TABLE_NUM") := list(ifelse(as.numeric(PROFIT_LOSS) > 0, ifelse(gsub(" *$", "", BID_TYP) == "B", (as.numeric(PRICE_TAKEN)-1)*as.numeric(BET_SIZE), as.numeric(BET_SIZE)),0) + 
                                               ifelse(as.numeric(PROFIT_LOSS) < 0, ifelse(gsub(" *$", "", BID_TYP) == "L", (as.numeric(PRICE_TAKEN)-1)*-as.numeric(BET_SIZE), -as.numeric(BET_SIZE)),0), 3)]
dt_4[,c("PROFIT_LOSS","TABLE_NUM") := list(ifelse(as.numeric(PROFIT_LOSS) > 0, ifelse(gsub(" *$", "", BID_TYP) == "B", (as.numeric(PRICE_TAKEN)-1)*as.numeric(BET_SIZE), as.numeric(BET_SIZE)),0) + 
                                               ifelse(as.numeric(PROFIT_LOSS) < 0, ifelse(gsub(" *$", "", BID_TYP) == "L", (as.numeric(PRICE_TAKEN)-1)*-as.numeric(BET_SIZE), -as.numeric(BET_SIZE)),0), 4)]
dt_5[,c("PROFIT_LOSS","TABLE_NUM") := list(ifelse(as.numeric(PROFIT_LOSS) > 0, ifelse(gsub(" *$", "", BID_TYP) == "B", (as.numeric(PRICE_TAKEN)-1)*as.numeric(BET_SIZE), as.numeric(BET_SIZE)),0) + 
                                               ifelse(as.numeric(PROFIT_LOSS) < 0, ifelse(gsub(" *$", "", BID_TYP) == "L", (as.numeric(PRICE_TAKEN)-1)*-as.numeric(BET_SIZE), -as.numeric(BET_SIZE)),0), 5)]

# 1.3 merge
dt <- rbind(dt_1, dt_2, dt_3, dt_4, dt_5)
dim(dt_1); dim(dt_2); dim(dt_3); dim(dt_4); dim(dt_5); dim(dt)
rm(list = c("dt_1", "dt_2", "dt_3", "dt_4", "dt_5", "colClasses")); gc()

# 1.4 cleansing BID_TYP and STATUS_ID
dt[, c("BID_TYP", "STATUS_ID") := 
       list(gsub(" *$", "", BID_TYP), gsub(" *$", "", STATUS_ID))]
str(dt)

# 1.5 formatting (not needed but I am an OCD)
# NUMBERS
dt$BET_PRICE <- as.numeric(dt$BET_PRICE)
dt$PRICE_TAKEN <- as.numeric(dt$PRICE_TAKEN)
dt$BET_SIZE <- as.numeric(dt$BET_SIZE)
dt$PROFIT_LOSS <- as.numeric(dt$PROFIT_LOSS)

# DATE
# PLACED_DATE
d <- strptime(dt$PLACED_DATE, "%d/%m/%Y %I:%M:%S %p")
dt$PLACED_DATE[is.na(d)] <- paste0(dt$PLACED_DATE[is.na(d)], " 6:00:00 AM") # PLACED_DATE
dt[, PLACED_DATE := parse_date_time(PLACED_DATE, "d!m*!Y! IM!S!")]
# TAKEN_DATE
dt$TAKEN_DATE[(nchar(dt$TAKEN_DATE) <20) & (!is.na(dt$TAKEN_DATE))] <- paste0(dt$TAKEN_DATE[(nchar(dt$TAKEN_DATE) <20) & 
                                                                                                (!is.na(dt$TAKEN_DATE))], " 6:01:10 AM")
dt[, TAKEN_DATE := parse_date_time(TAKEN_DATE, "d!m*!Y! IM!S!")]
# EVENT_DT
dt[, EVENT_DT := parse_date_time(EVENT_DT, "d!m*!Y! IM!S!")]
# OFF_DT
dt[, OFF_DT := parse_date_time(OFF_DT, "d!m*!Y! IM!S!")]
# SETTLED_DATE
dt[, SETTLED_DATE := parse_date_time(SETTLED_DATE, "d!m*!Y! IM!S!")]
# CANCELLED_DATE
dt$CANCELLED_DATE[(nchar(dt$CANCELLED_DATE) <20) & (!is.na(dt$CANCELLED_DATE))] <- paste0(dt$CANCELLED_DATE[(nchar(dt$CANCELLED_DATE) <20) & 
                                                                                                                (!is.na(dt$CANCELLED_DATE))], " 1:45:48 PM")
dt[, CANCELLED_DATE := parse_date_time(CANCELLED_DATE, "d!m*!Y! IM!S!")]

rm("d")
# FACTOR
unique(dt$BID_TYP)
unique(dt$STATUS_ID)
unique(dt$INPLAY_BET)
unique(dt$TABLE_NUM)
dt[, BID_TYP := as.factor(BID_TYP)]
dt[, STATUS_ID := as.factor(STATUS_ID)]
dt[, INPLAY_BET := as.factor(INPLAY_BET)]
dt[, TABLE_NUM := as.factor(TABLE_NUM)]

# check the class of columns
lapply(dt, class) # all good

# 1.6 NAs
apply(dt, 2, function(x) mean(is.na(x)))
# BET_ID              BET_TRANS_ID              MATCH_BET_ID                ACCOUNT_ID 
# 0.0000000                 0.0000000                 0.1872992                 0.0000000 
# COUNTRY_OF_RESIDENCE_NAME           PARENT_EVENT_ID                  EVENT_ID                     MATCH 
# 0.0000000                 0.0000000                 0.0000000                 0.0000000 
# EVENT_NAME                  EVENT_DT                    OFF_DT                   BID_TYP 
# 0.0000000                 0.0000000                 0.0000000                 0.0000000 
# STATUS_ID               PLACED_DATE                TAKEN_DATE              SETTLED_DATE 
# 0.0000000                 0.0000000                 0.1872992                 0.0000000 
# CANCELLED_DATE            SELECTION_NAME          PERSISTENCE_TYPE                 BET_PRICE 
# 0.8182610                 0.0000000                 0.9757013                 0.0000000 
# PRICE_TAKEN                INPLAY_BET                  BET_SIZE               PROFIT_LOSS 
# 0.1872992                 0.0000000                 0.0000000                 0.1881458 
# TABLE_NUM 
# 0.0000000 

# 1.7 Unique values
apply(dt, 2, function(x) length(unique(x)))
# BET_ID              BET_TRANS_ID              MATCH_BET_ID                ACCOUNT_ID 
# 2095220                   3461173                   1516071                     21020 
# COUNTRY_OF_RESIDENCE_NAME           PARENT_EVENT_ID                  EVENT_ID                     MATCH 
# 69                        44                        44                        44 
# EVENT_NAME                  EVENT_DT                    OFF_DT                   BID_TYP 
# 1                        44                        44                         2 
# STATUS_ID               PLACED_DATE                TAKEN_DATE              SETTLED_DATE 
# 4                    710461                    478716                        88 
# CANCELLED_DATE            SELECTION_NAME          PERSISTENCE_TYPE                 BET_PRICE 
# 336326                        14                         2                       350 
# PRICE_TAKEN                INPLAY_BET                  BET_SIZE               PROFIT_LOSS 
# 350                         2                    641709                   1127139 
# TABLE_NUM 
# 5 

#######################################
## 3.0 save ###########################
#######################################
save(dt, file = "../Datathon_Full_Dataset/processedData.RData")


