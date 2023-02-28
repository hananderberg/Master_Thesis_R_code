### Packages
library(dplyr)

### Load data sets
letter_temp <- read.csv("/Users/sofiawadell/Documents/MasterThesis/R_v2/Master_Thesis_R_code/Original_data/letter.csv",header=TRUE, sep=",")
credit_temp <- read.csv("/Users/sofiawadell/Documents/MasterThesis/R_v2/Master_Thesis_R_code/Original_data/credit.csv",header=TRUE)
mushrooms_temp <- read.csv("/Users/sofiawadell/Documents/MasterThesis/R_v2/Master_Thesis_R_code/Original_data/mushrooms.csv",header=TRUE, sep=",")
news_temp <- read.csv("/Users/sofiawadell/Documents/MasterThesis/R_v2/Master_Thesis_R_code/Original_data/news.csv",header=TRUE, sep=",")
bank_temp <- read.csv("/Users/sofiawadell/Documents/MasterThesis/R_v2/Master_Thesis_R_code/Original_data/bank.csv",header=TRUE, sep=",")


### Pre-process data ###


## (1) Letter ##

# - Categorical: None. 
# - Remove: Target variable, col1
head(letter_temp)

# Check if missing values 
sapply(letter_temp, function(x) sum(is.na(x))) 

# Remove column
letter <- letter_temp[, 2:(ncol(letter_temp))]
head(letter)

# write.csv(letter, file = "/Users/sofiawadell/Documents/MasterThesis/R_v2/Master_Thesis_R_code/Data_pre_processed/letter_pp.csv", row.names=FALSE)


## (2) Credit ##

# - Remove: Target variable - default.payment.next.month, Drop cols - ID 
# - Categorical: ['SEX', 'EDUCATION', 'MARRIAGE', 'PAY_0', 'PAY_2','PAY_3','PAY_4','PAY_5','PAY_6']
head(credit_temp)

# Check if missing values 
sapply(credit_temp, function(x) sum(is.na(x))) 

# Remove 
credit_temp <- credit_temp[, 2:(ncol(credit_temp)-1)] 
head(credit_temp)

# Categorical features 
str(credit_temp)
cols_to_convert <- c(2, 3, 4, 6, 7, 8, 9, 10, 11)
credit <- credit_temp %>% 
  mutate_at(cols_to_convert, as.factor)
str(credit)

write.csv(credit, file = "/Users/sofiawadell/Documents/MasterThesis/R_v2/Master_Thesis_R_code/Data_pre_processed/credit_pp.csv", row.names=FALSE)


## (3) Mushrooms ##

# - Remove: Target variable - Class
# - Categorical: All 
head(mushrooms_temp)

# Check if missing values 
sapply(mushrooms_temp, function(x) sum(is.na(x))) 

# Remove 
mushrooms_temp <- mushrooms_temp[, 2:(ncol(mushrooms_temp))] 
head(mushrooms_temp)

# Categorical features 
str(mushrooms_temp)
mushrooms <- mushrooms_temp %<>% mutate(across(where(is.character),as.factor))
str(mushrooms)

write.csv(mushrooms, file = "/Users/sofiawadell/Documents/MasterThesis/R_v2/Master_Thesis_R_code/Data_pre_processed/mushrooms_pp.csv", row.names=FALSE)


## (4) News ##

# - Remove: Target variable - 'shares'. Drop cols: ['url', 'timedelta']
# - Categorical (here one-hot-encoded) ['data_channel', 'weekday', 'is_weekend']  
head(news_temp)
str(news_temp)

# Check if missing values 
sapply(news_temp, function(x) sum(is.na(x))) 

# Remove 
news_temp <- news_temp[, 3:(ncol(news_temp)-1)]
head(news_temp)

# Categorical features 
str(news_temp)

# Data_channel - test 
# number_of_nans <- 0
# for (i in (1:length(news_temp$data_channel_is_bus))){
#   if (news_temp$data_channel_is_lifestyle[i] == 0 && news_temp$data_channel_is_entertainment[i] == 0 && news_temp$data_channel_is_bus[i] == 0 && news_temp$data_channel_is_socmed[i] == 0 && news_temp$data_channel_is_tech[i] == 0 && news_temp$data_channel_is_world[i] == 0) {
#     number_of_nans = number_of_nans+1
#   }
# }

news_temp$data_channel <- ifelse(news_temp$data_channel_is_lifestyle == 1, "data_channel_is_lifestyle", ifelse(news_temp$data_channel_is_entertainment == 1, "data_channel_is_entertainment", ifelse(news_temp$data_channel_is_bus == 1, "data_channel_is_bus", ifelse(news_temp$data_channel_is_socmed == 1, "data_channel_is_socmed", ifelse(news_temp$data_channel_is_tech == 1, "data_channel_is_tech", ifelse(news_temp$data_channel_is_world == 1, "data_channel_is_world", "No channel"))))))
news_temp$data_channel <- factor(news_temp$data_channel)
news_temp <- select(news_temp, -data_channel_is_lifestyle, -data_channel_is_entertainment, -data_channel_is_bus, -data_channel_is_socmed, -data_channel_is_tech,-data_channel_is_world)

# weekday
str(news_temp)
news_temp$weekday <- ifelse(news_temp$weekday_is_monday == 1, "weekday_is_monday", ifelse(news_temp$weekday_is_tuesday == 1, "weekday_is_tuesday", ifelse(news_temp$weekday_is_wednesday == 1, "weekday_is_wednesday", ifelse(news_temp$weekday_is_thursday == 1, "weekday_is_thursday", ifelse(news_temp$weekday_is_friday == 1, "weekday_is_friday", ifelse(news_temp$weekday_is_saturday == 1, "weekday_is_saturday", ifelse(news_temp$weekday_is_sunday == 1, "weekday_is_sunday", NA)))))))
news_temp$weekday <- factor(news_temp$weekday)
news_temp <- select(news_temp, -weekday_is_monday, -weekday_is_tuesday, -weekday_is_wednesday, -weekday_is_thursday, -weekday_is_friday, -weekday_is_saturday, -weekday_is_sunday)

# is_weekend
str(news_temp)
news <- news_temp %>% mutate(is_weekend = factor(is_weekend))
str(news)

write.csv(news, file = "/Users/sofiawadell/Documents/MasterThesis/R_v2/Master_Thesis_R_code/Data_pre_processed/news_pp.csv", row.names=FALSE)

## (5) Bank ##

# - Remove: Target variable - y
# - Categorical: ['job', 'marital', 'education', 'default', 'housing', 'loan', 'contact', 'month', 'day_of_week', 'poutcome']

head(bank_temp)

# Check if missing values 
sapply(bank_temp, function(x) sum(is.na(x))) 

# Remove target
bank_temp <- bank_temp[, 1:(ncol(bank_temp)-1)]
head(bank_temp)

# Categorical features: 
str(bank_temp)
cols_to_convert <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 15)
bank <- bank_temp %>% 
  mutate_at(cols_to_convert, as.factor)
str(bank)

write.csv(bank, file = "/Users/sofiawadell/Documents/MasterThesis/R_v2/Master_Thesis_R_code/Data_pre_processed/bank_pp.csv", row.names=FALSE)
