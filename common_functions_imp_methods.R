
####################  0. Load data and define variables needed ####################

# Variables
missingness = 0.2 

# Chosen data set: Choose from "letter", "credit", "news", "mushrooms", "bank"
data <- "news"

# Load packages
library(missForest)
library(mice)
library(dplyr)
library(Metrics)
library(mltools)
library(data.table)
library(magrittr)
library(imputeR)

####################  1a. Data load and pre-processing - adapt data sets ####################

if(data == "letter"){
  letter_temp <- read.csv("/Users/sofiawadell/Documents/MasterThesis/R_v2/Master_Thesis_R_code/Data_pre_processed/letter_pp.csv",header=TRUE, sep=",")
  dat <- letter_temp # No pre-processing needed
  
  
} else if (data == "credit"){
  credit_temp <- read.csv("/Users/sofiawadell/Documents/MasterThesis/R_v2/Master_Thesis_R_code/Data_pre_processed/credit_pp.csv",header=TRUE, sep = ",")
  cols_to_convert <- c(2, 3, 4, 6, 7, 8, 9, 10, 11)
  dat <- credit_temp %>% 
    mutate_at(cols_to_convert, as.factor)
  
} else if (data == "mushrooms"){
  mushrooms_temp <- read.csv("/Users/sofiawadell/Documents/MasterThesis/R_v2/Master_Thesis_R_code/Data_pre_processed/mushrooms_pp.csv",header=TRUE, sep=",")
  dat <- mushrooms_temp %<>% mutate(across(where(is.character),as.factor))

} else if (data == "news"){
  news_temp <- read.csv("/Users/sofiawadell/Documents/MasterThesis/R_v2/Master_Thesis_R_code/Data_pre_processed/news_pp.csv",header=TRUE, sep=",")
  news_temp$data_channel <- factor(news_temp$data_channel)
  news_temp$weekday <- factor(news_temp$weekday)
  dat <- news_temp %>% mutate(is_weekend = factor(is_weekend))

} else if (data == "bank"){
  bank_temp <- read.csv("/Users/sofiawadell/Documents/MasterThesis/R_v2/Master_Thesis_R_code/Data_pre_processed/bank_pp.csv",header=TRUE, sep=",")
  cols_to_convert <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 15)
  dat <- bank_temp %>% 
    mutate_at(cols_to_convert, as.factor)
  
} else {
  print("ERROR")
}
str(dat)


####################  1b. Data pre-processing - normalize  ####################

# Find categorical and numerical colums
cat_cols <- which(sapply(dat, is.factor))
num_cols <- which(sapply(dat, is.numeric))

#Define normalization function
normalize <- function(x) {
  return((x - min(x)) / (max(x) -min(x))
}

#apply normalization to numerical columns in data set
num_norm <- as.data.frame(lapply(dat[ , num_cols], normalize))
summary(num_norm)

# New dataset --> dat_norm
dat_cat <- dat[ , cat_cols]
dat_norm <- data.frame(num_norm,dat_cat)
summary(dat_norm)                   

# Extract categorical & numerical data to be able to compute separate RMSEs
cat_cols <- which(sapply(dat_norm, is.factor))
num_cols <- which(sapply(dat_norm, is.numeric))

dat_cat_norm <- dat_norm[ , cat_cols]
dat_num_norm <- dat_norm[ , num_cols]

head(dat_cat_norm)
head(dat_num_norm)

####################  2. Introduce missingness  ####################
set.seed(1)
dat.norm.mis <- prodNA(dat_norm, noNA = missingness)
summary(dat.norm.mis)

# View missing data & check ratio 
head(dat.norm.mis)
sapply(dat.norm.mis, function(x) sum(is.na(x)))

#Cat and num columns of missing data
dat_cat_norm_mis <- dat.norm.mis[ , cat_cols]
dat_num_norm_mis <- dat.norm.mis[ , num_cols]

####################  3. Impute using missForest  ####################

## Impute missing values. Use 'verbose' to see what happens between iterations:
dat.norm.imp <- missForest(dat.norm.mis, maxiter=3,xtrue = dat_norm, verbose = TRUE)

dat.norm.imp

## Here are the final results
dat.complete.norm <- dat.norm.imp$ximp

sapply(dat.complete.norm, function(x) sum(is.na(x)))
head(dat.complete.norm)

# Find numeric & categorical columns of complete data 
dat_cat_complete_norm <- dat.complete.norm[ , cat_cols]
dat_num_complete_norm <- dat.complete.norm[ , num_cols]

head(dat_cat_complete_norm)
head(dat_num_complete_norm)


####################  4. Calculate RMSE  ####################

## Numerical
squared_diff_num <- (dat_num_complete_norm - dat_num_norm) ^ 2 #Values
mean_squared_diff_num <- mean(as.matrix(squared_diff_num))
rmse_num_mf <- sqrt(mean_squared_diff_num)
rmse_num_mf

## Categorical
dat_cat_complete_norm_oh <- one_hot(as.data.table(dat_cat_complete_norm))
dat_cat_norm_oh <- one_hot(as.data.table(dat_cat_norm))

head(dat_cat_complete_norm_oh)
head(dat_cat_norm_oh)

squared_diff_cat <- (dat_cat_complete_norm_oh - dat_cat_norm_oh) ^ 2 #Values
mean_squared_diff_cat <- mean(as.matrix(squared_diff_cat))
rmse_cat_mf <- sqrt(mean_squared_diff_cat)
rmse_cat_mf

## Both numerical and categorical
dat_norm_oh <- data.frame(num_norm,dat_cat_norm_oh)
head(dat_norm_oh)
dat_complete_norm_oh <- data.frame(dat_num_complete_norm,dat_cat_complete_norm_oh)
head(dat_complete_norm_oh)


# Find mask matrix 

squared_diff <- (dat_complete_norm_oh - dat_norm_oh) ^ 2 #Values
mean_squared_diff <- mean(as.matrix(squared_diff))
rmse_full_mf <- sqrt(mean_squared_diff)
rmse_full_mf

cat("missForest: "," RMSE numerical: ",rmse_num_mf, " RMSE categorical: ", rmse_cat_mf, " RMSE: ", rmse_full_mf)



#### TEST ##

# both
squared_diff <- (dat.complete.norm - dat_norm) ^ 2 #Values
squared_diff
mean_squared_diff <- mean(as.matrix(squared_diff))
mean_squared_diff
rmse_full_mf <- sqrt(mean_squared_diff)
rmse_full_mf

mis <- is.na(dat.norm.mis)
rmse <- sqrt(mean((dat.complete.norm[mis] - dat_norm[mis])^{2}) / stats::var(dat_norm[mis]))
rmse


