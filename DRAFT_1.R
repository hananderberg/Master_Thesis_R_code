
####################  0. Load data and define variables needed ####################

# Variables

# Chosen data set: Choose from "letter", "credit", "news", "mushrooms", "bank"
data_name <- "credit"


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

# Test
load_test_data <- function(data_name, missingness_percent) {
  file_name_full_test <- paste0("/Users/sofiawadell/Documents/MasterThesis/R_v2/Master_Thesis_R_code/factor_test_data/factor_encode_", data_name, "_test.csv")
  test_data_x <- read.csv(file_name_full_test,header=TRUE, sep=",")
  file_name_miss_test <- paste0("/Users/sofiawadell/Documents/MasterThesis/R_v2/Master_Thesis_R_code/factor_test_data/factor_encode_", data_name, "_test_",missingness_percent,".csv")
  test_miss_data_x <- read.csv(file_name_miss_test,header=TRUE, sep=",")
  return(list(full=test_data_x,miss=test_miss_data_x))
}

# Test test
data <- load_test_data(data_name,"50")
test_data_x <- data$full
test_miss_data_x <- data$miss
head(test_miss_data_x)

# Train
load_train_data <- function(data_name, missingness_percent) {
  file_name_full_train <- paste0("/Users/sofiawadell/Documents/MasterThesis/R_v2/Master_Thesis_R_code/factor_train_data/factor_encode_", data_name, "_train.csv")
  train_data_x <- read.csv(file_name_full_train,header=TRUE, sep=",")
  file_name_miss_train <- paste0("/Users/sofiawadell/Documents/MasterThesis/R_v2/Master_Thesis_R_code/factor_train_data/factor_encode_", data_name, "_train_",missingness_percent,".csv")
  train_miss_data_x <- read.csv(file_name_miss_train,header=TRUE, sep=",")
  return(list(full=train_data_x,miss=train_miss_data_x))
}

# Test train 
data <- load_train_data("credit","50")
train_data_x <- data$full
train_miss_data_x <- data$miss
head(train_miss_data_x)


# find column names ending with "encoded"
encoded_cols <- grep("encoded$", names(credit_full), value = TRUE)

# set encoded columns as factors
credit_full[encoded_cols] <- lapply(credit_full[encoded_cols], as.factor)
str(credit_full)


imputed = mice(letter_10, maxit = 2, m=ncol(letter_10), seed = 500)
dat.complete.norm <- complete(imputed)