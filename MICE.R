
####################  0. Load packages and define variables needed ####################

# Variables
missingness = 0.1

# Chosen data set: Choose from "letter", "credit", "news", "mushroom", "bank"
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
missingness_percentage = missingness*100

# Load normalized train data without target column
load_train_data <- function(data_name, missingness_percentage) {
  file_name_full_train <- paste0("/Users/sofiawadell/Documents/MasterThesis/R_v2/Master_Thesis_R_code/factor_train_data_wo_target/factor_encode_", data_name, "_train.csv")
  train_data_x <- read.csv(file_name_full_train,header=TRUE, sep=",")
  file_name_miss_train <- paste0("/Users/sofiawadell/Documents/MasterThesis/R_v2/Master_Thesis_R_code/factor_train_data_wo_target/factor_encode_", data_name, "_train_",missingness_percentage,".csv")
  train_miss_data_x <- read.csv(file_name_miss_train,header=TRUE, sep=",")
  return(list(full=train_data_x,miss=train_miss_data_x))
}

# Load normalized test data without target column 
load_test_data <- function(data_name, missingness_percentage) {
  file_name_full_test <- paste0("/Users/sofiawadell/Documents/MasterThesis/R_v2/Master_Thesis_R_code/factor_test_data_wo_target/factor_encode_", data_name, "_test.csv")
  test_data_x <- read.csv(file_name_full_test,header=TRUE, sep=",")
  file_name_miss_test <- paste0("/Users/sofiawadell/Documents/MasterThesis/R_v2/Master_Thesis_R_code/factor_test_data_wo_target/factor_encode_", data_name, "_test_",missingness_percentage,".csv")
  test_miss_data_x <- read.csv(file_name_miss_test,header=TRUE, sep=",")
  return(list(full=test_data_x,miss=test_miss_data_x))
}

### Commands

# Load normalized train data
train_data <- load_train_data(data_name,missingness_percentage)
train_data_x <- train_data$full
train_miss_data_x <- train_data$miss
head(train_miss_data_x)
str(train_miss_data_x)

# Load normalized test data
test_data <- load_test_data(data_name,missingness_percentage)
test_data_x <- test_data$full
test_miss_data_x <- test_data$miss
head(test_miss_data_x)
str(test_miss_data_x)

### Factor encode categorical variables 
# Train
encoded_cols_train <- grep("encoded$", names(train_data_x), value = TRUE)
train_data_x[encoded_cols_train] <- lapply(train_data_x[encoded_cols_train], as.factor)
train_miss_data_x[encoded_cols_train] <- lapply(train_miss_data_x[encoded_cols_train], as.factor)
str(train_miss_data_x)

# Test
encoded_cols_test <- grep("encoded$", names(test_data_x), value = TRUE)
test_data_x[encoded_cols_train] <- lapply(test_data_x[encoded_cols_train], as.factor)
test_miss_data_x[encoded_cols_train] <- lapply(test_miss_data_x[encoded_cols_train], as.factor)
str(test_miss_data_x)

# Merge train and testing data 
nbr_of_training_rows = nrow(train_miss_data_x)
nbr_of_testing_rows = nrow(test_miss_data_x)
full_miss_data_x = rbind(train_miss_data_x, test_miss_data_x)

####################  3. Impute using MICE  ####################
imp <- mice(full_miss_data_x, ignore = c(rep(FALSE, nbr_of_training_rows), rep(TRUE, nbr_of_testing_rows)), defaultMethod = c("norm", 'logreg', 'polyreg', 'polr'), m = ncol(full_miss_data_x), seed = 1)
completed_dataset <- complete(imp)

####################  4. Save imputed dataset  ####################



