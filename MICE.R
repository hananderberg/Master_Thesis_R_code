
####################  0. Load packages and define variables needed ####################

# Variables
missingness = 0.1

# Chosen data set: Choose from "letter", "credit", "news", "mushroom", "bank"
data_name <- "bank"

####################  1. Data load and pre-processing - adapt data sets ####################

# Load packages needed
library(missForest)
library(mice)
library(dplyr)
library(Metrics)
library(mltools)
library(data.table)
library(magrittr)
library(imputeR)
source("help_functions_mice.R")

# State missingness percentage
missingness_percentage = missingness*100

# Load normalized train data
train_data <- load_train_data(data_name,missingness_percentage)
train_miss_data_x <- train_data$miss

# Control
head(train_miss_data_x)
str(train_miss_data_x)

# Load normalized test data
test_data <- load_test_data(data_name,missingness_percentage)
test_miss_data_x <- test_data$miss

# Control
head(test_miss_data_x)
str(test_miss_data_x)

### Factor encode categorical variables 
train_miss_data_x_encoded <- factor_encode(train_miss_data_x)
test_miss_data_x_encoded <- factor_encode(test_miss_data_x)

# Control
str(train_miss_data_x_encoded)
str(test_miss_data_x_encoded)

# Merge train and testing data 
nbr_of_training_rows = nrow(train_miss_data_x_encoded)
nbr_of_testing_rows = nrow(test_miss_data_x_encoded)
full_miss_data_x = rbind(train_miss_data_x_encoded, test_miss_data_x_encoded)

# Control
str(full_miss_data_x)

####################  3. Impute using MICE  ####################
imp <- mice(full_miss_data_x, ignore = c(rep(FALSE, nbr_of_training_rows), rep(TRUE, nbr_of_testing_rows)), defaultMethod = c("norm", 'logreg', 'polyreg', 'polr'), m = ncol(full_miss_data_x), seed = 1)
completed_dataset <- complete(imp)

# Test if NA's left  
sapply(completed_dataset, function(x) sum(is.na(x)))

# Control
head(completed_dataset)

####################  4. Save imputed data set  ####################

# Extract data
train_imp_data_x <- completed_dataset[1:nbr_of_training_rows, ]
test_imp_data_x <- completed_dataset[(nbr_of_training_rows+1):nrow(completed_dataset), ]

# Save train data
file_path <- paste0("/Users/sofiawadell/Documents/MasterThesis/R_v2/Master_Thesis_R_code/imputed_train_data/imputed_", data_name, "_train_", missingness_percentage, ".csv")
write.csv(train_imp_data_x, file_path, row.names = FALSE)

# Save test data
file_path <- paste0("/Users/sofiawadell/Documents/MasterThesis/R_v2/Master_Thesis_R_code/imputed_test_data/imputed_", data_name, "_test_", missingness_percentage, ".csv")
write.csv(test_imp_data_x, file_path, row.names = FALSE)



