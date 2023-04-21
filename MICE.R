
####################  0. Load packages and define variables needed ####################

# Variables
missingness = 0.3

# Chosen data set: Choose from "letter", "credit", "news", "mushroom", "bank"
data_name <- "bank"

# Chose if ctgan data should be used. Choose from "", "50" and "100"
ctgan <- "100"

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
if (ctgan ==""){
  train_miss_data_x <- load_train_data(data_name, missingness_percentage)
  print("CTGAN: None")
} else {
  train_miss_data_x <- load_train_data_ctgan(data_name, missingness_percentage, ctgan)
  print("CTGAN:")
  print(ctgan)
}

# Control
head(train_miss_data_x)
str(train_miss_data_x)
summary(train_miss_data_x)
sapply(train_miss_data_x, function(x) sum(is.na(x)))

# Load normalized test data
test_miss_data_x <- load_test_data(data_name, missingness_percentage)  

# Control
head(test_miss_data_x)


str(test_miss_data_x)
summary(test_miss_data_x)
sapply(test_miss_data_x, function(x) sum(is.na(x)))

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

####################  3. Impute using MICE  ####################
timing <- system.time({
  imp <- mice(full_miss_data_x, ignore = c(rep(FALSE, nbr_of_training_rows), rep(TRUE, nbr_of_testing_rows)), defaultMethod = c("norm", 'logreg', 'polyreg', 'polr'), m = ncol(full_miss_data_x), seed = 1)
  })

# Printing the execution time
cat("Execution time:", timing[3], "seconds\n")
completed_dataset <- complete(imp)


# Test if NA's left  
sapply(completed_dataset, function(x) sum(is.na(x)))

# Control
head(completed_dataset)

####################  4. Save imputed data set  ####################
# Extract data
train_imp_data_x <- completed_dataset[1:nbr_of_training_rows, ]
test_imp_data_x <- completed_dataset[(nbr_of_training_rows+1):nrow(completed_dataset), ]

str(train_imp_data_x)
str(test_imp_data_x)

# Save train data
if (ctgan == ""){
  file_path <- paste0("/Users/sofiawadell/Documents/MasterThesis/R_v2/Master_Thesis_R_code/norm_factor_imputed_mice_train_data/norm_factor_imputed_mice_", data_name, "_train_", missingness_percentage, ".csv")
  write.csv(train_imp_data_x, file_path, row.names = FALSE)
  
  # Save test data
  file_path <- paste0("/Users/sofiawadell/Documents/MasterThesis/R_v2/Master_Thesis_R_code/norm_factor_imputed_mice_test_data/norm_factor_imputed_mice_", data_name, "_test_", missingness_percentage, ".csv")
  write.csv(test_imp_data_x, file_path, row.names = FALSE)
} else {
  file_path <- paste0("/Users/sofiawadell/Documents/MasterThesis/R_v2/Master_Thesis_R_code/norm_factor_imputed_mice_train_data_ctgan", ctgan, "/norm_factor_imputed_mice_", data_name, "_train_", missingness_percentage, "_ctgan", ctgan, ".csv")
  write.csv(train_imp_data_x, file_path, row.names = FALSE)
  
  # Save test data
  file_path <- paste0("/Users/sofiawadell/Documents/MasterThesis/R_v2/Master_Thesis_R_code/norm_factor_imputed_mice_test_data_ctgan", ctgan, "/norm_factor_imputed_mice_", data_name, "_test_", missingness_percentage, "_ctgan", ctgan, ".csv")
  write.csv(test_imp_data_x, file_path, row.names = FALSE)
}






