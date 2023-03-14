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

# Factor encode categorical variables 
factor_encode <- function(df){
  encoded_cols_train <- grep("encoded$", names(df), value = TRUE)
  df[encoded_cols_train] <- lapply(df[encoded_cols_train], as.factor)
  df
}