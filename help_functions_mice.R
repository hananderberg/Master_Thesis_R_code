# Load normalized train data without target column
load_train_data <- function(data_name, missingness_percentage) {
  file_name_miss_train <- paste0("/Users/sofiawadell/Documents/MasterThesis/R_v2/Master_Thesis_R_code/norm_factor_train_data_wo_target/norm_factor_encode_", data_name, "_train_",missingness_percentage,".csv")
  train_miss_data_x <- read.csv(file_name_miss_train,header=TRUE, sep=",")
  return(train_miss_data_x)
}

# Load normalized test data without target column 
load_test_data <- function(data_name, missingness_percentage) {
  file_name_miss_test <- paste0("/Users/sofiawadell/Documents/MasterThesis/R_v2/Master_Thesis_R_code/norm_factor_test_data_wo_target/norm_factor_encode_", data_name, "_test_",missingness_percentage,".csv")
  test_miss_data_x <- read.csv(file_name_miss_test,header=TRUE, sep=",")
  return(test_miss_data_x)
}

# Factor encode categorical variables 
factor_encode <- function(df){
  encoded_cols_train <- grep("encoded$", names(df), value = TRUE)
  df[encoded_cols_train] <- lapply(df[encoded_cols_train], as.factor)
  df
}