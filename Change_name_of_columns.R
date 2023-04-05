load_data <- function(data_name, missingness_percentage, train_or_test) {
  file_name_imputed <- paste0("/Users/sofiawadell/Documents/MasterThesis/R_v2/Master_Thesis_R_code/norm_factor_imputed_mice_", train_or_test, "_data/norm_factor_imputed_mice_", data_name, "_", train_or_test, "_", missingness_percentage,".csv")
  imputed_data <- read.csv(file_name_imputed,header=TRUE, sep=",")
  return(imputed_data)
}

data_name = "mushroom"
missingness_percentage = 50
train_or_test = "test"
  
# Load data
imputed_data <- load_data(data_name, missingness_percentage, train_or_test)

# Replace dots with "-"
colnames(imputed_data) <- gsub("\\.", "-", colnames(imputed_data))

# View the modified data set
imputed_data

# # Save modified data set
# if (train_or_test == "train"){
#   # Save train data
#   file_path <- paste0("/Users/sofiawadell/Documents/MasterThesis/R_v2/Master_Thesis_R_code/norm_factor_imputed_mice_train_data/norm_factor_imputed_mice_", data_name, "_train_", missingness_percentage, ".csv")
#   write.csv(imputed_data, file_path, row.names = FALSE)
# }

if (train_or_test == "test"){
  # Save test data
  file_path <- paste0("/Users/sofiawadell/Documents/MasterThesis/R_v2/Master_Thesis_R_code/norm_factor_imputed_mice_test_data/norm_factor_imputed_mice_", data_name, "_test_", missingness_percentage, ".csv")
  write.csv(imputed_data, file_path, row.names = FALSE)
}



