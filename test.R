### TRAIN ###
load_train_data <- function(data_name, missingness_percentage,ctgan) {
  file_name_miss_train <- paste0("/Users/sofiawadell/Documents/MasterThesis/R_v2/Master_Thesis_R_code/norm_factor_imputed_mice_train_data_ctgan",ctgan, "/norm_factor_imputed_mice_", data_name, "_train_",missingness_percentage,"_ctgan",ctgan,".csv")
  train_miss_data_x <- read.csv(file_name_miss_train,header=TRUE, sep=",")
  return(train_miss_data_x)
}

data_name = "mushroom"
missingness_percentage = 30
ctgan = "50"

train_miss_data <- load_train_data(data_name, missingness_percentage, ctgan)

summary(train_miss_data)

# Replace all dots with -
colnames(train_miss_data) <- gsub("\\.", "-", colnames(train_miss_data))

summary(train_miss_data)

# Replace missing values 
train_data_no_missing <- apply(train_miss_data, 2, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
summary(train_data_no_missing)

# Save data
file_path <- paste0("/Users/sofiawadell/Documents/MasterThesis/R_v2/Master_Thesis_R_code/norm_factor_imputed_mice_train_data_ctgan",ctgan, "/norm_factor_imputed_mice_", data_name, "_train_",missingness_percentage,"_ctgan",ctgan,".csv")
write.csv(train_data_no_missing, file_path, row.names = FALSE)

### TEST ###
load_test_data <- function(data_name, missingness_percentage, ctgan) {
  file_name_miss_test <- paste0("/Users/sofiawadell/Documents/MasterThesis/R_v2/Master_Thesis_R_code/norm_factor_imputed_mice_test_data_ctgan",ctgan, "/norm_factor_imputed_mice_", data_name, "_test_",missingness_percentage,"_ctgan",ctgan,".csv")
  test_miss_data_x <- read.csv(file_name_miss_test,header=TRUE, sep=",")
  return(test_miss_data_x)
}

test_data <- load_test_data(data_name, missingness_percentage, ctgan)

summary(test_data)

# Replace all dots with -
colnames(test_data) <- gsub("\\.", "-", colnames(test_data))
summary(test_data)

# Replace missing values 
test_data_no_misisng <- apply(test_data, 2, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
summary(test_data_no_misisng)

# Save data
file_path <- paste0("/Users/sofiawadell/Documents/MasterThesis/R_v2/Master_Thesis_R_code/norm_factor_imputed_mice_test_data_ctgan",ctgan, "/norm_factor_imputed_mice_", data_name, "_test_",missingness_percentage,"_ctgan",ctgan,".csv")
write.csv(test_data_no_misisng, file_path, row.names = FALSE)
