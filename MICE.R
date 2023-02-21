####################  1. Intro  ####################

# Load packages
library(mice)
library(missForest)
library(dplyr)
library(fastmatch)
library(Metrics)
library(mltools)
library(data.table)
library(magrittr)


# Load data
dat <- read.csv(url("http://goo.gl/19NKXV"), header=TRUE, sep=",") 
original_dat <- dat

# Variables
missingness = 0.2 #TBU
maxit_MICE = 100

####################  1. Data pre-processing  ####################
str(dat)

## Change categorical variables to factors 
dat <- dat %<>% mutate(across(where(is.character),as.factor))

# Check if correct 
str(dat)

# Find categorical and numerical columns in data set
cat_cols <- which(sapply(dat, is.factor))
num_cols <- which(sapply(dat, is.numeric))

#Define normalization function
min_max_norm <- function(x) {
  2*(x - min(x)) / (max(x) - min(x)) - 1
}

#apply  normalization to numerical columns in data set
num_norm <- as.data.frame(lapply(dat[ , num_cols], min_max_norm))
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

# Create a vector with default methods
meth <- c()
for (i in 1:ncol(dat_norm)){
  if (sapply(dat_norm, is.numeric)[i]==TRUE){ #Numeric --> pmm
    meth <- append(meth,"pmm")
  } else if (sapply(dat_norm, is.factor)[i]==TRUE && nlevels(dat_norm[,i])==2){ #binary --> logreg
    meth <- append(meth,"logreg")
  } else {
    meth <- append(meth,"polyreg")
  }
}

# Check method vector 
meth 

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

####################  3. Impute using MICE  ####################

imputed = mice(dat.norm.mis, method=meth, maxit = maxit_MICE, m=ncol(dat.norm.mis), seed = 500)
dat.complete.norm <- complete(imputed)
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
rmse_num <- sqrt(mean_squared_diff_num)
rmse_num

## Categorical
dat_cat_complete_norm_oh <- one_hot(as.data.table(dat_cat_complete_norm))
dat_cat_norm_oh <- one_hot(as.data.table(dat_cat_norm))

head(dat_cat_complete_norm_oh)
head(dat_cat_norm_oh)

squared_diff_cat <- (dat_cat_complete_norm_oh - dat_cat_norm_oh) ^ 2 #Values
mean_squared_diff_cat <- mean(as.matrix(squared_diff_cat))
rmse_cat <- sqrt(mean_squared_diff_cat)
rmse_cat

## Both numerical and categorical
dat_norm_oh <- data.frame(num_norm,dat_cat_norm_oh)
head(dat_norm_oh)
dat_complete_norm_oh <- data.frame(dat_num_complete_norm,dat_cat_complete_norm_oh)
head(dat_complete_norm_oh)

squared_diff <- (dat_complete_norm_oh - dat_norm_oh) ^ 2 #Values
mean_squared_diff <- mean(as.matrix(squared_diff))
rmse_full <- sqrt(mean_squared_diff)
rmse_full

cat("RMSE numerical: ",rmse_num, " RMSE categorical: ", rmse_cat, " RMSE: ", rmse_full)
