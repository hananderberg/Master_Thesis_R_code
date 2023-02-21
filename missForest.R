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