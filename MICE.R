########## Intro 

# Load data
dat <- read.csv(url("http://goo.gl/19NKXV"), header=TRUE, sep=",") 
original_dat <- dat

# Load packages
library(mice)
library(missForest)
library(purrr)
library(tibble)
library(tidyverse)
library(magrittr)
library(dplyr)
library(imputeR)

########## Data preprocessing

## Change categorical variables to factors 
dat %<>% mutate(across(where(is.character),as.factor))

# Check
str(dat)

# Find categorical and numerical columns 
dat_num = dat[, sapply(dat, class) == 'numeric']
dat_fact = dat[, sapply(dat, class) == 'factor']

# Find default methods
meth <- make.method(dat)

############  Include missingness 

# Easy method
f <- rep(0.3, ncol(dat))
dat.mis.amp <- ampute(dat, freq = f, mech = "MAR")
dat.mis <- dat.mis.amp$amp

# View missing data
head(dat.mis)

########## Impute using MICE  
imputed = mice(dat.mis, method=meth, maxit = 100, m=ncol(dat.mis), seed = 500)
dat.complete <- complete(imputed)
sapply(dat.complete, function(x) sum(is.na(x)))
dat_num_com = dat.complete[, sapply(dat.complete, class) == 'numeric']

########## RMSE
Rmse(dat_num_com, dat_num_mis, dat_num, norm = TRUE)
head(dat_num)