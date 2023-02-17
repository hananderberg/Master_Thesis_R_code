########## Intro 

# Load data
dat <- read.csv(url("http://goo.gl/19NKXV"), header=TRUE, sep=",") 
original_dat <- dat

# Test to find ordered numerical category 
# nrow(dat)
# f <- c(1,2,3,4,5,6,7,8,9,10)
# Fake <- rep(f, times=25)
# length(Fake)
# dat$Fake <- Fake
# head(dat)

# Load packages
library(mice)
library(missForest)
library(purrr)
library(tibble)
library(tidyverse)
library(magrittr)
library(dplyr)
library(imputeR)
library(missMethods)
library(fastmatch)


########## Data preprocessing
str(dat)

## Change categorical variables to factors 
dat <- dat %<>% mutate(across(where(is.character),as.factor))

#Manually add if we have an ordered numerical column with > 2 factors
# dat <- dat %>% mutate(Fake = as.factor(Fake))

# Check if correct 
str(dat)

# Find categorical and numerical columns 
cat_cols <- which(sapply(dat, is.factor))
num_cols <- which(sapply(dat, is.numeric))

# Extract categorical & numerical data to be able to compute separate RMSEs 
dat_cat <- dat[ , cat_cols]
dat_num <- dat[ , num_cols]

# Create a vector with default methods
meth <- c()
for (i in 1:ncol(dat)){
  if (sapply(dat, is.numeric)[i]==TRUE){ #Numeric --> pmm
    meth <- append(meth,"pmm")
  } else if (sapply(dat, is.factor)[i]==TRUE && nlevels(dat[,i])==2){ #binary --> logreg
    meth <- append(meth,"logreg")
  } else {
    meth <- append(meth,"polyreg")
  }
}

#Manually add if we have an ordered column with > 2 factors 
#col <- fmatch("Fake",names(dat))
#col
# meth[col] <- "polr"

# Check method vector 
meth 

############  Include missingness 

set.seed(81)
dat.mis <- prodNA(dat, noNA = 0.2) #Also possible to use dat.mis <- delete_MCAR(dat, 0.2)
summary(dat.mis)

# View missing data & check ratio 
head(dat.mis)
sapply(dat.mis, function(x) sum(is.na(x)))

########## Impute using MICE  
imputed = mice(dat.mis, method=meth, maxit = 10, m=ncol(dat.mis), seed = 500)
dat.complete <- complete(imputed)
sapply(dat.complete, function(x) sum(is.na(x)))
head(dat.complete)

# Find numeric & categorical columns of cmplete data 
dat_num_com = dat.complete[, sapply(dat.complete, class) == 'numeric']

########## RMSE
Rmse(dat_num_com, dat_num_mis, dat_num, norm = TRUE)
head(dat_num)