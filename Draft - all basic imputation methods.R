

# https://cran.r-project.org/web/packages/missCompare/missCompare.pdf 
#https://www.analyticsvidhya.com/blog/2016/03/tutorial-powerful-packages-imputing-missing-values/
# https://www.r-bloggers.com/2016/06/handling-missing-data-with-mice-package-a-simple-approach/ 

.libPaths("/Users/sofiawadell/Documents/MasterThesis/R")
.libPaths()
list.files()
list.dirs()

# Load packages 
# install.packages("mice")
library(mice)
library(missForest)

# install.packages("imputeR")
library(imputeR)


#############Method 1 using MICE##########

#MICE package helps you to impute missing values by using multiple 
#techniques,depending on the kind of data you are working with.
#the methods used by this package are:

#  PMM (Predictive Mean Matching)  – For numeric variables
#  logreg(Logistic Regression) – For Binary Variables( with 2 levels)
#  polyreg(Bayesian polytomous regression) – For Factor Variables (>= 2 levels)
#  Proportional odds model (ordered, >= 2 levels)

##### 1a) IRIS EXAMPLE ############################################
data("iris")
summary(iris)
set.seed(81)
iris.mis <- prodNA(iris, noNA = 0.2)
summary(iris.mis)

# m = number of variables, maxit= 50, pmm = related to continuous data, won't work with categorical data 
imp <- mice(iris.mis, m=5, maxit = 10, method = c("pmm", "cart", "cart", "cart", "polr"), seed = 500)
iris_complete = complete(imp)
summary(iris_complete)

###################### EXAMPLE of MICE and RMSE 

## RMSE

# calculate the normalised RMSE for the imputation
Rmse(iris_complete[, c(1:4)], iris.mis[, c(1:4)], iris[, c(1:4)], norm = TRUE)


######## 1b) MICE: Example with another dataset 
dat <- read.csv(url("http://goo.gl/19NKXV"), header=TRUE, sep=",")
original <- dat
summary(dat)
set.seed(10)
dat[sample(1:nrow(dat), 20), "Cholesterol"] <- NA
dat[sample(1:nrow(dat), 20), "Smoking"] <- NA
dat[sample(1:nrow(dat), 20), "Education"] <- NA
dat[sample(1:nrow(dat), 5), "Age"] <- NA
dat[sample(1:nrow(dat), 5), "BMI"] <- NA

summary(dat)
sapply(dat, function(x) sum(is.na(x)))

# Transform data to factors
library(dplyr) 

dat <- dat %>%
  mutate(Smoking = as.factor(Smoking)) %>% 
  mutate(Education = as.factor(Education)) %>% 
  mutate(Cholesterol = as.numeric(Cholesterol))

str(dat)
summary(dat)

library(mice)
init = mice(dat, maxit=50) 
meth = init$method
meth[c("Age")]=""
meth[c("Cholesterol")]="norm" 
meth[c("Smoking")]="logreg" 
meth[c("Education")]="polyreg"
set.seed(103)
imputed = mice(dat, method=meth, m=5)
imputed <- complete(imputed)
sapply(imputed, function(x) sum(is.na(x)))

############# Method 2 using "missForest"##########

## Nonparametric missing value imputation on mixed-type data:
## Take a look at iris definitely has a variable that is a factor 
data(iris)
summary(iris)

## The data contains four continuous and one categorical variable.
## Artificially produce missing values using the 'prodNA' function:
set.seed(81)
iris.mis <- prodNA(iris, noNA = 0.2)
summary(iris.mis)

## Impute missing values providing the complete matrix for
## illustration. Use 'verbose' to see what happens between iterations:
iris.imp <- missForest(iris.mis, xtrue = iris, verbose = TRUE)
iris_complete = complete(imp)

## Here are the final results
iris.imp

##As can be seen here it still has the factor column
str(iris.imp$ximp)

# calculate the normalised RMSE for the imputation (not sure if it's working)
Rmse(iris_complete[, c(1:4)], iris.mis[, c(1:4)], iris[, c(1:4)], norm = TRUE)


########## Method 3: MEAN imputation  ##########
data(iris)
summary(iris)

## The data contains four continuous and one categorical variable.
## Artificially produce missing values using the 'prodNA' function:
set.seed(81)
iris.mis <- prodNA(iris, noNA = 0.2)
summary(iris.mis)
head(iris.mis)

# Impute columns 1 to 4
for(i in 1:4) {
  iris.mis[ , i][is.na(iris.mis[ , i])] <- mean(iris.mis[ , i], na.rm = TRUE)
}

head(iris.mis)

# Impute categorical
val <- unique(iris.mis[,5][!is.na(iris.mis[,5])])    
my_mode <- val[which.max(tabulate(match(iris.mis[,5], val)))]                                   # Replicate vec_miss
iris.mis[,5][is.na(iris.mis)[,5]] <- my_mode 

head(iris.mis)


##### Method 4: KNN
# https://www.youtube.com/watch?v=u8XvfhBdbMw 
