# **************************************
# packages
# **************************************
# install package
install_packages <- 0 #set to 1 if run for first time
if(install_packages == 1){
  install.packages(c("Hmisc",
                     "xgboost",
                     "readr",
                     "stringr",
                     "caret",
                     "car",
                     "plyr",
                     "dplyr",
                     "tidyr",
                     "data.table",
                     "DescTools",
                     "Matrix",
                     "glmnet",
                     "AppliedPredictiveModeling",
                     "caret",
                     "corrrplot",
                     "e1071",
                     "rpart",
                     "party",
                     "dummies"))
}

# load libraries
library(plyr)
library(ggplot2)
library(Hmisc)
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
library(plyr)
library(dplyr)
library(tidyr)
library(data.table)
library(DescTools)
library(Matrix)
library(glmnet)
library(AppliedPredictiveModeling)
library(caret)
library(corrplot)
library(e1071)
library(rpart)
library(party)
library(dummies)
# **************************************
# functions
# **************************************
'%nin%' <- Negate('%in%')

LogLossBinary = function(actual, predicted, eps = 1e-15) {  
  predicted = pmin(pmax(predicted, eps), 1-eps)  
  - (sum(actual * log(predicted) + (1 - actual) * log(1 - predicted))) / length(actual)
}