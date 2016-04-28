# set your working directory
setwd("C:/Users/Administrator/Allstate_hackathon")


#*****************create directory****************#
dir.create(paste0("data")) # original data and processed data all put in this folder
dir.create(paste0("submissions"))


#****************run scripts*****************#
source("script/utils.R")
source("script/preprocessing_ctree_mismatched_levels.R")
source("script/impute_mismatched_levels.R").
source("script/prepare_training_dataset.R")
source("script/xgboost_model.R")