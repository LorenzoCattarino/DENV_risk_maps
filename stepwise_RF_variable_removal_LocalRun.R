rm(list=ls())

# load packages
library(ranger)
library(weights)
library(ggplot2)
library(reshape2)
library(parallel)

# load functions 
source(file.path("R", "random_forest", "spatial_sampK_cv_rng2_stratif_pred.R"))
source(file.path("R", "random_forest", "RF_variable_importance_test", "RF_variable_importance_test_common_functions.R"))
source(file.path("R", "random_forest", "RF_variable_importance_test", "multi_steps_wrapper_LocalParallel.R"))
source(file.path("R", "random_forest", "RF_variable_importance_test", "stepwise_RF_variable_removal", "stepwise_RF_variable_addition_functions.R"))

# load data
dengue_dataset <- read.csv(file.path("data", "foi", "All_FOI_estimates_env_var.csv")) 

# remove NA and outliers
dengue_dataset <- dengue_dataset[!is.na(dengue_dataset$FOI),]
dengue_dataset <- dengue_dataset[!(dengue_dataset$country=="French Polynesia"|dengue_dataset$country=="Haiti"),]

# calculate weights 
#dengue_dataset$new.weight <- 1/dengue_dataset$variance
dengue_dataset$new.weight <- 1
dengue_dataset[dengue_dataset$FOI==0,"new.weight"] <- 0.25

# create vector of covariate column indices 
all_predictors <- c(26,9,10,27,44,34,25,37,47,50)
names(all_predictors) <- colnames(dengue_dataset[,all_predictors])

parameters <- list(
  grid.size = c(1,5,10),
  cell.fraction = 0.7,
  train.fraction = 1,
  pseudoAbs.value = -0.02,
  replicates = 1) 

result_folder <- "removal"

# get factor combinations 
factor_combinations <- get.factor.combinations (parameters)

# vector of folder names 
folder_names_vec <- sprintf("run_%s", factor_combinations$ID.run)

# create folders for data outputs 
sapply(folder_names_vec, function(x) {dir.create(file.path("output", "dengue_dataset", "predictor_importance_test", result_folder, x), FALSE, TRUE)})

# run 
test <- apply(factor_combinations[1,], 1, multi_grid_sizes_wrapper.removal, env.data, full.data, all.predictors)
