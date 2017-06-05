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
source(file.path("R", "random_forest", "RF_variable_importance_test", "stepwise_RF_variable_addition", "stepwise_RF_variable_addition_functions.R"))
source(file.path("R", "random_forest", "convert_df_to_list.R"))

# load data
dengue_dataset <- read.csv(file.path("data", "foi", "All_FOI_estimates_env_var.csv")) 

# remove NA and outliers
dengue_dataset <- dengue_dataset[!is.na(dengue_dataset$FOI), ]
dengue_dataset <- subset(dengue_dataset, country != "French Polynesia" & country != "Haiti")

# Get all the combinations of model predictors you want to try, land use classes excluded ---------------------------------------

altitude_var_names <- "altitude"
fourier_transform_elements <- c("const_term",	"Re0",	"Im0",	"Re1",	"Im1")
FTs_data_names <- c("DayTemp", "EVI", "MIR", "NightTemp", "RFE")
all_predictors <- c(altitude_var_names, 
                    apply(expand.grid(fourier_transform_elements, FTs_data_names), 1, function(x) paste(x[2],x[1], sep="_")))

predictor_combination_list <- list(
  all_predictors = all_predictors
)

# Get all combinations of factors -------------------------------------------------

factor_combinations <- expand.grid(ID.exp = 6, 
                                   grid.size = c(1,5,10), 
                                   cell.fraction = 0.7, 
                                   train.fraction = 1, 
                                   pseudoAbs.value = -0.02,
                                   weights = 1,
                                   adm = 1,
                                   pred_tag = "all_predictors",
                                   replicates = 1:10)

factor_combinations <- cbind(ID.run = seq_len(nrow(factor_combinations)), factor_combinations)

# Fitting RF models ----------------------------------------------------------

factor_combinations_list <- df_to_list (x = factor_combinations, use_names = TRUE)

# run 1 list element 
test <- lapply(factor_combinations_list[1], 
               multi_grid_sizes_wrapper.addition, 
               dataset = dengue_dataset, 
               all_predictor_combs = predictor_combination_list, 
               factor_combs_df = factor_combinations)

# Check intermediate outputs
readRDS(file.path("output", "dengue_dataset", "predictor_importance_test", 
                  "addition", "exp_6", "run_1", 
                  "predictor_importance_test_step_output_exp_6_run_1_level_1_step_1.rds"))
