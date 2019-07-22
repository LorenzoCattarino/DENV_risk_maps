# Fits RF to all original foi data (using fixed RF parameters) 

library(ranger)

source(file.path("R", "utility_functions.R"))
source(file.path("R", "create_parameter_list.R"))
source(file.path("R", "random_forest", "fit_ranger_RF_and_make_predictions.R"))


# define parameters ----------------------------------------------------------- 


extra_prms <- list(id = 15,
                   dependent_variable = "FOI",
                   no_predictors = 26,
                   ranger_threads = NULL)

out_name <- "all_data.rds"  


# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

model_id <- parameters$id

model_type <- paste0("model_", model_id)

var_to_fit <- parameters$dependent_variable

foi_offset <- parameters$foi_offset
  
number_of_predictors <- parameters$no_predictors

out_pth <- file.path("output", 
                     "EM_algorithm", 
                     "best_fit_models", 
                     model_type,
                     "model_objects")

covariates_dir <- parameters$covariates_dir


# load data ------------------------------------------------------------------- 


foi_data <- readRDS(file.path("output", 
                              "EM_algorithm", 
                              "best_fit_models", 
                              model_type,
                              "adm_foi_data",
                              "adm_foi_data.rds"))

predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection",
                                     covariates_dir,
                                     "predictor_rank.csv"), 
                           stringsAsFactors = FALSE)


# pre processing -------------------------------------------------------------- 


my_predictors <- predictor_rank$name[1:number_of_predictors]

training_dataset <- foi_data[, c(var_to_fit, my_predictors, "new_weight")]


# run job --------------------------------------------------------------------- 


RF_obj <- fit_ranger_RF(parms = parameters, 
                        dependent_variable = var_to_fit,
                        predictors = my_predictors, 
                        training_dataset = training_dataset, 
                        my_weights = "new_weight")

write_out_rds(RF_obj, out_pth, out_name)
