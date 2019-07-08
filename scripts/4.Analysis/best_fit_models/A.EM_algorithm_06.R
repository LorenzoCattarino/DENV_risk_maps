# Load back in the ranger model object 
# Make square-level predictions for the entire 20km dataset

library(ranger)

source(file.path("R", "random_forest", "fit_ranger_RF_and_make_predictions.R"))
source(file.path("R", "utility_functions.R"))  
source(file.path("R", "create_parameter_list.R"))


# define parameters ----------------------------------------------------------- 


extra_prms <- list(id = 15,
                   dependent_variable = "FOI",
                   no_predictors = 26) 


# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

model_id <- parameters$id

var_to_fit <- parameters$dependent_variable

number_of_predictors <- parameters$no_predictors

model_type <- paste0("model_", model_id)

out_fl_nm <- "square_predictions_all_data.rds"

out_pt <- file.path("output", 
                    "EM_algorithm", 
                    "best_fit_models", 
                    model_type)

RF_out_pth <- file.path("output", 
                        "EM_algorithm", 
                        "best_fit_models",
                        model_type,
                        "optimized_model_objects")

covariates_dir <- parameters$covariates_dir


# load data -------------------------------------------------------------------


pxl_data_covariates <- readRDS(file.path("output",
                                         "EM_algorithm",
                                         "best_fit_models",
                                         "env_variables",
                                         "env_vars_20km_2.rds"))

predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection",
                                     covariates_dir,
                                     "predictor_rank.csv"), 
                           stringsAsFactors = FALSE)


# pre processing --------------------------------------------------------------


my_predictors <- predictor_rank$name[1:number_of_predictors]


# get results ----------------------------------------------------------------- 


RF_obj <- readRDS(file.path(RF_out_pth, "RF_obj.rds"))


# make predictions ------------------------------------------------------------


prediction_set <- make_ranger_predictions(RF_obj, pxl_data_covariates, my_predictors)
  
  
# save ------------------------------------------------------------------------


write_out_rds(prediction_set, out_pt, out_fl_nm)
