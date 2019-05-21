# Load back in the ranger model object 
# Make square-level predictions for the entire 20km dataset

library(ranger)

source(file.path("R", "random_forest", "fit_ranger_RF_and_make_predictions.R"))
source(file.path("R", "utility_functions.R"))  


# define parameters ----------------------------------------------------------- 


parameters <- list(id = 13,
                   dependent_variable = "Z",
                   no_predictors = 26) 


# define variables ------------------------------------------------------------


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


# load data -------------------------------------------------------------------


pxl_data_covariates <- readRDS(file.path("output",
                                         "EM_algorithm",
                                         "best_fit_models",
                                         "env_variables",
                                         "env_vars_20km_2.rds"))

predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection",
                                     "stepwise_v3",
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
