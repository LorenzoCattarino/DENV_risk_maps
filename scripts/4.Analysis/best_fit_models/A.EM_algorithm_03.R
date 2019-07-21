# Estimate foi for each 20 km square of the dataset disaggregated from the entire original foi dataset

library(ranger)

source(file.path("R", "random_forest", "fit_ranger_RF_and_make_predictions.R"))
source(file.path("R", "utility_functions.R"))  
source(file.path("R", "create_parameter_list.R"))


# define parameters ----------------------------------------------------------- 


extra_prms <- list(id = 15,
                   dependent_variable = "FOI",
                   no_predictors = 26)   

out_fl_nm <- "covariates_and_foi_20km.rds"


# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

model_id <- parameters$id

model_type <- paste0("model_", model_id)

number_of_predictors <- parameters$no_predictors

out_pth <- file.path("output", 
                     "EM_algorithm", 
                     "best_fit_models",
                     model_type,
                     "env_variables_and_init_pred")
  
covariates_dir <- parameters$covariates_dir


# load data -------------------------------------------------------------------


RF_obj <- readRDS(file.path("output",
                            "EM_algorithm",
                            "best_fit_models",
                            model_type,
                            "model_objects",
                            "all_data.rds"))

pxl_data <- readRDS(file.path("output", 
                              "EM_algorithm",
                              "best_fit_models",
                              model_type,
                              "env_variables", 
                              "env_vars_20km.rds"))

predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection",
                                     covariates_dir,
                                     "predictor_rank.csv"), 
                           stringsAsFactors = FALSE)


# pre processing --------------------------------------------------------------


my_predictors <- predictor_rank$name[1:number_of_predictors]


# submit job ------------------------------------------------------------------ 


p_i <- make_ranger_predictions(mod_obj = RF_obj, 
                               dataset = pxl_data, 
                               sel_preds = my_predictors)

pxl_data$p_i <- p_i

write_out_rds(pxl_data, out_pth, out_fl_nm)
