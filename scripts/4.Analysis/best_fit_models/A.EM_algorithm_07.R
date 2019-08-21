# Creates a data frame with:  
#
# 1) admin unit observations
# 2) admin unit predictions 
# 3) population weighted average of the square predictions, within the observation's admin unit
# 4) population weighted average of the 1 km pixel predictions, within the observation's admin unit

library(ranger)
library(dplyr)

source(file.path("R", "utility_functions.R"))
source(file.path("R", "create_parameter_list.R"))
source(file.path("R", "prepare_datasets", "average_up.R"))
source(file.path("R", "random_forest", "fit_ranger_RF_and_make_predictions.R"))
source(file.path("R", "random_forest", "join_predictions.R"))


# define parameters -----------------------------------------------------------


extra_prms <- list(id = 3,
                   dependent_variable = "FOI",
                   no_predictors = 17)

out_name <- "all_scale_predictions.rds"


# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

model_id <- parameters$id

var_to_fit <- parameters$dependent_variable
  
number_of_predictors <- parameters$no_predictors

foi_offset <- parameters$foi_offset

model_type <- paste0("model_", model_id)

RF_obj_path <- file.path("output",
                         "EM_algorithm",
                         "best_fit_models",
                         model_type,
                         "optimized_model_objects")

out_pt <- file.path("output",
                    "EM_algorithm",
                    "best_fit_models",
                    model_type,
                    "adm_foi_predictions")

covariates_dir <- parameters$covariates_dir


# load data ------------------------------------------------------------------- 


foi_dataset <- readRDS(file.path("output",
                                 "EM_algorithm",
                                 "best_fit_models",
                                 model_type,
                                 "adm_foi_data",
                                 "adm_foi_data.rds")) 

sqr_dataset <- readRDS(file.path("output",
                                 "EM_algorithm",
                                 "best_fit_models",
                                 model_type,
                                 "env_variables",
                                 "env_vars_20km.rds"))

adm_dataset <- read.csv(file.path("output",
                                  "env_variables",
                                  "All_adm1_env_var.csv"),
                        stringsAsFactors = FALSE)

predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection",
                                     covariates_dir,
                                     "predictor_rank.csv"), 
                           stringsAsFactors = FALSE)

all_sqr_predictions <- readRDS(file.path("output",
                                         "EM_algorithm",
                                         "best_fit_models",
                                         model_type,
                                         "square_predictions_all_data.rds"))

RF_obj <- readRDS(file.path(RF_obj_path, "RF_obj.rds"))


# pre processing --------------------------------------------------------------


my_predictors <- predictor_rank$name[1:number_of_predictors]


# run ------------------------------------------------------------------------- 


join_all <- join_predictions(parms = parameters, 
                             foi_dataset = foi_dataset, 
                             RF_obj = RF_obj, 
                             adm_dataset = adm_dataset,
                             my_predictors = my_predictors, 
                             all_sqr_predictions = all_sqr_predictions, 
                             sqr_dataset = sqr_dataset)

write_out_rds(join_all, out_pt, out_name)  
