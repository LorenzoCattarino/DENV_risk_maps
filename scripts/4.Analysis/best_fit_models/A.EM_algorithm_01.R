# Filters the global 20 km square dataset based on the original foi dataset 

library(dplyr)

source(file.path("R", "utility_functions.R"))
source(file.path("R", "create_parameter_list.R"))
source(file.path("R", "prepare_datasets", "set_pseudo_abs_weights.R"))
source(file.path("R", "random_forest", "preprocess.R"))


# define parameters -----------------------------------------------------------


extra_prms <- list(id = 3,
                   dependent_variable = "FOI",
                   id_fld = "data_id",
                   grp_flds = c("data_id", "ID_0", "ID_1"))


# define variable -------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

model_id <- parameters$id

model_type <- paste0("model_", model_id)

var_to_fit <- parameters$dependent_variable

all_wgt <- parameters$all_wgt

foi_offset <- parameters$foi_offset

pseudoAbs_value <- parameters$pseudoAbs_value[var_to_fit]

out_pth_1 <- file.path("output", 
                       "EM_algorithm", 
                       "best_fit_models", 
                       model_type,
                       "adm_foi_data")

out_pth_2 <- file.path("output", 
                       "EM_algorithm", 
                       "best_fit_models", 
                       model_type,
                       "env_variables")


# load data -------------------------------------------------------------------


foi_data <- read.csv(file.path("output", 
                               "foi", 
                               "All_FOI_estimates_and_predictors.csv"),
                     stringsAsFactors = FALSE) 

pxl_data <- readRDS(file.path("output", 
                              "env_variables", 
                              "all_squares_env_var_0_1667_deg.rds"))


# pre processing --------------------------------------------------------------


foi_data_2 <- preprocess_adm_data(parameters, foi_data)

pxl_data_2 <- preprocess_pxl_data(parameters, foi_data_2, pxl_data)


# save outputs ----------------------------------------------------------------


write_out_rds(foi_data_2, out_pth_1, "adm_foi_data.rds")
write_out_rds(pxl_data_2, out_pth_2, "env_vars_20km.rds")
