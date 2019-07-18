# Filters the global 20 km square dataset based on the original foi dataset 

library(dplyr)

source(file.path("R", "create_parameter_list.R"))
source(file.path("R", "prepare_datasets", "set_pseudo_abs_weights.R"))
source(file.path("R", "utility_functions.R"))


# define parameters -----------------------------------------------------------


extra_prms <- list(id = 15,
                   dependent_variable = "FOI",
                   id_fld = "data_id") 

join_fields <- c("data_id", "ID_0", "ID_1")


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


foi_data[foi_data$type == "pseudoAbsence", var_to_fit] <- pseudoAbs_value

foi_data$new_weight <- all_wgt
pAbs_wgt <- get_sat_area_wgts(foi_data, parameters)
foi_data[foi_data$type == "pseudoAbsence", "new_weight"] <- pAbs_wgt

if(var_to_fit == "FOI" | var_to_fit == "Z"){
  
  foi_data[, var_to_fit] <- foi_data[, var_to_fit] + foi_offset
  
}


# join ------------------------------------------------------------------------


pxl_data_2 <- inner_join(pxl_data, foi_data[, c(join_fields, "type", "new_weight")])

pxl_data_3 <- set_wgts_to_sero_cells(foi_data, pxl_data_2, parameters)

if(length(unique(pxl_data_3$data_id)) != nrow(foi_data)){
  
  stop("Some data points are missing their cell")
  
}


# save outputs ----------------------------------------------------------------


write_out_rds(foi_data, out_pth_1, "adm_foi_data.rds")
write_out_rds(pxl_data_3, out_pth_2, "env_vars_20km.rds")
