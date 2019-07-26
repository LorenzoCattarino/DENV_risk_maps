# Load back square predictions for the world, for each model fit 
# Save a n squares x n fits matrix. 

source(file.path("R", "utility_functions.R"))
source(file.path("R", "create_parameter_list.R"))
source(file.path("R", "random_forest", "fit_ranger_RF_and_make_predictions.R"))


# define parameters ----------------------------------------------------------- 


extra_prms <- list(id = 8,
                   dependent_variable = "FOI",
                   base_info = c("cell", 
                                 "latitude", 
                                 "longitude", 
                                 "population", 
                                 "ID_0", 
                                 "ID_1", 
                                 "ID_2")) 

out_fl_nm <- "response.rds"


# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

model_type <- paste0("model_", parameters$id)

var_to_fit <- parameters$dependent_variable

foi_offset <- parameters$foi_offset

base_info <- parameters$base_info

global_predictions_in_path <- file.path("output", 
                                        "predictions_world",
                                        "bootstrap_models",
                                        model_type,
                                        "boot_samples")

out_pt <- file.path("output", 
                    "predictions_world", 
                    "bootstrap_models",
                    model_type)


# load data -------------------------------------------------------------------


all_sqr_covariates <- readRDS(file.path("output", 
                                        "env_variables", 
                                        "all_squares_env_var_0_1667_deg.rds"))


# pre processing -------------------------------------------------------------- 


fi <- list.files(global_predictions_in_path, 
                 pattern = "^sample",
                 full.names = TRUE)

all_samples <- loop(fi, readRDS, parallel = FALSE)


# combine all results together ------------------------------------------------ 


sqr_preds <- do.call("cbind", all_samples)

if(var_to_fit =="FOI"){
  
  sqr_preds <- sqr_preds - foi_offset

}

sqr_preds[sqr_preds < 0] <- 0

sqr_preds <- cbind(all_sqr_covariates[, base_info], sqr_preds)

write_out_rds(sqr_preds, out_pt, out_fl_nm)  
