# Makes predictions for all the squares in the world.

library(ranger)

source(file.path("R", "random_forest", "fit_ranger_RF_and_make_predictions.R"))
source(file.path("R", "plotting", "functions_for_plotting_raster_maps.R"))
source(file.path("R", "utility_functions.R"))
source(file.path("R", "create_parameter_list.R"))


# define parameters ----------------------------------------------------------- 


extra_prms <- list(id = 15,
                   dependent_variable = "FOI",
                   no_predictors = 26,
                   base_info = c("cell", 
                                 "latitude", 
                                 "longitude", 
                                 "population", 
                                 "ID_0", 
                                 "ID_1", 
                                 "ID_2"))   

RF_mod_name <- "RF_obj.rds"

out_fl_nm <- "response.rds"


# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

model_type <- paste0("model_", parameters$id)

var_to_fit <- parameters$dependent_variable

foi_offset <- parameters$foi_offset

out_pt <- file.path("output", "predictions_world", "best_fit_models", model_type)
  
base_info <- parameters$base_info

covariates_dir <- parameters$covariates_dir


# load data ------------------------------------------------------------------- 


all_sqr_covariates <- readRDS(file.path("output", 
                                        "env_variables", 
                                        "all_squares_env_var_0_1667_deg.rds"))

RF_obj_path <- file.path("output",
                         "EM_algorithm",
                         "best_fit_models",
                         model_type,
                         "optimized_model_objects")

predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection",
                                     covariates_dir,
                                     "predictor_rank.csv"), 
                           stringsAsFactors = FALSE)


# get best predictor ---------------------------------------------------------- 


my_predictors <- predictor_rank$name[1:parameters$no_predictors]


# submit one job -------------------------------------------------------------- 


RF_obj <- readRDS(file.path(RF_obj_path, RF_mod_name))

p_i <- make_ranger_predictions(RF_obj, all_sqr_covariates, my_predictors)

if(var_to_fit == "FOI"){
  
  p_i <- p_i - foi_offset

}

if(var_to_fit == "Z"){
  
  p_i <- (p_i - foi_offset) * all_sqr_covariates$birth_rate * 35
  
}

p_i[p_i < 0] <- 0

world_sqr_preds <- cbind(all_sqr_covariates[, base_info], best = p_i)

write_out_rds(world_sqr_preds, out_pt, out_fl_nm)  
