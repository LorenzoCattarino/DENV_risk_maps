# Makes predictions for all the squares in the world.

options(didehpc.cluster = "fi--didemrchnb")

my_resources <- c(
  file.path("R", "random_forest", "fit_ranger_RF_and_make_predictions.R"),
  file.path("R", "plotting", "functions_for_plotting_raster_maps.R"),
  file.path("R", "utility_functions.R"))
  
my_pkgs <- "ranger"

context::context_log_start()
ctx <- context::context_save(path = "context",
                             packages = my_pkgs,
                             sources = my_resources)


# define parameters ----------------------------------------------------------- 


parameters <- list(
  dependent_variable = "R0_1",
  foi_offset = 0.03,
  no_predictors = 9)   

RF_mod_name <- "RF_obj.rds"

base_info <- c("cell", "latitude", "longitude", "population", "ID_0", "ID_1", "ID_2")

model_id <- 2

extra_predictors <- NULL


# define variables ------------------------------------------------------------


foi_offset <- parameters$foi_offset

model_type <- paste0("model_", model_id)

out_pt <- file.path("output", "predictions_world", "best_fit_models", model_type)
  
out_fl_nm <- "response.rds"


# are you using the cluster? -------------------------------------------------- 


context::context_load(ctx)


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
                                     "stepwise",
                                     "predictor_rank.csv"), 
                           stringsAsFactors = FALSE)


# get best predictor ---------------------------------------------------------- 


my_predictors <- predictor_rank$name[1:parameters$no_predictors]
my_predictors <- c(my_predictors, extra_predictors)


# submit one job -------------------------------------------------------------- 


RF_obj <- readRDS(file.path(RF_obj_path, RF_mod_name))

p_i <- make_ranger_predictions(RF_obj, all_sqr_covariates, my_predictors)

p_i <- p_i - foi_offset

p_i[p_i < 0] <- 0

world_sqr_preds <- cbind(all_sqr_covariates[, base_info], best = p_i)

write_out_rds(world_sqr_preds, out_pt, out_fl_nm)  
