# Load back square predictions for the world, for each model fit 
# Save a n squares x n fits matrix. 

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "random_forest", "fit_ranger_RF_and_make_predictions.R"),
  file.path("R", "utility_functions.R"))

my_pkgs <- "ranger"

context::context_log_start()
ctx <- context::context_save(path = "context",
                             packages = my_pkgs,
                             sources = my_resources)

context::context_load(ctx)


# define parameters ----------------------------------------------------------- 


parameters <- list(
  id = 1,
  shape_1 = 0,
  shape_2 = 5,
  shape_3 = 1e6,
  all_wgt = 1,
  dependent_variable = "FOI",
  pseudoAbs_value = -0.02,
  grid_size = 1 / 120,
  no_predictors = 9,
  resample_grid_size = 20,
  foi_offset = 0.03,
  no_trees = 500,
  min_node_size = 20,
  no_samples = 50,
  EM_iter = 10) 

out_fl_nm <- "response.rds"

base_info <- c("cell", "latitude", "longitude", "population", "ID_0", "ID_1", "ID_2")


# define variables ------------------------------------------------------------


model_type <- paste0("model_", parameters$id)

var_to_fit <- parameters$dependent_variable

foi_offset <- parameters$foi_offset

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
