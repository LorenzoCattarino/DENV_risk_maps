# Makes foi predictions of all squares, for each model fit. 
# Save a n record x n fits matrix. 

options(didehpc.cluster = "fi--didemrchnb")

my_resources <- c(
  file.path("R", "utility_functions.r"),
  file.path("R", "random_forest", "functions_for_fitting_h2o_RF_and_making_predictions.r"))
  
my_pkgs <- "h2o"

context::context_log_start()
ctx <- context::context_save(path = "context",
                             packages = my_pkgs,
                             sources = my_resources)

context::context_load(ctx)


# ---------------------------------------- define parameters


model_tp <- "boot_model_20km_2"

no_fits <- 200

foi_out_fl_nm <- "FOI_all_squares_0_1667_deg.rds"

out_pt <- file.path(
  "output", 
  "predictions_world",
  model_tp)


# ---------------------------------------- load data


all_sqr_covariates <- readRDS(file.path("output", "env_variables", "all_squares_env_var_0_1667_deg.rds"))

RF_obj_path <- file.path(
  "output",
  "EM_algorithm",
  model_tp,
  "optimized_model_objects")

# predicting variable rank
predictor_rank <- read.csv(
  file.path("output", 
            "variable_selection", 
            "metropolis_hastings", 
            "exp_1", 
            "variable_rank_final_fits_exp_1.csv"),
  stringsAsFactors = FALSE)


# ---------------------------------------- get best predictor


my_predictors <- predictor_rank$variable[1:9]

my_predictors <- c(my_predictors, "RFE_const_term")


# ---------------------------------------- make prediction for each square and model fit


foi <- wrapper_to_make_preds(
  no_fits = no_fits,
  model_in_path = RF_obj_path, 
  dataset = all_sqr_covariates, 
  predictors = my_predictors, 
  parallel = FALSE)


# ---------------------------------------- set negative foi to zero 


foi[foi < 0] <- 0


# ---------------------------------------- save all fits foi predictions 


write_out_rds(foi, out_pt, foi_out_fl_nm)
