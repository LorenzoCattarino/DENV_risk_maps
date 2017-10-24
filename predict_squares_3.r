# Makes foi predictions of all squares, for each model fit. 
# Save a n record x n fits matrix. 

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "utility_functions.r"),
  file.path("R", "random_forest", "functions_for_fitting_h2o_RF_and_making_predictions.r"))
  
my_pkgs <- "h2o"

context::context_log_start()
ctx <- context::context_save(path = "context",
                             packages = my_pkgs,
                             sources = my_resources)


# ---------------------------------------- define parameters


model_tp <- "boot_model_20km_3"

no_fits <- 200

foi_out_fl_nm <- "FOI_all_squares_0_1667_deg.rds"

out_pt <- file.path(
  "output", 
  "predictions_world",
  model_tp)


# ---------------------------------------- are you using the cluster? 


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "24Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  
}


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


if(CLUSTER){
  
  t <- obj$enqueue(
    wrapper_to_load_admin_dataset(
      dat = all_sqr_covariates, 
      sel_preds = my_predictors, 
      parallel = FALSE, 
      model_in_path = RF_obj_path, 
      out_path = out_pt, 
      out_fl_nm = foi_out_fl_nm,
      no_fits = no_fits))
  
} else{
  
  wrapper_to_load_admin_dataset(
    dat = all_sqr_covariates, 
    sel_preds = my_predictors, 
    parallel = FALSE, 
    model_in_path = RF_obj_path, 
    out_path = out_pt, 
    out_fl_nm = foi_out_fl_nm,
    no_fits = no_fits)
  
}
  