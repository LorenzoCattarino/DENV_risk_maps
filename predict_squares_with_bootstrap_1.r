# Makes foi predictions for all the squares in the world, for each model fit. 

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


model_tp <- "boot_model_20km_2"

no_fits <- 200

RF_mod_name <- "RF_obj_sample"


# ---------------------------------------- are you using the cluster? 


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "20Core")
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

#my_predictors <- c(my_predictors, "RFE_const_term")


# ---------------------------------------- submit one job 


# t <- obj$enqueue(
#   wrapper_to_make_h2o_preds(
#     seq_len(no_fits)[1],
#     RF_mod_name = RF_mod_name,
#     model_in_path = RF_obj_path, 
#     dataset = all_sqr_covariates, 
#     predictors = my_predictors))
  

# ---------------------------------------- submit all jobs


if(CLUSTER){
  
  world_sqr_preds_all_fits <- queuer::qlapply(
    seq_len(no_fits),
    wrapper_to_make_h2o_preds,
    obj,
    RF_mod_name = RF_mod_name,
    model_in_path = RF_obj_path, 
    dataset = all_sqr_covariates, 
    predictors = my_predictors)
  
} else {
  
  world_sqr_preds_all_fits <- lapply(
    seq_len(no_fits)[1],
    wrapper_to_make_h2o_preds,
    RF_mod_name = RF_mod_name,
    model_in_path = RF_obj_path, 
    dataset = all_sqr_covariates, 
    predictors = my_predictors)
  
}
 