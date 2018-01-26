# Makes foi predictions of all level 1 admin units in the world, for each model fit. 
# Save a n admin units x n fits matrix. 

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(file.path("R", "utility_functions.R"),
                  file.path("R", "random_forest", "functions_for_fitting_h2o_RF_and_making_predictions.r"))

my_pkgs <- "h2o"

context::context_log_start()
ctx <- context::context_save(path = "context",
                             packages = my_pkgs,
                             sources = my_resources)


# define parameters ----------------------------------------------------------- 


var_to_fit <- "R0_3"

fit_type <- "boot"

model_tp <- paste0(var_to_fit, "_", fit_type, "_model")

RF_mod_name <- "RF_obj_sample"

no_fits <- 200

RF_obj_path <- file.path(
  "output",
  "EM_algorithm",
  model_tp,
  "optimized_model_objects")


# are you using the cluster? --------------------------------------------------  


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "20Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  
}


# load data ------------------------------------------------------------------- 


predictor_rank <- read.csv(
  file.path("output", 
            "variable_selection", 
            "metropolis_hastings", 
            "exp_1", 
            "variable_rank_final_fits_exp_1.csv"),
  stringsAsFactors = FALSE)

prediction_datasets <- read.csv(
  file.path("output", 
            "env_variables", 
            "All_adm1_env_var.csv"))


# pre processing --------------------------------------------------------------  


my_predictors <- predictor_rank$variable[1:9]


# submit all jobs -------------------------------------------------------------


if(CLUSTER){
  
  world_sqr_preds_all_fits <- queuer::qlapply(
    seq_len(no_fits),
    wrapper_to_make_h2o_preds,
    obj,
    RF_mod_name = RF_mod_name, 
    model_in_path = RF_obj_path, 
    dataset = prediction_datasets, 
    predictors = my_predictors, 
    start_h2o = TRUE,
    shut_h2o = TRUE)

} else {
  
  world_sqr_preds_all_fits <-lapply(
    seq_len(no_fits)[1],
    wrapper_to_make_h2o_preds,
    RF_mod_name = RF_mod_name, 
    model_in_path = RF_obj_path, 
    dataset = prediction_datasets, 
    predictors = my_predictors, 
    start_h2o = TRUE,
    shut_h2o = TRUE)
    
}
  