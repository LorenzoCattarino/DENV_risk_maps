# Load back square predictions for the world, for each model fit 
# Save a n squares x n fits matrix. 
# Take mean and sd of the predictions 

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "prepare_datasets", "calculate_mean_across_fits.R"),
  file.path("R", "utility_functions.R"))

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources)


# define parameters ----------------------------------------------------------- 


parameters <- list(
  shape_1 = 0,
  shape_2 = 5,
  shape_3 = 1e6,
  all_wgt = 1,
  resample_grid_size = 20,
  foi_offset = 0.03,
  no_trees = 500,
  min_node_size = 20,
  no_samples = 200,
  EM_iter = 10) 


# start up the cluster --------------------------------------------------------


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "12Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  context::parallel_cluster_start(7, ctx)
  
}


# load data -------------------------------------------------------------------


bootstrap_experiments <- read.csv(file.path("output", 
                                            "EM_algorithm", 
                                            "bootstrap_models", 
                                            "boostrap_fit_experiments.csv"),
                                  stringsAsFactors = FALSE)

all_sqr_covariates <- readRDS(file.path("output", 
                                        "env_variables", 
                                        "all_squares_env_var_0_1667_deg.rds"))


# pre processing --------------------------------------------------------------


bootstrap_experiments_by <- dplyr::group_by(bootstrap_experiments, exp_id, var, gs, no_pred)

bootstrap_experiments_uni <- dplyr::summarise_at(bootstrap_experiments_by, "rep_id", min)

boot_exp_ls <- df_to_list(bootstrap_experiments_uni, TRUE)


# run -------------------------------------------------------------------------


if (CLUSTER){
  
  multi_combine_and_mean <- queuer::qlapply(
    boot_exp_ls,
    combine_predictions_and_average,
    obj,
    parameters,
    all_sqr_covariates)
    
} else {
  
  multi_combine_and_mean <- combine_predictions_and_average(
    boot_exp_ls[[1]],
    parameters,
    all_sqr_covariates)
  
}

if (!CLUSTER){
  
  context::parallel_cluster_stop()

}
