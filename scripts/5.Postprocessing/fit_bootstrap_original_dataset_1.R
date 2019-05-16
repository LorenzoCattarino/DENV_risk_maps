
options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "random_forest", "fit_ranger_RF_and_make_predictions.R"),
  file.path("R", "utility_functions.R"))

my_pkgs <- "ranger"

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters ----------------------------------------------------------- 


parameters <- list(
  dependent_variable = "FOI",
  grid_size = 5,
  no_trees = 500,
  min_node_size = 20,
  pseudoAbs_value = -0.02,
  no_samples = 200)


# define variables ------------------------------------------------------------  


no_samples <- parameters$no_samples

grid_size <- parameters$grid_size

out_path <- file.path("output", "original_dataset_fits", "predictions_data")

my_dir <- paste0("grid_size_", grid_size)


# are you using the cluster? -------------------------------------------------- 


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "20Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  context::parallel_cluster_start(4, ctx)
  
}


# load data -------------------------------------------------------------------


foi_data <- read.csv(file.path("output", 
                               "foi", 
                               "All_FOI_estimates_linear_env_var_area_salje.csv"),
                     stringsAsFactors = FALSE) 

bt_samples <- readRDS(file.path("output", 
                                "EM_algorithm",
                                "bootstrap_models",
                                my_dir, 
                                "bootstrap_samples.rds"))

predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection",
                                     "stepwise",
                                     "predictor_rank.csv"), 
                           stringsAsFactors = FALSE)


# pre process ----------------------------------------------------------------- 


my_predictors <- predictor_rank$name[1:18]


# submit one job --------------------------------------------------------------  


# t <- obj$enqueue(
#   load_fit_and_predict(
#     seq_len(no_samples)[1],
#     boot_samples = bt_samples,
#     my_preds = my_predictors,
#     parms = parameters,
#     foi_data = foi_data,
#     out_path = out_path))


# submit all jobs ------------------------------------------------------------- 


if (CLUSTER) {

  fit_and_predict <- queuer::qlapply(
    seq_len(no_samples),
    load_fit_and_predict,
    obj,
    boot_samples = bt_samples,
    my_preds = my_predictors,
    parms = parameters,
    foi_data = foi_data,
    out_path = out_path)

} else {

  fit_and_predict <- lapply(
    seq_len(no_samples)[1],
    load_fit_and_predict,
    boot_samples = bt_samples,
    my_preds = my_predictors,
    parms = parameters,
    foi_data = foi_data,
    out_path = out_path)

}

if (!CLUSTER) {
  context::parallel_cluster_stop()
}
