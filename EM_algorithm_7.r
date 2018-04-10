# Load back in, from the EM algorithm, a vector of square-level predictions for the entire 20km dataset

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "utility_functions.R"),
  file.path("R", "random_forest", "fit_h2o_RF_and_make_predictions.r"),
  file.path("R", "random_forest", "exp_max_algorithm.R"),
  file.path("R", "plotting", "quick_raster_map.r"),
  file.path("R", "plotting", "generic_scatter_plot.r"))  

my_pkgs <- c("h2o", "dplyr", "fields", "ggplot2")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters ----------------------------------------------------------- 


var_to_fit <- "FOI"


# define variables ------------------------------------------------------------


model_type <- paste0(var_to_fit, "_best_model")

out_fl_nm <- "square_predictions_all_data.rds"

out_pt <- file.path("output", "EM_algorithm", "best_fit_models", model_type)


# rebuild the queue object? --------------------------------------------------- 


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "24Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  
}


# get results ----------------------------------------------------------------- 


all_tasks <- obj$task_times()

# loads the LAST task
my_task_id <- all_tasks[nrow(all_tasks), "task_id"]

EM_alg_run_t <- obj$task_get(my_task_id)

prediction_set <- EM_alg_run_t$result()


# save ------------------------------------------------------------------------


write_out_rds(prediction_set, out_pt, out_fl_nm)
