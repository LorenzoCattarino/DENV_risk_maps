# Load back in the results of the EM algorithm.
# Specifically, for each bootstrap sample, get:
#
# 1) Vector of square-level predictions for the entire 20km dataset

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "utility_functions.r"),
  file.path("R", "random_forest", "wrapper_to_Exp_Max_algorithm.r"),
  file.path("R", "random_forest", "functions_for_fitting_h2o_RF_and_making_predictions.r"),
  file.path("R", "random_forest", "Exp_Max_algorithm.r"),
  file.path("R", "random_forest", "get_lm_equation.r"),
  file.path("R", "plotting", "quick_raster_map.r"),
  file.path("R", "plotting", "generic_scatter_plot.r"))

my_pkgs <- c("h2o", "dplyr", "fields", "ggplot2")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# ---------------------------------------- define parameters


model_type <- "boot_model_20km_2"

out_fl_nm <- "square_predictions_all_data.rds"


# ---------------------------------------- define variables 


out_pt <- file.path("output", "EM_algorithm", model_type)


# ---------------------------------------- rebuild the queue object?


if (CLUSTER) {

  config <- didehpc::didehpc_config(template = "12and16Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)

} else {

  context::context_load(ctx)

}


# ---------------------------------------- get results


# loads the LAST task bundle
my_task_id <- obj$task_bundle_info()[nrow(obj$task_bundle_info()), "name"] 

EM_alg_run_t <- obj$task_bundle_get(my_task_id)

EM_alg_run <- EM_alg_run_t$results()


# ---------------------------------------- combine all results together


prediction_sets <- do.call("cbind", EM_alg_run)

write_out_rds(prediction_sets, out_pt, out_fl_nm)
