# Load back in the results of the EM algorithm.
# Specifically, for each bootstrap sample, get:
# 1) Vector of square-level predictions for the entire 20km dataset

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "utility_functions.r"),
  file.path("R", "random_forest", "wrapper_to_Exp_Max_algorithm.r"),
  file.path("R", "random_forest", "fit_h2o_random_forest_model.r"),
  file.path("R", "random_forest", "make_h2o_RF_predictions.r"),
  file.path("R", "random_forest", "Exp_Max_algorithm.r"),
  file.path("R", "random_forest", "quick_raster_map.r"),
  file.path("R", "random_forest", "get_lm_equation.r"),
  file.path("R", "generic_scatter_plot.r"))

my_pkgs <- c("h2o", "dplyr", "fields", "ggplot2")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# ---------------------------------------- define parameters


model_type <- "boot_model_20km_cw"

out_fl_nm <- "square_predictions_all_data.rds"
out_pt <- file.path("output", "predictions_world", model_type)


# ---------------------------------------- rebuild the queue object?


if (CLUSTER) {

  config <- didehpc::didehpc_config(template = "12and16Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)

} else {

  context::context_load(ctx)

}


# ---------------------------------------- get results


my_task_id <- "garish_senegalpython"

EM_alg_run_t <- obj$task_bundle_get(my_task_id)

EM_alg_run <- EM_alg_run_t$results()


# ---------------------------------------- combine all results together


prediction_sets <- do.call("cbind", EM_alg_run)

write_out_rds(prediction_sets, out_pt, out_fl_nm)
