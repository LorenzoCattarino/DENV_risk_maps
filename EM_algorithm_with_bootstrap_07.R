# Load back in the pxl-level predictions from the optimized EM models.
# Specifically, for each bootstrap sample, get:
#
# 1) Vector of square-level predictions for the entire 20km dataset

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
  no_samples = 200,
  EM_iter = 10) 

out_fl_nm <- "square_predictions_all_data.rds"


# define variables ------------------------------------------------------------  


model_type <- paste0("model_", parameters$id)

var_to_fit <- parameters$dependent_variable

my_dir <- paste0("grid_size_", parameters$grid_size)

out_pt <- file.path("output", "EM_algorithm", "bootstrap_models", model_type)


# rebuild the queue object? --------------------------------------------------- 


if (CLUSTER) {

  config <- didehpc::didehpc_config(template = "20Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)

} else {

  context::context_load(ctx)

}


# get results ----------------------------------------------------------------- 


bundles <- obj$task_bundle_info()

my_task_id <- bundles[nrow(bundles), "name"] 

final_sqr_preds_t <- obj$task_bundle_get(my_task_id)

final_sqr_preds <- final_sqr_preds_t$results()


# combine results ------------------------------------------------------------- 


prediction_sets <- do.call("cbind", final_sqr_preds)

write_out_rds(prediction_sets, out_pt, out_fl_nm)
