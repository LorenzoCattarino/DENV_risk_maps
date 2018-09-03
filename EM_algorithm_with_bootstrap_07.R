# Load back in the pxl data predictions from the optimized EM models.
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
  dependent_variable = "FOI",
  grid_size = 5,
  no_samples = 200,
  no_predictors = 9)   

model_type_tag <- "_boot_model_23"

out_fl_nm <- "square_predictions_all_data.rds"


# define variables ------------------------------------------------------------  


var_to_fit <- parameters$dependent_variable

model_type <- paste0(var_to_fit, model_type_tag)

my_dir <- paste0("grid_size_", parameters$grid_size)

out_pt <- file.path("output", "EM_algorithm", "bootstrap_models", my_dir, model_type)


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
