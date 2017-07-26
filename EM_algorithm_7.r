# Load back in the results of the EM algorithm.
# Get:
# 1) Vector of square-level predictions for the entire 20km dataset
# 2) Vector of positional binary indices indicating whether each square point was in the train set 
# 3) Vector of positional binary indices indicating whether each square point was in the test set

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "utility_functions.R"),
  file.path("R", "random_forest", "Exp_Max_algorithm.R"),
  file.path("R", "random_forest", "make_h2o_RF_predictions.r"),
  file.path("R", "random_forest", "quick_raster_map.r"))  

my_pkgs <- c("h2o", "dplyr", "fields")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# ---------------------------------------- rebuild the queue object?


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "24Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  
}


# ---------------------------------------- define parameters


out_fl_nm <- "square_predictions.rds"
out_pt <- file.path("output", "predictions", "best_model_20km_cw")


# ---------------------------------------- get results 


my_task_id <- "odourless_gull"

EM_alg_run_t <- obj$task_bundle_get(my_task_id)

EM_alg_run <- EM_alg_run_t$results()


# ---------------------------------------- get predictions 


len <- length(EM_alg_run[[1]][[1]])

prediction_sets <- vapply(EM_alg_run, "[[", numeric(len), 1)

out <- data.frame(pred = prediction_sets)

write_out_rds(out, out_pt, out_fl_nm)
