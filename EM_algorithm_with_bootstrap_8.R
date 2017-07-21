# Load back in the results of the EM algorithm.
# Specifically, for each bootstrap sample, get:
# 1) Vector of square-level predictions for the entire 20km dataset
# 2) Vector of positional binary indices indicating whether each square point was in the train set 
# 3) Vector of positional binary indices indicating whether each square point was in the test set

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "random_forest", "wrapper_to_Exp_Max_algorithm.r"),
  file.path("R", "random_forest", "get_1_0_point_position.r"),
  file.path("R", "random_forest", "fit_random_forest_model.r"),
  file.path("R", "random_forest", "make_h2o_RF_predictions.r"),
  file.path("R", "random_forest", "Exp_Max_algorithm.r"),
  file.path("R", "random_forest", "quick_raster_map.r"),
  file.path("R", "utility_functions.r"))

my_pkgs <- c("h2o", "dplyr", "fields")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# ---------------------------------------- rebuild the queue object?


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "12and16Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  
}


# ---------------------------------------- define parameters



# ---------------------------------------- get results 


my_task_id <- "odourless_gull"

EM_alg_run_t <- obj$task_bundle_get(my_task_id)

EM_alg_run <- EM_alg_run_t$results()


# ---------------------------------------- get mean predictions for train and test set 


len <- length(EM_alg_run[[1]][[1]])
  
prediction_sets <- vapply(EM_alg_run, "[[", numeric(len), 1)

#train_sets <- vapply(EM_alg_run, "[[", numeric(len), 2)
#test_sets <- vapply(EM_alg_run, "[[", numeric(len), 3)



# ===================================================================
# ===================================================================

# recorded train and test sets are wrong - you have to recalculate them

full_pxl_dts <- readRDS(file.path("output", "EM_algorithm", "env_variables", "aggreg_pixel_level_env_vars_20km.rds"))

no_squares <- nrow(full_pxl_dts)

no_fits <- 200

out_mat <- matrix(0, nrow = no_squares, ncol = no_fits)

for (i in seq_len(no_fits)){
  
  pxl_dts_nm <- paste0("All_FOI_estimates_disaggreg_20km_sample_", i, ".rds")
  
  pxl_dts_boot <- readRDS(file.path("output", "EM_algorithm", "env_variables_foi", "boot_samples", pxl_dts_nm))
  
  test <- semi_join(full_pxl_dts, pxl_dts_boot, by = c("data_id", "latitude", "longitude"))
  
  ids <- test$square[order(test$square)]
  
  train_sets <-  rep(0, no_squares)
  
  train_sets[ids] <- 1  
  
  test_sets <- 1 - train_sets
  
}

# ===================================================================
# ===================================================================



train_sets_n <- rowSums(train_sets)
test_sets_n <- rowSums(test_sets)

mean_prediction_train <- rowSums(prediction_sets * train_sets) / train_sets_n
mean_prediction_test <- rowSums(prediction_sets * test_sets) / test_sets_n

corr.coeff.train <- wtd.cors(y.data, mean.prediction.train, my_weights)
corr.coeff.valid <- wtd.cors(y.data, mean.prediction.valid, my_weights)  
