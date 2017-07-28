# Load back in the results of the EM algorithm.
# Specifically, for each bootstrap sample, get:
# 1) Vector of square-level predictions for the entire 20km dataset


options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "random_forest", "wrapper_to_Exp_Max_algorithm.r"),
  file.path("R", "random_forest", "fit_h2o_random_forest_model.r"),
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


out_fl_nm <- "square_predictions_all_data.rds"
out_pt <- file.path("output", "predictions", "boot_model_20km_cw")


# ---------------------------------------- get results


my_task_id <- "clumsy_mosasaur"

EM_alg_run_t <- obj$task_bundle_get(my_task_id)

EM_alg_run <- EM_alg_run_t$results()


# ---------------------------------------- get mean predictions for train and test set


prediction_sets <- do.call("cbind", EM_alg_run)

write_out_rds(prediction_sets, out_pt, out_fl_nm)


# #train_sets <- vapply(EM_alg_run, "[[", numeric(len), 2)
# #test_sets <- vapply(EM_alg_run, "[[", numeric(len), 3)
# 
# 
# 
# # ===================================================================
# # ===================================================================
# 
# # recorded train and test sets are wrong - you have to recalculate them
# 
# full_pxl_dts <- readRDS(file.path("output", "EM_algorithm", "env_variables", "aggreg_pixel_level_env_vars_20km.rds"))
# 
# no_squares <- nrow(full_pxl_dts)
# 
# no_fits <- 200
# 
# train_sets <- matrix(0, nrow = no_squares, ncol = no_fits)
# 
# for (i in seq_len(no_fits)){
# 
#   pxl_dts_nm <- paste0("All_FOI_estimates_disaggreg_20km_sample_", i, ".rds")
# 
#   pxl_dts_boot <- readRDS(file.path("output", "EM_algorithm", "env_variables_foi", "boot_samples", pxl_dts_nm))
# 
#   test <- semi_join(full_pxl_dts, pxl_dts_boot, by = c("data_id", "latitude", "longitude"))
# 
#   ids <- test$square[order(test$square)]
# 
#   train_sets[ids, i] <- 1
# 
# }
# 
# test_sets <- 1 - train_sets
# 
# # ===================================================================
# # ===================================================================
# 
# 
# 
# train_sets_n <- rowSums(train_sets)
# test_sets_n <- rowSums(test_sets)
# 
# produc_train <- prediction_sets * train_sets
# produc_test <- prediction_sets * test_sets
# 
# mean_prediction_train <- rowSums(produc_train) / train_sets_n
# mean_prediction_test <- rowSums(produc_test) / test_sets_n
# 
# percentiles_train <- t(apply(produc_train, 1, quantile, probs = c(0.025, 0.975)))
# percentiles_test <- t(apply(produc_test, 1, quantile, probs = c(0.025, 0.975)))
# colnames(percentiles_train) <- c("low_perc_train", "up_perc_train")
# colnames(percentiles_test) <- c("low_perc_test", "up_perc_test")
