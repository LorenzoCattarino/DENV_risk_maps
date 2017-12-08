# Load back square predictions for the world, for each model fit 
# Save a n squares x n fits matrix. 

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "utility_functions.r"),
  file.path("R", "random_forest", "functions_for_fitting_h2o_RF_and_making_predictions.r"))

my_pkgs <- "h2o"

context::context_log_start()
ctx <- context::context_save(path = "context",
                             packages = my_pkgs,
                             sources = my_resources)


# ---------------------------------------- define parameters


model_tp <- "boot_model_20km_2"

out_fl_nm <- "FOI_all_squares.rds"

out_pt <- file.path(
  "output", 
  "predictions_world",
  model_tp)


# ---------------------------------------- rebuild the queue object?


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "24Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  
}


# ---------------------------------------- get results


# loads the LAST task bundle
my_task_id <- obj$task_bundle_info()[nrow(obj$task_bundle_info()), "name"] 

world_sqr_preds_all_fits_t <- obj$task_bundle_get(my_task_id)

world_sqr_preds_all_fits <- world_sqr_preds_all_fits_t$results()


# ---------------------------------------- combine all results together


world_sqr_preds <- do.call("cbind", world_sqr_preds_all_fits)

write_out_rds(world_sqr_preds, out_pt, out_fl_nm)  
