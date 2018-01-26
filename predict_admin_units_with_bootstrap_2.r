# Load back square predictions for the world, for each model fit 
# Save a n squares x n fits matrix. 

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(file.path("R", "utility_functions.R"),
                  file.path("R", "random_forest", "functions_for_fitting_h2o_RF_and_making_predictions.r"))

my_pkgs <- "h2o"

context::context_log_start()
ctx <- context::context_save(path = "context",
                             packages = my_pkgs,
                             sources = my_resources)


# define parameters ----------------------------------------------------------- 


var_to_fit <- "R0_3"

fit_type <- "boot"

model_tp <- paste0(var_to_fit, "_", fit_type, "_model")

out_fl_nm <- "FOI_all_adm1.rds"

out_pt <- file.path(
  "output", 
  "predictions_world", 
  model_tp)


# are you using the cluster? --------------------------------------------------  


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "20Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  
}


# get results -----------------------------------------------------------------


# loads the LAST task bundle
my_task_id <- obj$task_bundle_info()[nrow(obj$task_bundle_info()), "name"] 

world_sqr_preds_all_fits_t <- obj$task_bundle_get(my_task_id)

world_sqr_preds_all_fits <- world_sqr_preds_all_fits_t$results()


# combine ---------------------------------------------------------------------


world_sqr_preds <- do.call("cbind", world_sqr_preds_all_fits)


# save ------------------------------------------------------------------------


write_out_rds(world_sqr_preds, out_pt, out_fl_nm)

