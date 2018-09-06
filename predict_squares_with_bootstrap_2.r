# Load back square predictions for the world, for each model fit 
# Save a n squares x n fits matrix. 

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "random_forest", "fit_ranger_RF_and_make_predictions.R"),
  file.path("R", "utility_functions.R"))

my_pkgs <- "ranger"

context::context_log_start()
ctx <- context::context_save(path = "context",
                             packages = my_pkgs,
                             sources = my_resources)


# define parameters ----------------------------------------------------------- 


parameters <- list(
  dependent_variable = "FOI",
  foi_offset = 0.03,
  grid_size = 5,
  no_samples = 200,
  no_predictors = 23)   

out_fl_nm <- "response.rds"

base_info <- c("cell", "latitude", "longitude", "population", "ID_0", "ID_1", "ID_2")

model_type_tag <- "_boot_model_22"


# define variables ------------------------------------------------------------


var_to_fit <- parameters$dependent_variable

foi_offset <- parameters$foi_offset

model_type <- paste0(var_to_fit, model_type_tag)

my_dir <- paste0("grid_size_", parameters$grid_size)

out_pt <- file.path("output", 
                    "predictions_world", 
                    "bootstrap_models",
                    my_dir,
                    model_type)


# load data ------------------------------------------------------------------- 


all_sqr_covariates <- readRDS(file.path("output", 
                                        "env_variables", 
                                        "all_squares_env_var_0_1667_deg.rds"))


# rebuild the queue object? --------------------------------------------------- 


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "24Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  
}

# obj$enqueue(install.packages(file.path("R_sources", "h2o_3.18.0.8.tar.gz"), repos=NULL, type="source"))$wait(Inf)


# get results ----------------------------------------------------------------- 


bundles <- obj$task_bundle_info()

my_task_id <- bundles[nrow(bundles), "name"] 

sqr_preds_boot_t <- obj$task_bundle_get(my_task_id)

sqr_preds_boot <- sqr_preds_boot_t$results()


# combine all results together ------------------------------------------------ 


sqr_preds <- do.call("cbind", sqr_preds_boot)

if(var_to_fit =="FOI"){
  
  sqr_preds <- sqr_preds - foi_offset

}

sqr_preds[sqr_preds < 0] <- 0

sqr_preds <- cbind(all_sqr_covariates[, base_info], sqr_preds)

write_out_rds(sqr_preds, out_pt, out_fl_nm)  
