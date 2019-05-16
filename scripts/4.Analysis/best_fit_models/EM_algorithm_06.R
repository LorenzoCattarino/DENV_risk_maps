# Load back in from the context folder the ranger model object 
# Make square-level predictions for the entire 20km dataset

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- FALSE

my_resources <- c(
  file.path("R", "random_forest", "fit_ranger_RF_and_make_predictions.R"),
  file.path("R", "prepare_datasets", "set_pseudo_abs_weights.R"),
  file.path("R", "random_forest", "exp_max_algorithm.R"),
  file.path("R", "plotting", "functions_for_plotting_raster_maps.R"),
  file.path("R", "plotting", "generic_scatter_plot.R"),
  file.path("R", "prepare_datasets", "calculate_wgt_corr.R"),
  file.path("R", "utility_functions.R"))  

my_pkgs <- c("ranger", "dplyr", "fields", "ggplot2", "weights", "colorRamps")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters ----------------------------------------------------------- 


parameters <- list(
  id = 1,
  dependent_variable = "FOI",
  pseudoAbs_value = -0.02,
  no_predictors = 26,
  resample_grid_size = 20,
  shape_1 = 0,
  shape_2 = 5,
  shape_3 = 1.6e6,
  foi_offset = 0.03,
  no_trees = 500,
  min_node_size = 20,
  ranger_threds = NULL,
  all_wgt = 1,
  EM_iter = 10) 

extra_predictors <- NULL


# define variables ------------------------------------------------------------


model_id <- parameters$id

var_to_fit <- parameters$dependent_variable

number_of_predictors <- parameters$no_predictors

model_type <- paste0("model_", model_id)

out_fl_nm <- "square_predictions_all_data.rds"

out_pt <- file.path("output", 
                    "EM_algorithm", 
                    "best_fit_models", 
                    model_type)

RF_out_pth <- file.path("output", 
                        "EM_algorithm", 
                        "best_fit_models",
                        model_type,
                        "optimized_model_objects")


# rebuild the queue object? --------------------------------------------------- 


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "16Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  
}


# load data -------------------------------------------------------------------


pxl_data_covariates <- readRDS(file.path("output",
                                         "EM_algorithm",
                                         "best_fit_models",
                                         "env_variables",
                                         "env_vars_20km_2.rds"))

predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection",
                                     "stepwise_v3",
                                     "predictor_rank.csv"), 
                           stringsAsFactors = FALSE)


# pre processing --------------------------------------------------------------


my_predictors <- predictor_rank$name[1:number_of_predictors]

my_predictors <- c(my_predictors, extra_predictors)


# get results ----------------------------------------------------------------- 


if(CLUSTER){
  
  all_tasks <- obj$task_times()
  
  # loads the LAST task
  my_task_id <- all_tasks[nrow(all_tasks), "task_id"]
  
  EM_alg_run_t <- obj$task_get(my_task_id)
  
  RF_obj <- EM_alg_run_t$result()
  
} else {
  
  RF_obj <- readRDS(file.path(RF_out_pth, "RF_obj.rds"))
  
}


# make predictions ------------------------------------------------------------


prediction_set <- make_ranger_predictions(RF_obj, pxl_data_covariates, my_predictors)
  
  
# save ------------------------------------------------------------------------


write_out_rds(prediction_set, out_pt, out_fl_nm)
