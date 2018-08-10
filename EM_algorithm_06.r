# Load back in from the context folder the ranger model object 
# Make square-level predictions for the entire 20km dataset

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "random_forest", "fit_ranger_RF_and_make_predictions.R"),
  file.path("R", "prepare_datasets", "set_pseudo_abs_weights.R"),
  file.path("R", "random_forest", "exp_max_algorithm.R"),
  file.path("R", "plotting", "quick_raster_map.R"),
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
  dependent_variable = "FOI",
  shape_1 = 0,
  shape_2 = 5,
  shape_3 = 1.6e6,
  pseudoAbs_value = -0.02,
  foi_offset = 0.03,
  no_trees = 500,
  min_node_size = 20,
  all_wgt = 1,
  wgt_limits = c(1, 500),
  EM_iter = 10,
  no_predictors = 26) 

model_type_tag <- "_best_model_5"

extra_predictors <- NULL


# define variables ------------------------------------------------------------


var_to_fit <- parameters$dependent_variable

number_of_predictors <- parameters$no_predictors

model_type <- paste0(var_to_fit, model_type_tag)

out_fl_nm <- "square_predictions_all_data.rds"

out_pt <- file.path("output", "EM_algorithm", "best_fit_models", model_type)


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
                              paste0("env_variables_", var_to_fit, "_fit"), 
                              "covariates_and_foi_20km_2.rds"))

predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection",
                                     "stepwise",
                                     "predictor_rank.csv"), 
                           stringsAsFactors = FALSE)


# pre processing --------------------------------------------------------------


my_predictors <- predictor_rank$name[1:number_of_predictors]

my_predictors <- c(my_predictors, extra_predictors)


# get results ----------------------------------------------------------------- 


all_tasks <- obj$task_times()

# loads the LAST task
my_task_id <- all_tasks[nrow(all_tasks), "task_id"]

EM_alg_run_t <- obj$task_get(my_task_id)

RF_obj <- EM_alg_run_t$result()

prediction_set <- make_ranger_predictions(RF_obj, pxl_data_covariates, my_predictors)
  
  
# save ------------------------------------------------------------------------


write_out_rds(prediction_set, out_pt, out_fl_nm)
