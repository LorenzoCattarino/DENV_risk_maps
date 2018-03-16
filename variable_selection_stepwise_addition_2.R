
options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "random_forest", "functions_for_fitting_h2o_RF_and_making_predictions.r"),
  file.path("R", "random_forest", "stepwise_variable_addition_removal.R"),
  file.path("R", "prepare_datasets", "set_pseudo_abs_weights.R"),
  file.path("R", "utility_functions.R"))

my_pkgs <- "h2o"

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters ----------------------------------------------------------- 


parameters <- list(
  grid_size = 1,
  no_trees = 500,
  min_node_size = 20,
  no_steps_L1 = 20,   # 20
  no_steps_L2 = 10,   # 10
  pseudoAbs.value = -0.02,
  all_wgt = 1,
  wgt_limits = c(1, 500),
  no_reps = 10)       # 10

no_fits <- 50

var_to_fit <- "FOI"

altitude_var_names <- "altitude"

fourier_transform_elements <- c("const_term",	"Re0",	"Im0",	"Re1",	"Im1")

FTs_data_names <- c("DayTemp", "EVI", "MIR", "NightTemp", "RFE")

out_path <- file.path("output", 
                      "variable_selection", 
                      "stepwise",
                      "addition")


# define variables ------------------------------------------------------------


my_dir <- paste0("grid_size_", parameters$grid_size)


# rebuild the queue object? --------------------------------------------------- 


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "20Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  context::parallel_cluster_start(8, ctx)
  
}


# get results ----------------------------------------------------------------- 


bundles <- obj$task_bundle_info()

my_task_id <- bundles[nrow(bundles), "name"] 

bsample_step_addition_t <- obj$task_bundle_get(my_task_id)

bsample_step_addition <- bsample_step_addition_t$results()





rm(list=ls())

# load packages
library(context)
library(queuer)
library(didewin)
library(ggplot2)

#### Rebuild original queue from variable addition job #### 
root <- "context"
ctx <- context::context_save(packages=c("ranger", "weights"),
                             sources=c(file.path("R", "random_forest", "spatial_sampK_cv_rng2_stratif_pred.R"), 
                                       file.path("R", "random_forest", "RF_variable_importance_test", "RF_variable_importance_test_common_functions.R"),
                                       file.path("R", "random_forest", "RF_variable_importance_test", "multi_steps_wrapper_DideParallel.R"),
                                       file.path("R", "random_forest", "RF_variable_importance_test", "stepwise_RF_variable_addition", "stepwise_RF_variable_addition_functions.R")), 
                             root=root)
obj <- didewin::queue_didewin(ctx)

stepwise_addition_task_bundle_id <- "viewable_urva"
stepwise_addition_task_bundle <- obj$task_bundle_get(stepwise_addition_task_bundle_id)

# get stepwise addition results
stepwise_addition_results <- stepwise_addition_task_bundle$results()

### you really need to store labels of factors used (e.g., grid size) in your outputs 
stepwise_addition_results_gs_1 <- stepwise_addition_results[1:10]
stepwise_addition_results_gs_5 <- stepwise_addition_results[11:20]
stepwise_addition_results_gs_10 <- stepwise_addition_results[21:30]

# get all predictors added during all stepwise addition replicates - by grid size 
all_predictors_stepwise_addition_gs_1 <- sapply(stepwise_addition_results_gs_1, function(x) { x[[2]][,"changed_predictor"] } )
all_predictors_stepwise_addition_gs_5 <- sapply(stepwise_addition_results_gs_5, function(x) { x[[2]][,"changed_predictor"] } )
all_predictors_stepwise_addition_gs_10 <- sapply(stepwise_addition_results_gs_10, function(x) { x[[2]][,"changed_predictor"] } )

# get the most frequently selected predictors - by grid size
sel_freq_sorted.predictors_gs_1 <- sorting.sel.freq (all_predictors_stepwise_addition_gs_1, 20)
sel_freq_sorted.predictors_gs_5 <- sorting.sel.freq (all_predictors_stepwise_addition_gs_5, 20)
sel_freq_sorted.predictors_gs_10 <- sorting.sel.freq (all_predictors_stepwise_addition_gs_10, 20)

#### Create new queue ####
didewin::didewin_config_global(cluster="fi--didemrchnb", template="16Core")
ctx <- context::context_save(packages=c("ranger", "weights"),
                             sources=c(file.path("R", "random_forest", "spatial_sampK_cv_rng2_stratif_pred.R"), 
                                       file.path("R", "random_forest", "RF_variable_importance_test", "RF_variable_importance_test_common_functions.R"),
                                       file.path("R", "random_forest", "RF_variable_importance_test", "multi_steps_wrapper_DideParallel.R"),
                                       file.path("R", "random_forest", "RF_variable_importance_test", "stepwise_RF_variable_removal", "stepwise_RF_variable_removal_functions.R")), 
                             root=root)
obj <- didewin::queue_didewin(ctx)

# load data
dengue_dataset <- read.csv(file.path("data", "foi", "All_FOI_estimates_env_var.csv")) 

# remove NA and outliers
dengue_dataset <- dengue_dataset[!is.na(dengue_dataset$FOI),]
dengue_dataset <- dengue_dataset[!(dengue_dataset$country=="French Polynesia"|dengue_dataset$country=="Haiti"),]

# calculate weights 
#dengue_dataset$new.weight <- 1/dengue_dataset$variance
dengue_dataset$new.weight <- 1
dengue_dataset[dengue_dataset$FOI==0,"new.weight"] <- 0.25

parameters <- list(
  grid.size = c(1,5,10),
  cell.fraction = 0.7,
  train.fraction = 1,
  pseudoAbs.value = -0.02,
  replicates = 1) 

result_folder <- "removal"

# get factor combinations 
factor_combinations <- get.factor.combinations (parameters)

# vector of folder names 
folder_names_vec <- sprintf("run_%s", factor_combinations$ID.run)

# create folders for data outputs 
sapply(folder_names_vec, function(x) {dir.create(file.path("output", "dengue_dataset", "predictor_importance_test", "removal", x), FALSE, TRUE)})

# submit multiple jobs together 
removal_gs_1 <- queuer::enqueue_bulk(obj, factor_combinations[1,], multi_grid_sizes_wrapper.removal, dengue_dataset, sel_freq_sorted.predictors_gs_1, do.call=TRUE, timeout=0)
removal_gs_5 <- queuer::enqueue_bulk(obj, factor_combinations[2,], multi_grid_sizes_wrapper.removal, dengue_dataset, sel_freq_sorted.predictors_gs_5, do.call=TRUE, timeout=0)
removal_gs_10 <- queuer::enqueue_bulk(obj, factor_combinations[3,], multi_grid_sizes_wrapper.removal, dengue_dataset, sel_freq_sorted.predictors_gs_10, do.call=TRUE, timeout=0)

# check intermediate outputs
#readRDS(file.path("output", "dengue_dataset", "predictor_importance_test", 
#                  "removal", "run_1", 
#                  "predictor_importance_test_step_output_run_1_level_1_step_1.rds"))
