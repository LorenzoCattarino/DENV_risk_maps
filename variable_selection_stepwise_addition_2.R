
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


top_ones <- 20

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
                      "stepwise")


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


# load data -------------------------------------------------------------------


boot_samples <- readRDS(file.path("output", 
                                  "EM_algorithm",
                                  "bootstrap_models",
                                  my_dir, 
                                  "bootstrap_samples.rds"))


# get results ----------------------------------------------------------------- 


bundles <- obj$task_bundle_info()

my_task_id <- bundles[nrow(bundles), "name"] 

bsample_step_addition_t <- obj$task_bundle_get(my_task_id)

bsample_step_addition <- bsample_step_addition_t$results()

# get predictors added during the second level of addition 
# by boot sample (list slot) and replicate (matrix columns) 
ret1 <- lapply(bsample_step_addition, get_changed_predictors, parameters$no_steps_L2)

# get the most frequently selected predictors
ret2 <- lapply(ret1, calculate_sel_freq, top_ones)

# save best predictors from stepwise addition - for each bootstrap sample
lapply(seq(no_fits),
       save_addition_best_preds,
       results = ret2, 
       names = names(boot_samples[[1]]), 
       out_path)
  