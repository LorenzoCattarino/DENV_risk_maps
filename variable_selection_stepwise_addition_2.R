
options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "random_forest", "fit_h2o_RF_and_make_predictions.r"),
  file.path("R", "random_forest", "variable_selection_stepwise.R"),
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
  no_steps_L1 = 26, 
  no_steps_L2 = 0,   
  pseudoAbs_value = -0.02,
  all_wgt = 1,
  wgt_limits = c(1, 500),
  no_reps = 10,
  no_samples = 200)     

top_ones_within_reps <- 10
top_ones_across_reps <- 20

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

all_preds <- lapply(bsample_step_addition, get_changed_predictors, parameters$no_steps_L1)

all_preds_top <- lapply(all_preds, get_top_from_replicates, top_ones_within_reps)
  
# calculate selection frequency of all 26 predictors across replicates
ret1 <- lapply(all_preds_top, calculate_sel_freq, top_ones_across_reps)

no_samples <- parameters$no_samples

# save best predictors from stepwise addition - for each bootstrap sample
lapply(seq(no_samples),
       save_addition_best_preds,
       results = ret1, 
       names = names(boot_samples[[1]]), 
       out_path)
  