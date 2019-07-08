
options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "random_forest", "fit_ranger_RF_and_make_predictions.R"),
  file.path("R", "random_forest", "variable_selection_stepwise.R"),
  file.path("R", "prepare_datasets", "set_pseudo_abs_weights.R"),
  file.path("R", "utility_functions.R"),
  file.path("R", "create_parameter_list.R"))

my_pkgs <- c("ranger", "ggplot2")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters ----------------------------------------------------------- 


extra_prms <- list(var_to_fit = "FOI",
                   no_reps = 10, 
                   addition = TRUE,
                   parallel_2 = TRUE,
                   top_ones_within_reps = 10,
                   top_ones_across_reps = 20,
                   stepwise_exp_id = 5)


# rebuild the queue object? --------------------------------------------------- 


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "24Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  context::parallel_cluster_start(8, ctx)
  
}


# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

my_dir <- paste0("grid_size_", parameters$grid_size)

top_ones_within_reps <- parameters$top_ones_within_reps
top_ones_across_reps <- parameters$top_ones_across_reps

stepwise_exp_dir <- paste0("stepwise_v", parameters$stepwise_exp_id)

table_out_path <- file.path("output", 
                            "variable_selection", 
                            stepwise_exp_dir)

plot_out_path <- file.path("figures", 
                           "variable_selection", 
                           stepwise_exp_dir)


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

# plot data only for the first 10 samples 
lapply(seq_along(bsample_step_addition)[1:10], 
       plot_RMSE_addition, 
       bsample_step_addition, 
       plot_out_path)

all_preds <- lapply(bsample_step_addition, 
                    get_changed_predictors, 
                    nrow(bsample_step_addition[[1]][[1]]))

all_preds_top <- lapply(all_preds, get_top_from_replicates, top_ones_within_reps)
  
# calculate selection frequency of all 26 predictors across replicates
ret1 <- lapply(all_preds_top, calculate_sel_freq, top_ones_across_reps)

no_samples <- parameters$no_samples

# save best predictors from stepwise addition - for each bootstrap sample
lapply(seq(no_samples),
       save_addition_best_preds,
       results = ret1, 
       names = names(boot_samples[[1]]), 
       table_out_path)
  