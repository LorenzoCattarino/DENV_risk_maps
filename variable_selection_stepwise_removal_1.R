
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

out_path <- file.path("output", 
                      "variable_selection", 
                      "stepwise")


# define variables ------------------------------------------------------------


my_dir <- paste0("grid_size_", parameters$grid_size)


# are you using the cluster? -------------------------------------------------- 


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "20Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  context::parallel_cluster_start(8, ctx)
  
}


# load data -------------------------------------------------------------------


foi_data <- read.csv(
  file.path("output", "foi", "All_FOI_estimates_linear_env_var_area.csv"),
  stringsAsFactors = FALSE) 

boot_samples <- readRDS(file.path("output", 
                                  "EM_algorithm",
                                  "bootstrap_models",
                                  my_dir, 
                                  "bootstrap_samples.rds"))


# pre process -----------------------------------------------------------------


foi_data[foi_data$type == "pseudoAbsence", var_to_fit] <- parameters$pseudoAbs.value

foi_data$new_weight <- parameters$all_wgt

pAbs_wgt <- get_area_scaled_wgts(foi_data, parameters$wgt_limits)

foi_data[foi_data$type == "pseudoAbsence", "new_weight"] <- pAbs_wgt


# submit one test job ---------------------------------------------------------


# t <- obj$enqueue(
#   stepwise_removal_boot(
#     seq_len(no_fits)[1],
#     boot_ls = boot_samples,
#     y_var = var_to_fit,
#     parms = parameters,
#     foi_data = foi_data,
#     out_path = out_path,
#     addition = FALSE))


# submit all jobs -------------------------------------------------------------


if (CLUSTER) {

  bsample_step_removal <- queuer::qlapply(
    seq_len(no_fits),
    stepwise_removal_boot,
    obj,
    boot_ls = boot_samples,
    y_var = var_to_fit,
    parms = parameters,
    foi_data = foi_data,
    out_path = out_path,
    addition = FALSE)

} else {

  bsample_step_removal <- lapply(
    seq_len(no_fits)[1],
    stepwise_removal_boot,
    boot_ls = boot_samples,
    y_var = var_to_fit,
    parms = parameters,
    foi_data = foi_data,
    out_path = out_path,
    addition = FALSE)

}

if (!CLUSTER) {
  context::parallel_cluster_stop()
}
