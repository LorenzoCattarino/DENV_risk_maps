
options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- FALSE

my_resources <- c(
  file.path("R", "random_forest", "wrapper_to_multi_factor_MH_var_sel.R"),
  file.path("R", "random_forest", "functions_for_fitting_h2o_RF_and_making_predictions.R"),
  file.path("R", "random_forest", "wrapper_to_multi_factor_MH_var_sel.R"),
  file.path("R", "prepare_datasets", "set_pseudo_abs_weights.R"),
  file.path("R", "utility_functions.R"))

my_pkgs <- c("h2o", "ggplot2", "gridExtra")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters ----------------------------------------------------------- 


parameters <- list(
  grid_size = 1,
  no_trees = 500,
  min_node_size = 20,
  pseudoAbs_value = -0.02,
  all_wgt = 1,
  wgt_limits = c(1, 500),
  it = 10,
  scaling_factor = 10000,
  var_scale = 0)

no_fits <- 50

var_to_fit <- "FOI"

altitude_var_names <- "altitude"

fourier_transform_elements <- c("const_term",	"Re0",	"Im0",	"Re1",	"Im1")

FTs_data_names <- c("DayTemp", "EVI", "MIR", "NightTemp", "RFE")

out_fig_path <- file.path("figures", 
                          "variable_selection", 
                          "metropolis_hastings")

out_tab_path <- file.path("output", 
                          "variable_selection", 
                          "metropolis_hastings")



# define variables ------------------------------------------------------------


my_dir <- paste0("grid_size_", parameters$grid_size)


# are you using the cluster? -------------------------------------------------- 


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "20Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  context::parallel_cluster_start(4, ctx)
  
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


all_FT_names <- apply(expand.grid(fourier_transform_elements, FTs_data_names), 
                      1, 
                      function(x) paste(x[2], x[1], sep="_"))

all_predictors <- c(altitude_var_names, all_FT_names)

foi_data[foi_data$type == "pseudoAbsence", var_to_fit] <- parameters$pseudoAbs_value

foi_data$new_weight <- parameters$all_wgt

pAbs_wgt <- get_area_scaled_wgts(foi_data, parameters$wgt_limits)

foi_data[foi_data$type == "pseudoAbsence", "new_weight"] <- pAbs_wgt


# submit one test job ---------------------------------------------------------


# t <- obj$enqueue(
#   MH_variable_selection_boot(boot_ls = boot_samples,
#                              foi_data = foi_data, 
#                              predictors = all_predictors,
#                              dependent_variable = var_to_fit, 
#                              parms = parameters,
#                              out_fig_pth = out_fig_path,
#                              out_tab_pth = out_tab_path))


# submit all jobs -------------------------------------------------------------


if (CLUSTER) {
  
  MH_chains <- queuer::qlapply(
    seq_len(no_fits),
    MH_variable_selection_boot,
    obj,
    boot_ls = boot_samples,
    foi_data = foi_data, 
    predictors = all_predictors,
    dependent_variable = var_to_fit, 
    parms = parameters,
    out_fig_pth = out_fig_path,
    out_tab_pth = out_tab_path)
  
} else {
  
  MH_chains <- lapply(
    seq_len(no_fits)[1],
    MH_variable_selection_boot,
    boot_ls = boot_samples,
    foi_data = foi_data, 
    predictors = all_predictors,
    dependent_variable = var_to_fit, 
    parms = parameters,
    out_fig_pth = out_fig_path,
    out_tab_pth = out_tab_path)
  
}

if (!CLUSTER) {
  context::parallel_cluster_stop()
}
