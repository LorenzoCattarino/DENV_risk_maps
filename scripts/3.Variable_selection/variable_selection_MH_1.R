
options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "random_forest", "fit_ranger_RF_and_make_predictions.R"),
  file.path("R", "random_forest", "variable_selection_metro_hast.R"),
  file.path("R", "prepare_datasets", "set_pseudo_abs_weights.R"),
  file.path("R", "utility_functions.R"))

my_pkgs <- c("ranger", "ggplot2", "gridExtra")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters ----------------------------------------------------------- 


parameters <- list(
  grid_size = 5,
  no_trees = 500,
  min_node_size = 20,
  pseudoAbs_value = -0.02,
  all_wgt = 1,
  wgt_limits = c(1, 500),
  it = 100000,
  scaling_factor = 10000,
  var_scale = 0,
  no_samples = 200)

no_samples <- parameters$no_samples

var_to_fit <- "FOI"

altitude_var_names <- "altitude"

fourier_transform_elements <- c("const_term",	"Re0",	"Im0",	"Re1",	"Im1")

FTs_data_names <- c("DayTemp", "EVI", "MIR", "NightTemp", "RFE")

extra_predictors <- c("log_pop_den", "travel_time")

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
  
  config <- didehpc::didehpc_config(template = "12and16Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  context::parallel_cluster_start(4, ctx)
  
}


# load data -------------------------------------------------------------------


foi_data <- read.csv(file.path("output", 
                               "foi", 
                               "All_FOI_estimates_linear_env_var_area_salje.csv"),
                     stringsAsFactors = FALSE) 

boot_samples <- readRDS(file.path("output", 
                                  "EM_algorithm",
                                  "bootstrap_models",
                                  my_dir, 
                                  "bootstrap_samples.rds"))


# pre process -----------------------------------------------------------------


all_combs <- expand.grid(FTs_data_names, fourier_transform_elements)

all_FT_names <- paste(all_combs$Var1, all_combs$Var2, sep = "_")

all_predictors <- c(altitude_var_names, all_FT_names, extra_predictors)

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
    seq_len(no_samples),
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
    seq_len(no_samples)[1],
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
