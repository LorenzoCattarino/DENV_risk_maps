
options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "random_forest", "fit_ranger_RF_and_make_predictions.R"),
  file.path("R", "random_forest", "variable_selection_stepwise.R"),
  file.path("R", "prepare_datasets", "set_pseudo_abs_weights.R"),
  file.path("R", "utility_functions.R"),
  file.path("R", "create_parameter_list.R"))

my_pkgs <- "ranger"

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters ----------------------------------------------------------- 


extra_prms <- list(
  var_to_fit = "Z",
  addition = FALSE,
  parallel_2 = TRUE)

out_path <- file.path("output", 
                      "variable_selection", 
                      "stepwise_v4")

altitude_var_names <- "altitude"

fourier_transform_elements <- c("const_term",	"Re0",	"Im0",	"Re1",	"Im1")

FTs_data_names <- c("DayTemp", "EVI", "MIR", "NightTemp", "RFE")

extra_predictors <- "log_pop_den"


# are you using the cluster? -------------------------------------------------- 


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "12and16Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  context::parallel_cluster_start(4, ctx)
  
}


# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

my_dir <- paste0("grid_size_", parameters$grid_size)

var_to_fit <- parameters$var_to_fit

pseudoAbs_value <- parameters$pseudoAbs_value[var_to_fit]


# load data -------------------------------------------------------------------


foi_data <- read.csv(file.path("output", 
                               "foi", 
                               "All_FOI_estimates_and_predictors.csv"),
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

foi_data[foi_data$type == "pseudoAbsence", var_to_fit] <- pseudoAbs_value

foi_data$new_weight <- parameters$all_wgt

pAbs_wgt <- get_sat_area_wgts(foi_data, parameters)

foi_data[foi_data$type == "pseudoAbsence", "new_weight"] <- pAbs_wgt

no_samples <- parameters$no_samples


# submit one test job ---------------------------------------------------------


# t <- obj$enqueue(
#   stepwise_removal_boot(
#     seq_len(no_samples)[1],
#     boot_ls = boot_samples,
#     parms = parameters,
#     predictors = all_predictors,
#     foi_data = foi_data,
#     out_path = out_path))


# submit all jobs -------------------------------------------------------------


if (CLUSTER) {

  bsample_step_removal <- queuer::qlapply(
    seq_len(no_samples),
    stepwise_removal_boot,
    obj,
    boot_ls = boot_samples,
    parms = parameters,
    predictors = all_predictors,
    foi_data = foi_data,
    out_path = out_path)

} else {

  bsample_step_removal <- lapply(
    seq_len(no_samples)[1],
    stepwise_removal_boot,
    boot_ls = boot_samples,
    parms = parameters,
    predictors = all_predictors,
    foi_data = foi_data,
    out_path = out_path)

}

if (!CLUSTER) {
  context::parallel_cluster_stop()
}
