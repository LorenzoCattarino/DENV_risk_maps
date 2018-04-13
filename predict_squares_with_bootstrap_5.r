# Take mean, median, sd and 95% CI of foi, R0 and burden measures, for each 20 km square
# THIS IS FOR THE MAPS!

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "utility_functions.r"),
  file.path("R", "burden_and_interventions", "calculate_mean_across_fits.r"))

my_pkgs <- NULL

context::context_log_start()
ctx <- context::context_save(path = "context",
                             packages = my_pkgs,
                             sources = my_resources)


# define parameters ----------------------------------------------------------- 


parameters <- list(
  grid_size = 2,
  resample_grid_size = 20,
  no_trees = 500,
  min_node_size = 20,
  pseudoAbs_value = -0.02,
  all_wgt = 1,
  wgt_limits = c(1, 500),
  no_samples = 200,
  EM_iter = 10,
  no_predictors = 9)   

var_to_fit <- "FOI"

vars_to_average <- "response"

base_info <- c("cell", "lat.grid", "long.grid", "population", "ADM_0", "ADM_1", "ADM_2")


# define variables ------------------------------------------------------------


no_samples <- parameters$no_samples

model_type <- paste0(var_to_fit, "_boot_model")

my_dir <- paste0("grid_size_", parameters$grid_size)

col_names <- as.character(seq_len(no_samples))

in_path <- file.path("output", 
                     "predictions_world", 
                     "bootstrap_models")#,
                     # my_dir,
                     # model_type)


# are you using the cluster? -------------------------------------------------- 


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "24Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else{
  
  context::context_load(ctx)
  context::parallel_cluster_start(7, ctx)
  
}


# load data ------------------------------------------------------------------- 


all_sqr_covariates <- readRDS(file.path("output", 
                                        "env_variables", 
                                        "all_squares_env_var_0_1667_deg.rds"))


# run one job -----------------------------------------------------------------


# t <- obj$enqueue(
#   average_foi_and_burden_predictions(
#     seq_along(vars_to_average)[1],
#     vars = vars_to_average,
#     in_path = in_path,
#     out_path = in_path,
#     col_names = col_names,
#     base_info = base_info,
#     covariate_dts = all_sqr_covariates))
  
  
# run -------------------------------------------------------------------------


if (CLUSTER) {

  means_all_scenarios <- queuer::qlapply(
    seq_along(vars_to_average),
    average_foi_and_burden_predictions,
    obj,
    vars = vars_to_average,
    in_path = in_path,
    out_path = in_path,
    col_names = col_names,
    base_info = base_info,
    covariate_dts = all_sqr_covariates)

} else {

  means_all_scenarios <- loop(
    seq_along(vars_to_average),
    average_foi_and_burden_predictions,
    vars = vars_to_average,
    in_path = in_path,
    out_path = in_path,
    col_names = col_names,
    base_info = base_info,
    covariate_dts = all_sqr_covariates,
    parallel = FALSE)

}

if(!CLUSTER){
  context::parallel_cluster_stop()
}
