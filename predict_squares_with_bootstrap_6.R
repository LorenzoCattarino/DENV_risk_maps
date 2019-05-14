# Take mean, median, sd and 95% CI of any output of the burden analysis, for each 20 km square
# THIS IS FOR THE MAP!

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "prepare_datasets", "calculate_mean_across_fits.R"),
  file.path("R", "utility_functions.R"))

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources)


# define parameters ----------------------------------------------------------- 


parameters <- list(
  id = 21,
  shape_1 = 0,
  shape_2 = 5,
  shape_3 = 1e6,
  all_wgt = 1,
  dependent_variable = "FOI",
  pseudoAbs_value = -0.02,
  grid_size = 1 / 120,
  no_predictors = 26,
  resample_grid_size = 20,
  foi_offset = 0.03,
  no_trees = 500,
  min_node_size = 20,
  no_samples = 200,
  EM_iter = 10) 

vars_to_average <- "transformed_3_r_wolbachia_4"


# define variables ------------------------------------------------------------


model_type <- paste0("model_", parameters$id)

col_names <- as.character(seq_len(parameters$no_samples))

in_path <- file.path("output", 
                     "predictions_world",
                     "bootstrap_models",
                     model_type)


# are you using the cluster? -------------------------------------------------- 


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "20Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else{
  
  context::context_load(ctx)
  context::parallel_cluster_start(7, ctx)
  
}


# run one job -----------------------------------------------------------------


# t <- obj$enqueue(
#   wrapper_to_average_bsamples(
#     seq_along(vars_to_average)[1],
#     vars = vars_to_average,
#     in_path = in_path,
#     out_path = in_path,
#     col_names = col_names))


# run -------------------------------------------------------------------------


if (CLUSTER) {
  
  means_all_scenarios <- queuer::qlapply(
    seq_along(vars_to_average),
    wrapper_to_average_bsamples,
    obj,
    vars = vars_to_average,
    in_path = in_path,
    out_path = in_path,
    col_names = col_names)
  
} else {
  
  means_all_scenarios <- loop(
    seq_along(vars_to_average),
    wrapper_to_average_bsamples,
    vars = vars_to_average,
    in_path = in_path,
    out_path = in_path,
    col_names = col_names,
    parallel = FALSE)
  
}

if(!CLUSTER){
  context::parallel_cluster_stop()
}
