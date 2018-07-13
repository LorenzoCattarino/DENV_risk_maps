# Take mean, median, sd and 95% CI of foi, R0 and burden measures, for each 20 km square
# THIS IS FOR THE MAPS!

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
  dependent_variable = "FOI",
  grid_size = 5,
  no_samples = 200)   

vars_to_average <- "response"

model_type_tag <- "_boot_model"


# define variables ------------------------------------------------------------


model_type <- paste0(parameters$dependent_variable, model_type_tag)

my_dir <- paste0("grid_size_", parameters$grid_size)

col_names <- as.character(seq_len(parameters$no_samples))

in_path <- file.path("output", 
                     "predictions_world",
                     "bootstrap_models",
                     my_dir,
                     model_type)


# are you using the cluster? -------------------------------------------------- 


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "24Core")
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
