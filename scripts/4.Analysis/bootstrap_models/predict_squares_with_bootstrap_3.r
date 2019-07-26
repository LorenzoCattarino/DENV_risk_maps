# Take mean, median, sd and 95% CI of foi, R0 and burden measures, for each 20 km square
# THIS IS FOR THE MAP!

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "utility_functions.R"),
  file.path("R", "create_parameter_list.R"),
  file.path("R", "prepare_datasets", "calculate_mean_across_fits.R"))

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources)


# define parameters ----------------------------------------------------------- 


extra_prms <- list(id = 4) 

vars_to_average <- "response"


# are you using the cluster? -------------------------------------------------- 


if (CLUSTER) {
  
  # config <- didehpc::didehpc_config(template = "24Core")
  obj <- didehpc::queue_didehpc(ctx)
  
} else{
  
  context::context_load(ctx)
  
}


# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

model_type <- paste0("model_", parameters$id)

col_names <- as.character(seq_len(parameters$no_samples))

in_path <- file.path("output", 
                     "predictions_world",
                     "bootstrap_models",
                     model_type)


# run one job -----------------------------------------------------------------


t <- obj$enqueue(
  wrapper_to_average_bsamples(
    seq_along(vars_to_average)[1],
    vars = vars_to_average,
    in_path = in_path,
    out_path = in_path,
    col_names = col_names))
  
  
# run -------------------------------------------------------------------------


# if (CLUSTER) {
# 
#   means_all_scenarios <- queuer::qlapply(
#     seq_along(vars_to_average),
#     wrapper_to_average_bsamples,
#     obj,
#     vars = vars_to_average,
#     in_path = in_path,
#     out_path = in_path,
#     col_names = col_names)
# 
# } else {
# 
#   means_all_scenarios <- loop(
#     seq_along(vars_to_average),
#     wrapper_to_average_bsamples,
#     vars = vars_to_average,
#     in_path = in_path,
#     out_path = in_path,
#     col_names = col_names,
#     parallel = FALSE)
# 
# }
# 
# if(!CLUSTER){
#   context::parallel_cluster_stop()
# }
