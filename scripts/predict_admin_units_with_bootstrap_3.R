# Take mean, median, sd and 95% CI of foi, R0 and burden measures, for each 20 km square
# THIS IS FOR THE MAP!

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- FALSE

my_resources <- c(
  file.path("R", "prepare_datasets", "calculate_mean_across_fits.R"),
  file.path("R", "utility_functions.R"))

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources)


# define parameters ----------------------------------------------------------- 


parameters <- list(
  id = 24,
  no_samples = 200) 

vars_to_average <- "response"


# define variables ------------------------------------------------------------


model_type <- paste0("model_", parameters$id)

col_names <- as.character(seq_len(parameters$no_samples))

in_path <- file.path("output", 
                     "predictions_world",
                     "bootstrap_models",
                     model_type,
                     "adm_1")


# are you using the cluster? -------------------------------------------------- 


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "20Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else{
  
  context::context_load(ctx)
  
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
