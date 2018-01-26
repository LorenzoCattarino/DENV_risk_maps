# Take mean, sd and 95% CI of foi for each admin unit 1
# THIS IS FOR THE MAPS!

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- FALSE

my_resources <- c(
  file.path("R", "utility_functions.r"),
  file.path("R", "burden_and_interventions", "calculate_mean_across_fits.r"))

my_pkgs <- NULL

context::context_log_start()
ctx <- context::context_save(path = "context",
                             packages = my_pkgs,
                             sources = my_resources)


# define parameters -----------------------------------------------------------


model_tp <- "R0_3_boot_model"

vars <- "FOI" 

scenario_ids <- 1

no_fits <- 200

col_names <- as.character(seq_len(no_fits))

base_info <- c("OBJECTID", "ID_0", "country", "ID_1", "name1", "population")

in_path <- file.path("output", "predictions_world", model_tp)

out_path <- file.path("output", "predictions_world", model_tp)

dts_tag <- "all_adm1"


# are you using the cluster? --------------------------------------------------


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "20Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else{
  
  context::context_load(ctx)
  
}


# load data ------------------------------------------------------------------- 


prediction_datasets <- read.csv(
  file.path("output", 
            "env_variables", 
            "All_adm1_env_var.csv"))


# run ------------------------------------------------------------------------- 


if (CLUSTER) {
  
  mean_foi <- obj$enqueue(
    average_foi_and_burden_predictions(
      seq_along(vars),
      vars = vars,
      in_path = in_path,
      out_path = out_path,
      scenario_ids = scenario_ids,
      col_names = col_names,
      base_info = base_info,
      dts_tag = dts_tag,
      covariate_dts = prediction_datasets))
  
} else {
  
  mean_foi <- average_foi_and_burden_predictions(
    seq_along(vars),
    vars = vars,
    in_path = in_path,
    out_path = out_path,
    scenario_ids = scenario_ids,
    col_names = col_names,
    base_info = base_info,
    dts_tag = dts_tag,
    covariate_dts = prediction_datasets)
  
}
