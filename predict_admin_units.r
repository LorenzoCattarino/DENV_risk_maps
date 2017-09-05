options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- FALSE

my_resources <- c(
  file.path("R", "utility_functions.R"),
  file.path("R", "random_forest", "wrapper_to_load_admin_dataset.R"),
  file.path("R", "prepare_datasets", "remove_NA_rows.R"),
  file.path("R", "random_forest", "wrapper_to_make_predictions.R"),
  file.path("R", "random_forest", "make_h2o_RF_predictions.R"),
  file.path("R", "prepare_datasets", "calculate_mean_across_fits.r"))

my_pkgs <- "h2o"

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# ---------------------------------------- are you using the cluster?


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "24Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)

} else {
  
  context::context_load(ctx)
  #context::parallel_cluster_start(8, ctx)

}


# ---------------------------------------- define parameters


adm_levels <- c(1, 2)

model_tp <- "boot_model_20km_cw"

no_fits <- 50

bse_inf_1 <- c("OBJECTID", "ID_0", "country", "ID_1", "name1", "population")
bse_inf_2 <- c("OBJECTID", "ID_0", "country", "ID_1", "name1", "ID_2", "name2", "population")

var_names <- "mean_pred"
# var_names <- c("mean_pred" ,"low_perc", "up_perc")


# ---------------------------------------- define variables


RF_obj_path <- file.path(
  "output",
  "EM_algorithm",
  model_tp,
  "optimized_model_objects")

out_pth <- file.path(
  "output", 
  "predictions_world", 
  model_tp)


# ---------------------------------------- load data


# predicting variable rank
predictor_rank <- read.csv(
  file.path("output", 
            "variable_selection", 
            "metropolis_hastings", 
            "exp_1", 
            "variable_rank_final_fits_exp_1.csv"),
  stringsAsFactors = FALSE)


# ---------------------------------------- pre processing 


# get the vector of best predictors (from MH variable selection routine)
best_predictors <- predictor_rank$variable[1:9]

bse_infs <- list(bse_inf_1, bse_inf_2)

# dataset for predictions 
prediction_datasets <- lapply(adm_levels, function(x){
  read.csv(file.path("output", 
                     "env_variables", 
                     paste0("All_adm", x, "_env_var.csv")))})


# ---------------------------------------- submit jobs


if (CLUSTER) {
  
  all_admin <- queuer::qlapply(
    seq_along(adm_levels), 
    wrapper_to_load_admin_dataset, 
    obj,
    prediction_datasets = prediction_datasets,
    adm_levels = adm_levels, 
    bse_infs = bse_infs, 
    sel_preds = best_predictors, 
    parallel = FALSE,
    var_names = var_names, 
    model_in_path = RF_obj_path,
    out_path = out_pth,
    no_fits = no_fits)
  
} else {
  
  all_admin <- lapply(
    seq_along(adm_levels)[2],
    wrapper_to_load_admin_dataset,
    prediction_datasets = prediction_datasets,
    adm_levels = adm_levels, 
    bse_infs = bse_infs, 
    sel_preds = best_predictors, 
    parallel = FALSE,
    var_names = var_names, 
    model_in_path = RF_obj_path,
    out_path = out_pth,
    no_fits = no_fits)
  
}

if (!CLUSTER) {
  context::parallel_cluster_stop()
}
