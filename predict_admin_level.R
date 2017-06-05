options(didewin.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "utility_functions.R"),
  file.path("R", "random_forest", "make_RF_predictions.R"),
  file.path("R", "random_forest", "wrapper_to_load_admin_dataset.R"),
  file.path("R", "random_forest", "wrapper_to_make_predictions.R"),
  file.path("R", "prepare_datasets", "remove_NA_rows.R"))

my_pkgs <- c("ranger")

context::context_log_start()
ctx <- context::context_save(packages = my_pkgs,
                             sources = my_resources,
                             root = "context")


# ---------------------------------------- are you using the cluster?


if (CLUSTER) {
  
  config <- didewin::didewin_config(template = "24Core")
  obj <- didewin::queue_didewin(ctx, config = config)

} else {
  
  context::context_load(ctx)
  #context::start_parallel_cluster(8, ctx)

}


# ---------------------------------------- define parameters


cut_off <- 0

adm_levels <- c(1, 2)

bse_inf_1 <- c("OBJECTID", "ID_0", "country", "ID_1", "name1", "population")
bse_inf_2 <- c("OBJECTID", "ID_0", "country", "ID_1", "name1", "ID_2", "name2", "population")

var_names <- "mean_pred" #, "sd_pred", "low_perc", "up_perc")

RF_obj_path <- file.path(
  "output",
  "model_objects",
  "best_model_20km_cw.RDS")

out_pth <- file.path(
  "output", 
  "predictions", 
  "best_model_20km_cw")


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
    cut_off = cut_off,
    var_names = var_names, 
    model_in_path = RF_obj_path,
    out_path = out_pth)
  
} else {
  
  all_admin <- lapply(
    seq_along(adm_levels),
    wrapper_to_load_admin_dataset,
    prediction_datasets = prediction_datasets,
    adm_levels = adm_levels, 
    bse_infs = bse_infs, 
    sel_preds = best_predictors, 
    parallel = TRUE,
    cut_off = cut_off,
    var_names = var_names, 
    model_in_path = RF_obj_path,
    out_path = out_pth)
  
}

if (!CLUSTER) {
  context:::stop_parallel_cluster()
}
