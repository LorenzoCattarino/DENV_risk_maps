# Makes foi predictions of all level 1 admin units, for each model fit. 
# Save a n record x n fits matrix. 

options(didehpc.cluster = "fi--didemrchnb")

my_resources <- c(
  file.path("R", "utility_functions.R"),
  file.path("R", "random_forest", "functions_for_fitting_h2o_RF_and_making_predictions.r"))

my_pkgs <- "h2o"

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)

context::context_load(ctx)


# ---------------------------------------- define parameters


adm_level <- 1

model_tp <- "boot_model_20km_2"

no_fits <- 200

foi_out_fl_nm <- paste0("FOI_all_adm_", adm_level, ".rds")

bse_inf_1 <- c("OBJECTID", "ID_0", "country", "ID_1", "name1", "population")
bse_inf_2 <- c("OBJECTID", "ID_0", "country", "ID_1", "name1", "ID_2", "name2", "population")

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

# dataset for predictions 
prediction_datasets <- lapply(c(1, 2), function(x){
  read.csv(file.path("output", 
                     "env_variables", 
                     paste0("All_adm", x, "_env_var.csv")))})


# ---------------------------------------- pre processing 


# get the vector of best predictors (from MH variable selection routine)
my_predictors <- predictor_rank$variable[1:9]

my_predictors <- c(my_predictors, "RFE_const_term")

bse_infs <- list(bse_inf_1, bse_inf_2)


# ---------------------------------------- make prediction for each square and model fit


foi <- wrapper_to_make_preds(
  no_fits = no_fits,
  model_in_path = RF_obj_path, 
  dataset = prediction_datasets[[adm_level]], 
  predictors = my_predictors, 
  parallel = FALSE)


# ---------------------------------------- set negative foi to zero 


foi[foi < 0] <- 0


# ---------------------------------------- save all fits foi predictions 


write_out_rds(foi, out_pt, foi_out_fl_nm)


# # ---------------------------------------- submit jobs
# 
# 
# if (CLUSTER) {
#   
#   all_admin <- queuer::qlapply(
#     seq_along(adm_levels), 
#     wrapper_to_load_admin_dataset, 
#     obj,
#     prediction_datasets = prediction_datasets,
#     adm_levels = adm_levels, 
#     bse_infs = bse_infs, 
#     sel_preds = my_predictors, 
#     parallel = FALSE,
#     model_in_path = RF_obj_path,
#     out_path = out_pth,
#     no_fits = no_fits)
#   
# } else {
#   
#   all_admin <- lapply(
#     seq_along(adm_levels)[2],
#     wrapper_to_load_admin_dataset,
#     prediction_datasets = prediction_datasets,
#     adm_levels = adm_levels, 
#     bse_infs = bse_infs, 
#     sel_preds = my_predictors, 
#     parallel = FALSE,
#     model_in_path = RF_obj_path,
#     out_path = out_pth,
#     no_fits = no_fits)
#   
# }
# 
# if (!CLUSTER) {
#   context::parallel_cluster_stop()
# }
