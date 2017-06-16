# Estimates foi for each resampled square, of each 20km resolution bootstrap sample

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "random_forest", "load_predict_and_save.r"),
  file.path("R", "random_forest", "make_RF_predictions.r"),
  file.path("R", "utility_functions.r"))

my_pkgs <- "ranger"

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# ---------------------------------------- are you using the cluster? 


if (CLUSTER) {
  
  obj <- didehpc::queue_didehpc(ctx)
  
} else {
  
  context::context_load(ctx)
  
}


# ---------------------------------------- define parameters


no_fits <- 1

boot_pxl_df_path <- file.path("output", "EM_algorithm", "env_variables", "boot_samples")

RF_obj_path <- file.path("output", "EM_algorithm", "model_objects", "boot_samples")

out_pt <- file.path("output", "EM_algorithm", "env_variables_foi", "boot_samples")

out_fl_nm_all <- paste0("All_FOI_estimates_disaggreg_20km_sample_", seq_len(no_fits), ".rds")


# ---------------------------------------- load data


predictor_rank <- read.csv(
  file.path("output", 
            "variable_selection", 
            "metropolis_hastings", 
            "exp_1", 
            "variable_rank_final_fits_exp_1.csv"),
  stringsAsFactors = FALSE)


# ---------------------------------------- pre processing


my_predictors <- predictor_rank$variable[1:9]


# ---------------------------------------- submit one job 


# t <- obj$enqueue(
#   load_predict_and_save(
#     seq_len(no_fits),
#     pxl_dts_path = boot_pxl_df_path, 
#     RF_obj_path = RF_obj_path, 
#     my_preds = my_predictors, 
#     out_file_path = out_pt, 
#     out_file_name = out_fl_nm_all))


# ---------------------------------------- submit all jobs


if (CLUSTER) {

  initial_square_preds <- queuer::qlapply(
    seq_len(no_fits),
    load_predict_and_save,
    obj,
    pxl_dts_path = boot_pxl_df_path,
    RF_obj_path = RF_obj_path,
    my_preds = my_predictors,
    out_file_path = out_pt,
    out_file_name = out_fl_nm_all)

}else{

  initial_square_preds <- lapply(
    seq_len(no_fits),
    load_predict_and_save,
    pxl_dts_path = boot_pxl_df_path,
    RF_obj_path = RF_obj_path,
    my_preds = my_predictors,
    out_file_path = out_pt,
    out_file_name = out_fl_nm_all)

}
