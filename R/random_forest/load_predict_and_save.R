load_predict_and_save <- function(
  i, RF_obj_path, 
  my_preds, no_fits){
  
  pxl_dts_path <- file.path("output", "EM_algorithm", "env_variables", "boot_samples")
  
  pxl_dts_nm <- paste0("aggreg_pixel_level_env_vars_20km_sample_", i, ".rds")
  
  RF_obj_nm <- paste0("RF_model_object_", i, ".rds")
  
  pxl_dts_boot <- readRDS(file.path(pxl_dts_path, pxl_dts_nm))
  
  h2o.init()

  # RF_obj <- readRDS(file.path(RF_obj_path, RF_obj_nm))
  RF_obj <- h2o.loadModel(file.path(RF_obj_path, RF_obj_nm))
    
  p_i <- make_h2o_predictions(
    mod_obj = RF_obj, 
    dataset = pxl_dts_boot, 
    sel_preds = my_preds)
  
  pxl_dts_boot$p_i <- p_i
  
  
  # ---------------------------------------- 
  
  
  out_file_name <- paste0("covariates_and_foi_20km_", seq_len(no_fits), ".rds")
  
  out_file_path <- file.path("output", "EM_algorithm", "env_variables_foi", "boot_samples")
  
  a <- out_file_name[i]
  
  write_out_rds(pxl_dts_boot, out_file_path, a)
  
  h2o.shutdown(prompt = FALSE)
  
}
