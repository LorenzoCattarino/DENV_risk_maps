load_predict_and_save <- function(
  i, pxl_dts_path, RF_obj_path, 
  my_preds, out_file_path, out_file_name){
  
  pxl_dts_nm <- paste0("aggreg_pixel_level_env_vars_20km_sample_", i, ".rds")
  
  RF_obj_nm <- paste0("RF_obj_sample_", i, ".rds")
  
  pxl_dts_boot <- readRDS(file.path(pxl_dts_path, pxl_dts_nm))
  
  RF_obj <- readRDS(file.path(RF_obj_path, RF_obj_nm))
  
  p_i <- make_predictions(
    mod_obj = RF_obj, 
    dataset = pxl_dts_boot, 
    sel_preds = my_preds)
  
  pxl_dts_boot$p_i <- p_i
  
  a <- out_file_name[i]
  
  write_out_rds(pxl_dts_boot, out_file_path, a)
  
}
