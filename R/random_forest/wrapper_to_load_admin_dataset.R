wrapper_to_load_admin_dataset <- function(
  i, prediction_datasets, adm_levels, 
  bse_infs, sel_preds, parallel, 
  cut_off, var_names, model_in_path, 
  out_path){
  
  pred_dts <- prediction_datasets[[i]]
  
  adm_lvl <- adm_levels[i]
  
  base_info <- bse_infs[[i]]
  
  file_name <- paste0("adm_", adm_lvl, "_predictions" , ".RDS")
  
  out <- wrapper_to_make_preds(
    dataset = pred_dts, 
    predictors = sel_preds, 
    model_in_path = model_in_path,
    parallel = parallel,
    cut_off = cut_off, 
    base_info = base_info, 
    var_names = var_names)  
  
  dir.create(out_path, FALSE, TRUE)
  
  saveRDS(out, file.path(out_path, file_name))

}
