wrapper_to_load_admin_dataset <- function(
  i, prediction_datasets, adm_levels, 
  bse_infs, sel_preds, parallel, 
  var_names, model_in_path, 
  out_path, no_fits){
  
  pred_dts <- prediction_datasets[[i]]
  
  pred_dts <- remove_NA_rows(pred_dts, sel_preds)
    
  adm_lvl <- adm_levels[i]
  
  base_info <- bse_infs[[i]]
  
  file_name <- paste0("adm_", adm_lvl, "_predictions" , ".csv")
  
  foi <- wrapper_to_make_preds(
    no_fits = no_fits, 
    model_in_path = model_in_path, 
    dataset = pred_dts, 
    predictors = sel_preds, 
    parallel = parallel)  
  
  foi[foi < 0] <- 0
  
  ret <- mean_across_fits(foi)
  
  col_nms <- paste0("foi", "_", colnames(ret))
  
  av_df <- setNames(ret, col_nms)
  
  out <- cbind(pred_dts[, base_info], av_df)
  
  zero_logic <- out$foi_mean == 0
  
  out_mz <- out[!zero_logic, ] 
  
  write_out_csv(out_mz, out_path, file_name)

}
