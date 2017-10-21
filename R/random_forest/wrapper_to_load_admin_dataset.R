wrapper_to_load_admin_dataset <- function(
  dat, sel_preds, parallel, 
  model_in_path, 
  out_path, no_fits){
  
  foi <- wrapper_to_make_preds(
    no_fits = no_fits, 
    model_in_path = model_in_path, 
    dataset = dat, 
    predictors = sel_preds, 
    parallel = parallel)  
  
  foi[foi < 0] <- 0
  
  write_out_rds(foi, out_path, file_name)

}
