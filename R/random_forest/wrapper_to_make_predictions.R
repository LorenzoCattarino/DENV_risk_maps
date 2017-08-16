wrapper_to_make_preds <- function(
  no_fits, model_in_path, dataset, 
  predictors, parallel){
  
  #browser()
  
  
  # -------------------------------------- start up h2o 
  
  
  h2o.init()
  
  
  # -------------------------------------- loop through model fits
  
  
  out <- loop_simplify(
    seq_len(no_fits),
    function(i){
      RF_obj_nm <- paste0("RF_obj_sample_", i, ".rds")
      RF_obj <- h2o.loadModel(file.path(model_in_path, RF_obj_nm))
      make_h2o_predictions(RF_obj, dataset, predictors)
    },
    parallel = parallel,
    what = numeric(nrow(dataset)))
  
  
  # -------------------------------------- close down h2o 
  
  
  h2o.shutdown(prompt = FALSE)
  
  
  # --------------------------------------
  
  out  

}
