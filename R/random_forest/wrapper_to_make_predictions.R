wrapper_to_make_preds <- function(
  dataset, predictors, model_in_path,
  parallel, cut_off, base_info, 
  var_names, no_fits, average){
  
  #browser()
  
  # Remove records with at least one NA predictor value
  dataset_2 <- remove_NA_rows(dataset, predictors)
    
  if (average) {
    
    preds_all_models <- loop_simplify(
    seq_len(no_fits),
    function(i){
      RF_obj_nm <- paste0("RF_obj_sample_", i, ".rds")
      RF_obj <- h2o.loadModel(file.path(model_in_path, RF_obj_nm))
      make_h2o_predictions(RF_obj, dataset_2, predictors)
    },
    parallel = parallel,
    what = numeric(nrow(dataset_2)))
  
  } else {

    RF_obj_nm <- "best_model_20km_cw.rds"
    RF_obj <- h2o.loadModel(file.path(model_in_path, RF_obj_nm))
    preds_all_models <- make_h2o_predictions(RF_obj, dataset_2, predictors)
      
  }
  
  preds_all_models[preds_all_models < cut_off] <- 0
  
  # if there is only one pixel in the dataset
  
  if (average) {
    
    if(is.null(dim(preds_all_models))) {
      
      mean_pred <- mean(preds_all_models)
      
      percentiles <- quantile(preds_all_models, probs = c(0.025, 0.975))
      
      a <- percentiles[1]
      b <- percentiles[2]
      
    } else {
      
      mean_pred <- rowMeans(preds_all_models)
      
      percentiles <- apply(preds_all_models, 1, FUN = quantile, probs = c(0.025, 0.975))
      
      percentiles <- t(percentiles)
      
      a <- percentiles[, 1]
      b <- percentiles[, 2]
      
    }
    
  } else {
    
    mean_pred <- preds_all_models
  
  }
  
  zero_logical <- mean_pred == 0
  
  if (average) {
    
    out <- setNames(data.frame(dataset_2[!zero_logical, base_info],
                             mean_pred[!zero_logical],
                             a[!zero_logical],
                             b[!zero_logical]),
                  nm = c(base_info, var_names))
  } else {
    
    out <- setNames(data.frame(dataset_2[!zero_logical, base_info],
                               mean_pred[!zero_logical]),
                    nm = c(base_info, var_names))
    
  }
  
  if(sum(is.na(out$mean_pred)) > 0) stop ("NA predictions")
    
  out  

}
