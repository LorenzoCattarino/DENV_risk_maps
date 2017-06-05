wrapper_to_make_preds <- function(
  dataset, predictors, model_in_path,
  parallel, cut_off, base_info, 
  var_names){
  
  #browser()
  
  model_lst <- readRDS(model_in_path)
  
  # Remove records with at least one NA predictor value
  dataset_2 <- remove_NA_rows(dataset, predictors)
  
  preds_all_models <- make_predictions(
    model_lst,
    dataset = dataset_2,
    sel_preds = predictors)

  #browser()
  
  # preds_all_models <- loop_simplify(
  #   model_lst,
  #   make_predictions,
  #   dataset = dataset_2,
  #   sel_preds = predictors,
  #   parallel = parallel,
  #   what = numeric(nrow(dataset_2)))
  
  preds_all_models[preds_all_models < cut_off] <- 0
  
  # if there is only one pixel in the dataset
  
  if(is.null(dim(preds_all_models))){
    
    mean_pred <- preds_all_models
      
    #mean_pred <- mean(preds_all_models)
    
    #sd_pred <- sd(preds_all_models)
    
    #percentiles <- quantile(preds_all_models, probs = c(0.025, 0.975))
    
    #a <- percentiles[1]
    #b <- percentiles[2]
    
  }else{
    
    mean_pred <- preds_all_models
    
    #mean_pred <- rowMeans(preds_all_models)
    
    #sd_pred <- apply(preds_all_models, 1, FUN = sd)
    
    #percentiles <- apply(preds_all_models, 1, FUN = quantile, probs = c(0.025, 0.975))
    
    #percentiles <- t(percentiles)
    
    #a <- percentiles[, 1]
    #b <- percentiles[, 2]
    
  }
  
  zero_logical <- mean_pred == 0
  
  out <- setNames(data.frame(dataset_2[!zero_logical, base_info],
                             mean_pred[!zero_logical]),
                             #sd_pred[!zero_logical],
                             #a[!zero_logical],
                             #b[!zero_logical]),
                  nm = c(base_info, var_names))
  
  #if(sum(is.na(out$mean_pred)) > 0) stop ("NA predictions")
    
  out  

}
