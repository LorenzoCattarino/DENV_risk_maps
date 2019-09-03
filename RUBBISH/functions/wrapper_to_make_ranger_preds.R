wrapper_to_make_ranger_preds <- function(i, 
                                         model_in_path, 
                                         dataset,
                                         predictors) {
  
  #browser()
  
  cat("bootstrap sample =", i, "\n")
  
  RF_obj_nm <- paste0("sample_", i, ".rds")
  
  RF_obj <- readRDS(file.path(model_in_path, RF_obj_nm))
  
  make_ranger_predictions(RF_obj, dataset, predictors)
  
}

