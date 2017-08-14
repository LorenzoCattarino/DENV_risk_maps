make_predictions <- function(mod_obj, dataset, sel_preds){
  
  #cat("OOB error =", mod_obj$prediction.error, "\n")
  
  #cat("R2 =", mod_obj$r.squared, "\n")
  
  #browser()
  
  x_data <- subset(dataset,, sel_preds, drop = FALSE)
  
  prediction_run <- predict(mod_obj, x_data)
  
  preds <- prediction_run$predictions
  
  preds [preds < 0] <- 0
  
  preds
}
