make_h2o_predictions <- function(mod_obj, dataset, sel_preds){
  
  #browser()
  
  x_data <- subset(dataset, , sel_preds, drop = FALSE)
  
  x_data <- as.h2o(x_data)
  
  preds <- predict(mod_obj, x_data)
  
  as.vector(preds)
  
}
