fit_ranger_RF <- function(dependent_variable, 
                          predictors, 
                          training_dataset, 
                          no_trees, 
                          min_node_size, 
                          my_weights, 
                          model_nm){
  
  wgts <- training_dataset[, my_weights]
  
  train <- training_dataset[, c(dependent_variable, predictors)]
  
  ranger(formula = paste0(dependent_variable, "~ ."),
         data = train,
         num.trees = no_trees,
         case.weights = wgts,
         write.forest = TRUE,
         min.node.size = min_node_size,
         verbose = TRUE)
  
}

make_ranger_predictions <- function(mod_obj, dataset, sel_preds){
  
  x_data <- subset(dataset, , sel_preds, drop = FALSE)
  
  preds <- predict(mod_obj, x_data)
  
  preds$predictions
  
}
