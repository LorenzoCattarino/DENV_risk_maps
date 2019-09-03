fit_ranger_RF <- function(parms,
                          dependent_variable, 
                          predictors, 
                          training_dataset, 
                          my_weights){
  
  num_threads <- parms$ranger_threads
  min_node_size <- parms$min_node_size
  no_trees <- parms$no_trees
  
  wgts <- training_dataset[, my_weights]
  
  train <- training_dataset[, c(dependent_variable, predictors)]
  
  ranger(formula = paste0(dependent_variable, "~ ."),
         data = train,
         num.trees = no_trees,
         importance = "impurity",
         case.weights = wgts,
         write.forest = TRUE,
         min.node.size = min_node_size,
         verbose = TRUE,
         num.threads = num_threads)
  
}

make_ranger_predictions <- function(mod_obj, dataset, sel_preds){
  
  x_data <- subset(dataset, , sel_preds, drop = FALSE)
  
  preds <- predict(mod_obj, x_data)
  
  preds$predictions
  
}
