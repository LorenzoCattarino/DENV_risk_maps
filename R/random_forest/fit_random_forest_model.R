fit_RF <- function(
  dependent_variable, training_dataset, no_trees, min_node_size, my_weights) {
  
  frmla <- as.formula(paste(dependent_variable, ".", sep = " ~ "))
  
  RFmodel <- ranger(formula = frmla, 
                    data = training_dataset, 
                    num.trees = no_trees, 
                    case.weights = my_weights, 
                    write.forest = TRUE, 
                    min.node.size = min_node_size,
                    verbose = TRUE)
  
}
