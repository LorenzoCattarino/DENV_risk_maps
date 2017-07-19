fit_h2o_RF <- function(
  dependent_variable, predictors, training_dataset, no_trees, min_node_size, my_weights, model_nm) {
  
  train <- as.h2o(training_dataset)
  
  h2o.randomForest(x = predictors,
                   y = dependent_variable, 
                   training_frame = train, 
                   model_id = model_nm,
                   ntrees = no_trees, 
                   weights_column = my_weights, 
                   max_depth = min_node_size)

}
