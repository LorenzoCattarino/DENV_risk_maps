wrapper_to_core_fun <- function(
  i, model_dataset, grid_size, 
  pseudo_abs_logical, predictors,
  dependent_variable, no_trees, min_node_size, 
  x.data, y.data){
  
  no_data <- nrow(model_dataset)
  
  # overlay squared grid on data points 
  gridded_dataset <- grid_up(model_dataset, grid_size, rnd_dist = TRUE)
  
  # get the cells occupied with at least one data point
  occupied_cells <- unique(gridded_dataset$cell)
  
  # do bootstrapping and get the full training dataset
  training_dataset <- do_boostrap(gridded_dataset)
  
  # get the cells occupied with at least one training point
  occupied_cells_train <- unique(training_dataset$cell)
  
  # get the cells occupied with at least one validating point
  occupied_cells_valid <- unique(occupied_cells[!occupied_cells %in% occupied_cells_train])
  
  # get the position (1/0) of the points in the training dataset
  train_point_pos <- get_training_point_positions(no_data, training_dataset)
  
  # get the position (1/0) of the points in the validating dataset
  valid_point_pos <- get_validating_point_positions(no_data, training_dataset)
  
  # remove pseudo absences
  train_point_pos <- train_point_pos[!pseudo_abs_logical]
  valid_point_pos <- valid_point_pos[!pseudo_abs_logical]    
  
  # subset training dataset
  training_dataset <- training_dataset[, c(dependent_variable, predictors)]
  
  
  # ---------------------------------------- run core routine
  
  
  ret <- spatial.cv.rf(
    preds = predictors,
    y_var = dependent_variable, 
    train_set = training_dataset,
    no_trees = no_trees, 
    min_node_size = min_node_size,
    x_data = x.data, 
    y_data = y.data,
    valid_points = valid_point_pos)
  
  ret$train_point_pos <- train_point_pos
  ret$valid_point_pos <- valid_point_pos
  ret$occupied_cells_valid <- length(occupied_cells_valid)
  ret
}
