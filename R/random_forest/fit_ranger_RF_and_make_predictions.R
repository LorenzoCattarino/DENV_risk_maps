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

fit_predict_and_error <- function(dataset, 
                                  y_var, 
                                  my_preds,
                                  no_trees, 
                                  min_node_size,
                                  foi_data) {
  
  train_set <- dataset[, c(y_var, my_preds, "new_weight")]
  
  RF_obj <- fit_ranger_RF(dependent_variable = y_var, 
                          predictors = my_preds, 
                          training_dataset = train_set, 
                          no_trees = no_trees, 
                          min_node_size = min_node_size,
                          my_weights = "new_weight")
  
  p_i <- make_ranger_predictions(mod_obj = RF_obj, 
                                 dataset = foi_data, 
                                 sel_preds = my_preds)
  
  all_points <- foi_data$data_id
  
  train_points <- dataset$data_id
  
  unique_train_points <- unique(train_points)
  
  valid_points <- all_points[!all_points %in% unique_train_points]
  
  y.data <- foi_data$FOI 
  
  my_weights <- foi_data$new_weight
  
  p_i[p_i < 0] <- 0
  
  y.data[y.data < 0] <- 0
  
  rmse.train <- sqrt(weighted.mean((y.data[valid_points] - p_i[valid_points])^2, my_weights[valid_points]))
  rmse.valid <- sqrt(weighted.mean((y.data[train_points] - p_i[train_points])^2, my_weights[train_points]))
  
  c(rmse.train = rmse.train, rmse.valid = rmse.valid)
  
}
