spatial.cv.rf <- function(
  preds, y_var, train_set, 
  no_trees, min_node_size, x_data, 
  y_data, valid_points, my_weights){
  
  # fit a RF model
  ranger_obj <- fit_RF(
    dependent_variable = y_var, 
    training_dataset = train_set, 
    no_trees = no_trees, 
    min_node_size = min_node_size,
    my_weights = my_weights)
  
  # make predictions
  predictions <- make_predictions(
    mod_obj = ranger_obj, 
    dataset = x_data, 
    sel_preds = preds)
    
  # calculate sum of squared errors
  sse <- calc_SSE(
    y.data = y_data,
    valid.set = valid_points,
    predictions = predictions)
  
  list(obj = ranger_obj,
       predictions = predictions,
       sse = sse)
}
