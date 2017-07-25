spatial.cv.rf <- function(
  preds, y_var, train_set, 
  no_trees, min_node_size, my_weights, 
  model_name){
  
  h2o.init()
  
  # fit a RF model
  RF_obj <- fit_h2o_RF(
    dependent_variable = y_var, 
    predictors = preds, 
    training_dataset = train_set, 
    no_trees = no_trees, 
    min_node_size = min_node_size,
    my_weights = my_weights,
    model_nm = model_name)
  
  # make predictions
  predictions <- make_h2o_predictions(
    mod_obj = RF_obj, 
    dataset = train_set, 
    sel_preds = preds)
    
  # # calculate sum of squared errors
  # sse <- calc_SSE(
  #   y.data = y_data,
  #   valid.set = valid_points,
  #   predictions = predictions)
  
  h2o.shutdown(prompt = FALSE)
  
  predictions

}
