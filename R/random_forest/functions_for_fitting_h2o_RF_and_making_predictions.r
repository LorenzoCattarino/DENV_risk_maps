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

make_h2o_predictions <- function(mod_obj, dataset, sel_preds){
  
  #browser()
  
  x_data <- subset(dataset, , sel_preds, drop = FALSE)
  
  x_data <- as.h2o(x_data)
  
  preds <- predict(mod_obj, x_data)
  
  as.vector(preds)
  
}

wrapper_to_make_preds <- function(
  no_fits, model_in_path, dataset, 
  predictors, parallel){
  
  #browser()
  
  
  # -------------------------------------- start up h2o 
  
  
  h2o.init()
  
  
  # -------------------------------------- loop through model fits
  
  
  out <- loop_simplify(
    seq_len(no_fits),
    function(i){
      RF_obj_nm <- paste0("RF_obj_sample_", i, ".rds")
      RF_obj <- h2o.loadModel(file.path(model_in_path, RF_obj_nm))
      make_h2o_predictions(RF_obj, dataset, predictors)
    },
    parallel = parallel,
    what = numeric(nrow(dataset)))
  
  
  # -------------------------------------- close down h2o 
  
  
  h2o.shutdown(prompt = FALSE)
  
  
  # --------------------------------------
  
  out  

}

wrapper_to_load_admin_dataset <- function(
  dat, sel_preds, parallel, 
  model_in_path, 
  out_path, no_fits){
  
  foi <- wrapper_to_make_preds(
    no_fits = no_fits, 
    model_in_path = model_in_path, 
    dataset = dat, 
    predictors = sel_preds, 
    parallel = parallel)  
  
  foi[foi < 0] <- 0
  
  write_out_rds(foi, out_path, file_name)
  
}
