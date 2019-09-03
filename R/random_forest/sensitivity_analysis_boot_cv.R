wrapper_to_multi_factor_RF_fit <- function(
  x, model_dataset, predictors, dependent_variable, ranger_out, fit_parallel){


  # ---------------------------------------- get parameters 
  
  
  exp_ID <- x$exp_ID
  cat("ID exp =", exp_ID, "\n")
  
  run_ID <- x$run_ID
  cat("ID run =", run_ID, "\n")

  no_trees <- x$tree_num
  cat("no trees =", no_trees, "\n")
  
  min_node_size <- x$node_size
  cat("minimum node size =", min_node_size, "\n")  
  
  grid_size <- x$grid_size
  cat("grid size =", grid_size, "\n")
  
  pseudoAbs_value <- x$pseudo_absence_value
  cat("pseudo absence value =", pseudoAbs_value, "\n")

  pseudoAbs_prop <- x$pseudo_absence_prop
  cat("pseudo absence proportion =", pseudoAbs_prop, "\n")

  no_fits <- x$runs
  cat("number of fits =", no_fits, "\n")
  
  
  # ---------------------------------------- define parameters 
  

  diagnostic_nms <- c("corr_coeff_train", "corr_coeff_valid", "SS_valid", "no_cells_valid", "av_obj_size") 
  
  
  # ---------------------------------------- create empty objects 
  
  
  diagnostics <- setNames(rep(0, length(diagnostic_nms)), nm = diagnostic_nms)
  
  #browser()
  
  
  # ---------------------------------------- pre processing
  
  
  # find pseudo absences
  pseudo_abs_logical <- model_dataset$type == "pseudoAbsence"
  
  # set pseudo absence value 
  model_dataset[model_dataset$type == "pseudoAbsence", dependent_variable] <- pseudoAbs_value
  
  # get observations 
  y.data <- model_dataset[!pseudo_abs_logical, dependent_variable]
  
  # get predictor values 
  x.data <- model_dataset[!pseudo_abs_logical, predictors, drop = FALSE]
  
  # get case weights 
  my_weights <- model_dataset$new.weight[!pseudo_abs_logical]
  
  len <- sum(!pseudo_abs_logical)
  
  
  # ---------------------------------------- start loop
  
  
  #browser()
  
  res <- loop(seq_len(no_fits), 
              wrapper_to_core_fun, 
              model_dataset = model_dataset, 
              grid_size = grid_size, 
              pseudo_abs_logical = pseudo_abs_logical, 
              predictors = predictors,
              dependent_variable = dependent_variable, 
              no_trees = no_trees, 
              min_node_size = min_node_size, 
              x.data = x.data, 
              y.data = y.data,
              parallel = fit_parallel)
  
  
  # ---------------------------------------- extract results
  
  
  model_obj_lst <- lapply(res, "[[", "obj")
  prediction_sets <- vapply(res, "[[", numeric(len), "predictions")
  error_set <- vapply(res, "[[", numeric(1), "sse")
  cell_set <- vapply(res, "[[", numeric(1), "occupied_cells_valid")
  training_sets <- vapply(res, "[[", numeric(len), "train_point_pos")
  validating_sets <- vapply(res, "[[", numeric(len), "valid_point_pos")

  
  # ---------------------------------------- calculate predictions and diagnostics
  
  
  training_sets_n <- rowSums(training_sets)
  validating_sets_n <- rowSums(validating_sets)
  
  # calculate the mean of the predictions across fits (by dataset)
  mean.prediction.train <- rowSums(prediction_sets * training_sets) / training_sets_n
  mean.prediction.valid <- rowSums(prediction_sets * validating_sets) / validating_sets_n

  # calculate the mean of the SSEs across fits
  mean_sse <- mean(error_set)
  
  # calculate the average number of cells occupied with at least one datapoint  
  mean_occ_cells <- mean(cell_set)
  
  # calculate the correaltion coefficient for train and valid datasets
  corr.coeff.train <- wtd.cors(y.data, mean.prediction.train, my_weights)
  corr.coeff.valid <- wtd.cors(y.data, mean.prediction.valid, my_weights)  

  obj_szs <- sapply(model_obj_lst, object.size)
  
  av_obj_szs <- mean(obj_szs)
  
  diagnostics[] <- c(corr.coeff.train, corr.coeff.valid, mean_sse, mean_occ_cells, av_obj_szs)
    
  obs_preds_df <- data.frame(
    ID_0 = model_dataset$ID_0[!pseudo_abs_logical], 
    ID_1 = model_dataset$ID_1[!pseudo_abs_logical], 
    y.data, 
    train_set = mean.prediction.train, 
    test_set = mean.prediction.valid)
  
  out <- list(diagnostics, obs_preds_df)
  
  if(ranger_out){
    out <- list(diagnostics, model_obj_lst)
  }
  
  out

}

wrapper_to_core_fun <- function(
  i, model_dataset, grid_size, 
  predictors, dependent_variable, 
  no_trees, min_node_size, 
  x_data, y_data){
  
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
  
  # get weights 
  my_weights <- training_dataset$new_weight  
  
  # subset training dataset
  training_dataset <- training_dataset[, c(dependent_variable, predictors)]
  
  
  # ---------------------------------------- run core routine
  
  
  ret <- spatial.cv.rf(
    preds = predictors,
    y_var = dependent_variable, 
    train_set = training_dataset,
    no_trees = no_trees, 
    min_node_size = min_node_size,
    x_data = x_data, 
    y_data = y_data,
    valid_points = valid_point_pos,
    my_weights = my_weights)
  
  ret$train_point_pos <- train_point_pos
  ret$valid_point_pos <- valid_point_pos
  ret$occupied_cells_valid <- length(occupied_cells_valid)
  ret
}

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
