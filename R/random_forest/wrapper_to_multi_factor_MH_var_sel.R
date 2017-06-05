wrapper_to_multi_factor_MH_var_sel <- function(
  x, model_dataset, predictors, dependent_variable, 
  no_trees, min_node_size, grid_size, pseudoAbs_value, 
  Niter, var_scale){
  
  
  # ---------------------------------------- get parameters
  
  
  exp_id <- x$exp_ID
  cat("exp ID =", exp_id, "\n")
  
  run_id <- x$run_ID
  cat("run ID =", run_id, "\n")
  
  scaling_factor <- x$scaling_factor
  cat("scaling factor =", scaling_factor, "\n")
  

  # ---------------------------------------- define parameters 

  
  diagnostic_nms <- c(
    "iter", 
    "cur.OF_before", 
    "next.OF", 
    "rnd_number", 
    "MH_prob", 
    "cur.OF_after", 
    "accept", 
    "cur.n.sel")
  
  
  # ---------------------------------------- create empty objects 
  
  
  n.vars <- length(predictors)
  
  tracker <- setNames(matrix(0, nrow = Niter, ncol = length(diagnostic_nms) + n.vars), 
                      nm = c(diagnostic_nms, predictors))
  

  # ---------------------------------------- pre processing
  
  
  no_data <- nrow(model_dataset)
  
  # find pseudo absences
  pseudo_abs_logical <- model_dataset$type == "pseudoAbsence"
  
  # set pseudo absence value 
  model_dataset[model_dataset$type == "pseudoAbsence", dependent_variable] <- pseudoAbs_value

  # get observations 
  y.data <- model_dataset[!pseudo_abs_logical, dependent_variable]
  
  # get predictor values 
  x.data <- model_dataset[!pseudo_abs_logical, predictors, drop = FALSE]
  
  # overlay squared grid on data points 
  gridded_dataset <- grid_up(model_dataset, grid_size)
  
  # get the cells occupied with at least one data point
  occupied_cells <- unique(gridded_dataset$cell)
  
  # do bootstrapping and get the full training dataset
  training_dataset <- do_boostrap(gridded_dataset, occupied_cells)

  # get the position (1/0) of the points in the validating dataset
  valid_point_pos <- get_validating_point_positions(no_data, training_dataset)
  
  # remove pseudo absences
  valid_point_pos <- valid_point_pos[!pseudo_abs_logical]   
  
  
  # -------------------------------------- first iteration
  
  
  # initial state 
  cur.var.list <- sample(x = 0:1, size = n.vars, replace = TRUE)
  
  cur.n.sel <- sum(cur.var.list)
  
  selected_predictors <- predictors[cur.var.list == 1]
  
  #run core routine
  run_core <- spatial.cv.rf(
    preds = selected_predictors, 
    y_var = dependent_variable, 
    train_set = training_dataset, 
    no_trees = no_trees, 
    min_node_size = min_node_size,
    x_data = x.data, 
    y_data = y.data, 
    valid_points = valid_point_pos)
  
  # extract sum of squared errors
  SS <- run_core[[3]]
  
  cur.OF <- scaling_factor * SS
  
  
  # -------------------------------------- loop through iterations 
  
  
  for (iter in 1:Niter){
    
    next.var.list <- cur.var.list
    
    # suggest move
    var.id <- sample(x = n.vars, size = sample(1:3, 1))
    next.var.list[var.id] <- 1 - next.var.list[var.id]
    next.n.sel <- sum(next.var.list)
    
    selected_predictors <- predictors[next.var.list == 1]
    
    # run core routine 
    run_core <- spatial.cv.rf(
      preds = selected_predictors, 
      y_var = dependent_variable, 
      train_set = training_dataset, 
      no_trees = no_trees, 
      min_node_size = min_node_size,
      x_data = x.data, 
      y_data = y.data, 
      valid_points = valid_point_pos)
    
    # extract sum of squared errors
    SS <- run_core[[3]]
    
    next.OF <- scaling_factor * SS
    
    rnd_number <- runif(1) 
    
    MH_prob <- min(exp((cur.OF - next.OF + var_scale * (cur.n.sel - next.n.sel))), 1)
    
    tracker[iter, "cur.OF_before"] <- cur.OF
    tracker[iter, "MH_prob"] <- MH_prob
    
    if (rnd_number < MH_prob) {
      
      cur.var.list <- next.var.list
      cur.n.sel <- next.n.sel
      cur.OF <- next.OF
      tracker[iter, "accept"] <- 1
    }
    
    tracker[iter, "cur.OF_after"] <- cur.OF
    
    tracker[iter, "iter"] <- iter
    tracker[iter, "rnd_number"] <- rnd_number
    tracker[iter, "next.OF"] <- next.OF
    tracker[iter, "cur.n.sel"] <- cur.n.sel
    tracker[iter, predictors] <- cur.var.list
    
  }
  
  # write out the chain 
  write_out_rds (tracker, run_id, exp_id)
    
  # save plot of chain and frequency distribution 
  plot_MH_var_sel_outputs(tracker, run_id, exp_id)
  
  tracker
  
}
