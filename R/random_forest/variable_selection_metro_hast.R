MH_variable_selection_boot <- function(i, 
                                       boot_ls,
                                       foi_data, 
                                       predictors, 
                                       dependent_variable, 
                                       parms,
                                       out_fig_pth,
                                       out_tab_pth){
  
  
  # ---------------------------------------- define parameters
  
  
  #browser()
  
  no_trees <- parms$no_trees 
  min_node_size <- parms$min_node_size 
  grid_size <- parms$grid_size
  psAb_val <- parms$pseudoAbs_value
  Niter <- parms$it
  scaling_factor <- parms$scaling_factor
  var_scale <- parms$var_scale
  
  diagnostic_nms <- c("iter", 
                      "cur.OF_before", 
                      "next.OF", 
                      "rnd_number", 
                      "MH_prob", 
                      "cur.OF_after", 
                      "accept", 
                      "cur.n.sel")
  
  
  # ---------------------------------------- create empty objects 
  
  
  ID_sample <- i
  
  adm_dts_boot <- boot_ls[[ID_sample]]
  
  adm_dts_boot[adm_dts_boot$type == "pseudoAbsence", dependent_variable] <- psAb_val
  
  n.vars <- length(predictors)
  
  tracker <- matrix(0, nrow = Niter, ncol = length(diagnostic_nms) + n.vars)
  colnames(tracker) <- 1:dim(tracker)[2]
  colnames(tracker) <- c(diagnostic_nms, predictors)
  
  # no_data <- nrow(model_dataset)
  # 
  # # get observations 
  # y.data <- model_dataset[!pseudo_abs_logical, dependent_variable]
  # 
  # # get predictor values 
  # x.data <- model_dataset[!pseudo_abs_logical, predictors, drop = FALSE]
  # 
  # # overlay squared grid on data points 
  # gridded_dataset <- grid_up(model_dataset, grid_size)
  # 
  # # get the cells occupied with at least one data point
  # occupied_cells <- unique(gridded_dataset$cell)
  # 
  # # do bootstrapping and get the full training dataset
  # training_dataset <- do_boostrap(gridded_dataset, occupied_cells)
  # 
  # # get the position (1/0) of the points in the validating dataset
  # valid_point_pos <- get_validating_point_positions(no_data, training_dataset)
  # 
  # # remove pseudo absences
  # valid_point_pos <- valid_point_pos[!pseudo_abs_logical]   
  
  
  # -------------------------------------- first iteration
  
  
  # initial state 
  cur.var.list <- sample(x = 0:1, size = n.vars, replace = TRUE)
  
  cur.n.sel <- sum(cur.var.list)
  
  selected_predictors <- predictors[cur.var.list == 1]
  
  #run core routine
  run_core <- fit_predict_and_error(dataset = adm_dts_boot, 
                                    y_var = dependent_variable, 
                                    my_preds = predictors,
                                    no_trees = no_trees, 
                                    min_node_size = min_node_size,
                                    foi_data = foi_data)

  # extract sum of squared errors
  SS <- run_core["rmse.valid"]
  
  cur.OF <- scaling_factor * SS
  
  
  # -------------------------------------- loop through iterations 
  
  
  for (iter in 1:Niter){
    
    cat("iteration =", iter, "\n")
    
    next.var.list <- cur.var.list
    
    # suggest move
    var.id <- sample(x = n.vars, size = sample(1:3, 1))
    next.var.list[var.id] <- 1 - next.var.list[var.id]
    next.n.sel <- sum(next.var.list)
    
    selected_predictors <- predictors[next.var.list == 1]
    
    # run core routine 
    run_core <- fit_predict_and_error(dataset = adm_dts_boot, 
                                      y_var = dependent_variable, 
                                      my_preds = selected_predictors,
                                      no_trees = no_trees, 
                                      min_node_size = min_node_size,
                                      foi_data = foi_data)
    
    # extract sum of squared errors
    SS <- run_core["rmse.valid"]
    
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
  
  out_tab_name <- paste0("sample_", ID_sample, ".rds")
    
  out_fig_name <- paste0("sample_", ID_sample, ".pdf")
    
  write_out_rds(tracker, out_tab_pth, out_tab_name)
    
  my_path <- file.path(out_fig_pth, paste0("sample_", ID_sample))
  
  plot_MH_var_sel_outputs(tracker, my_path, out_fig_name)
  
  tracker
  
}

fit_predict_and_error <- function(parms,
                                  dataset, 
                                  y_var, 
                                  my_preds,
                                  foi_data) {
  
  train_set <- dataset[, c(y_var, my_preds, "new_weight")]
  
  RF_obj <- fit_ranger_RF(parms = parms,
                          dependent_variable = y_var, 
                          predictors = my_preds, 
                          training_dataset = train_set, 
                          my_weights = "new_weight")
  
  p_i <- make_ranger_predictions(mod_obj = RF_obj, 
                                 dataset = foi_data, 
                                 sel_preds = my_preds)
  
  all_points <- foi_data$data_id
  
  train_points <- dataset$data_id
  
  unique_train_points <- unique(train_points)
  
  valid_points <- all_points[!all_points %in% unique_train_points]
  
  y.data <- foi_data[, y_var] 
  
  my_weights <- foi_data$new_weight
  
  p_i[p_i < 0] <- 0
  
  y.data[y.data < 0] <- 0
  
  rmse.train <- sqrt(weighted.mean((y.data[valid_points] - p_i[valid_points])^2, my_weights[valid_points]))
  rmse.valid <- sqrt(weighted.mean((y.data[train_points] - p_i[train_points])^2, my_weights[train_points]))
  
  c(rmse.train = rmse.train, rmse.valid = rmse.valid)
  
}

plot_MH_var_sel_outputs <- function(data_to_plot, out_path, file_tag){
  
  if(!is.data.frame(data_to_plot)){
    
    data_to_plot <- as.data.frame(data_to_plot)
    
  }
  
  dir.create(out_path, FALSE, TRUE)
  
  # plot and save
  p1 <- ggplot(
    data_to_plot, aes(iter, cur.OF_after)) +
    geom_line() +
    scale_x_continuous("Iterations")
  
  p2 <- ggplot(
    data_to_plot, aes(cur.OF_after)) +
    geom_histogram() + 
    scale_y_continuous("Frequency")
  
  p3 <- grid.arrange(p1, p2)
  
  ggsave(file.path(out_path, file_tag), plot = p3)
  
}
