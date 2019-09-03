load_fit_and_predict <- function(i,
                                 boot_samples,
                                 my_preds,
                                 parms,
                                 foi_data,
                                 out_path) {
  
  base_info <- c("ID_0", "ID_1", "data_id", "type", "FOI", "admin", "train")
  
  y_var <- parms$dependent_variable
  psAb_val <- parms$pseudoAbs_value
  no_trees <- parms$no_trees
  min_node_size <- parms$min_node_size
  
  ID_sample <- i
  
  dataset <- boot_samples[[ID_sample]]
  
  dataset[dataset$type == "pseudoAbsence", y_var] <- psAb_val
  
  train_set <- dataset[, c(y_var, my_preds, "new_weight")]
  
  #browser()
  
  RF_obj <- fit_ranger_RF(dependent_variable = y_var, 
                          predictors = my_preds, 
                          training_dataset = train_set, 
                          no_trees = no_trees, 
                          min_node_size = min_node_size,
                          my_weights = "new_weight")
  
  p_i <- make_ranger_predictions(mod_obj = RF_obj, 
                                 dataset = foi_data, 
                                 sel_preds = my_preds)
  
  p_i[p_i < 0] <- 0
  
  foi_data[, y_var][foi_data[, y_var] < 0] <- 0
  
  foi_data$admin <- p_i
  
  train_points <- dataset$data_id
  
  unique_train_points <- unique(train_points)
  
  train_ids <- rep(0, nrow(foi_data))
  
  train_ids[unique_train_points] <- 1
  
  foi_data$train <- train_ids
  
  ret <- foi_data[, base_info]
  
  out_name <- paste0("predictions_", i, ".rds")
  
  write_out_rds(ret, out_path, out_name)
  
}
