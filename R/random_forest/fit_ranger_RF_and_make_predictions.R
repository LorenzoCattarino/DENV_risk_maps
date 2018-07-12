fit_ranger_RF <- function(dependent_variable, 
                          predictors, 
                          training_dataset, 
                          no_trees, 
                          min_node_size, 
                          my_weights){
  
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

get_boot_sample_and_fit_RF <- function(i, 
                                       parms,
                                       boot_ls, 
                                       my_preds, 
                                       out_path) {
  
  # browser()
  
  no_trees <- parms$no_trees
  min_node_size <- parms$min_node_size
  psAb_val <- parms$pseudoAbs_value
  y_var <- parms$dependent_variable
  foi_offset <- parms$foi_offset
    
  a <- paste0("RF_model_object_", i, ".rds")
  
  adm_dts_boot <- boot_ls[[i]]
  
  adm_dts_boot[adm_dts_boot$type == "pseudoAbsence", y_var] <- psAb_val
  
  if(y_var == "FOI"){
    
    adm_dts_boot[, y_var] <- adm_dts_boot[, y_var] + foi_offset
    
  }
  
  training_dataset <- adm_dts_boot[, c(y_var, my_preds, "new_weight")]
  
  RF_obj <- fit_ranger_RF(dependent_variable = y_var, 
                       predictors = my_preds, 
                       training_dataset = training_dataset, 
                       no_trees = no_trees, 
                       min_node_size = min_node_size,
                       my_weights = "new_weight")
  
  write_out_rds(RF_obj, out_path, a)
  
}

load_predict_and_save <- function(i, 
                                  RF_obj_path, 
                                  my_preds, 
                                  out_file_path,
                                  in_path){
  
  #browser()
  
  pxl_dts_nm <- paste0("env_vars_20km_", i, ".rds")
  
  RF_obj_nm <- paste0("RF_model_object_", i, ".rds")
  
  pxl_dts_boot <- readRDS(file.path(in_path, pxl_dts_nm))
  
  RF_obj <- readRDS(file.path(RF_obj_path, RF_obj_nm))
  
  p_i <- make_ranger_predictions(
    mod_obj = RF_obj, 
    dataset = pxl_dts_boot, 
    sel_preds = my_preds)
  
  pxl_dts_boot$p_i <- p_i
  
  
  # ---------------------------------------- 
  
  
  out_file_name <- paste0("env_vars_and_foi_20km_", i, ".rds")
  
  write_out_rds(pxl_dts_boot, out_file_path, out_file_name)
  
}
