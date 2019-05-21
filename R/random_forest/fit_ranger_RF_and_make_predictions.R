fit_ranger_RF <- function(parms,
                          dependent_variable, 
                          predictors, 
                          training_dataset, 
                          my_weights){
  
  num_threads <- parms$ranger_threads
  min_node_size <- parms$min_node_size
  no_trees <- parms$no_trees
  
  wgts <- training_dataset[, my_weights]
  
  train <- training_dataset[, c(dependent_variable, predictors)]
  
  ranger(formula = paste0(dependent_variable, "~ ."),
         data = train,
         num.trees = no_trees,
         importance = "impurity",
         case.weights = wgts,
         write.forest = TRUE,
         min.node.size = min_node_size,
         verbose = TRUE,
         num.threads = num_threads)
  
}

make_ranger_predictions <- function(mod_obj, dataset, sel_preds){
  
  x_data <- subset(dataset, , sel_preds, drop = FALSE)
  
  preds <- predict(mod_obj, x_data)
  
  preds$predictions
  
}

wrapper_to_make_ranger_preds <- function(i, 
                                         model_in_path, 
                                         dataset,
                                         predictors) {
  
  #browser()
  
  cat("bootstrap sample =", i, "\n")
  
  RF_obj_nm <- paste0("sample_", i, ".rds")
  
  RF_obj <- readRDS(file.path(model_in_path, RF_obj_nm))
  
  make_ranger_predictions(RF_obj, dataset, predictors)
  
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
    
  aa <- paste0("sample_", i, ".rds")
  
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
  
  write_out_rds(RF_obj, out_path, aa)
  
}

load_predict_and_save <- function(i, 
                                  RF_obj_path, 
                                  my_preds, 
                                  out_path,
                                  in_path){

  pxl_dts_nm <- paste0("sample_", i, ".rds")
  
  RF_obj_nm <- paste0("sample_", i, ".rds")
  
  pxl_dts_boot <- readRDS(file.path(in_path, pxl_dts_nm))
  
  RF_obj <- readRDS(file.path(RF_obj_path, RF_obj_nm))
  
  p_i <- make_ranger_predictions(RF_obj, pxl_dts_boot, my_preds)
  
  pxl_dts_boot$p_i <- p_i
    
  out_file_name <- paste0("sample_", i, ".rds")
  
  write_out_rds(pxl_dts_boot, out_path, out_file_name)
  
}

attach_pred_different_scale_to_data <- function(i, 
                                                model_path, 
                                                foi_data,
                                                adm_dts, 
                                                predictors, 
                                                data_sqr_predictions_in_path,
                                                sqr_dts, 
                                                tile_ids,
                                                bt_samples,
                                                out_path,
                                                grp_fields,
                                                parms){
  
  
  # browser()
  
  
  # -------------------------------------- define variables
  
  
  var_to_fit <- parms$dependent_variable
  foi_offset <- parms$foi_offset
  
  RF_obj_nm <- paste0("sample_", i, ".rds")
  
  out_name <- paste0("sample_", i, ".rds")
  
  
  # -------------------------------------- load data
  
  
  RF_obj <- readRDS(file.path(model_path, RF_obj_nm))
  
  sqr_preds <- readRDS(file.path(data_sqr_predictions_in_path, RF_obj_nm))
  
  
  # -------------------------------------- process admin predictions
  
  
  adm_dts_2 <- remove_NA_rows(adm_dts, predictors)
  
  adm_pred <- make_ranger_predictions(RF_obj, adm_dts_2, predictors)
  
  if(var_to_fit == "FOI"){
    
    adm_pred <- adm_pred - foi_offset 
    sqr_preds <- sqr_preds - foi_offset
    
  }
  
  adm_dts_2$admin <- adm_pred
  
  fltr_adm <- inner_join(adm_dts_2, foi_data[, grp_fields])
  
  
  # -------------------------------------- process square predictions
  
  
  sqr_dts_2 <- cbind(sqr_dts, square = sqr_preds)
  
  average_sqr <- average_up(pxl_df = sqr_dts_2,
                            grp_flds = grp_fields,
                            var_names = "square")
  
  
  # -------------------------------------- process 1 km predictions
  
  
  # #[c(140, 141, 170, 171)]
  # 
  # tile_prds <- loop(
  #   seq_along(tile_ids),
  #   load_predict_filter,
  #   ids_vec = tile_ids,
  #   predictors = predictors,
  #   RF_obj = RF_obj,
  #   foi_dts = foi_data,
  #   grp_flds = grp_fields,
  #   parallel = FALSE)
  # 
  # tile_prds_rb <- do.call("rbind", tile_prds)
  # 
  # average_pxl <- average_up(
  #   pxl_df = tile_prds_rb,
  #   grp_flds = grp_fields,
  #   var_names = "pred")
  # 
  # names(average_pxl)[names(average_pxl) == "pred"] <- "mean_pxl_pred"
  
  
  # -------------------------------------- join admin, square and pixel level predictions
  
  
  df_lst <- list(foi_data[, c(grp_fields, "type", "o_j")],
                 fltr_adm[, c(grp_fields, "admin")],
                 average_sqr[, c(grp_fields, "square")])#,
  #average_pxl[, c(grp_fields, "mean_pxl_pred")]) 
  
  join_all <- Reduce(function(...) left_join(...), df_lst)
  
  join_all[join_all$type == "serology", "square"] <- sqr_dts_2[sqr_dts_2$type == "serology" & sqr_dts_2$new_weight == 1, "square"]
  
  
  # --------------------------------------
  
  
  bt_dts <- bt_samples[[i]]
  
  ids <- unique(bt_dts$data_id)
  
  train_ids <- rep(0, nrow(foi_data))
  
  train_ids[ids] <- 1
  
  join_all$train <- train_ids
  
  
  # -------------------------------------- save 
  
  
  write_out_rds(join_all, out_path, out_name)
  

}
