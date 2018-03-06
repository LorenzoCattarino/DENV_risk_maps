fit_h2o_RF <- function(dependent_variable, 
                       predictors, 
                       training_dataset, 
                       no_trees, 
                       min_node_size, 
                       my_weights, 
                       model_nm){
  
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

wrapper_to_make_h2o_preds <- function(i, 
                                      RF_mod_name, 
                                      model_in_path, 
                                      dataset,
                                      predictors, 
                                      start_h2o,
                                      shut_h2o) {
  
  #browser()
  
  cat("model fit =", i, "\n")
  
  if(start_h2o) {
    h2o.init()
  }
  
  RF_obj_nm <- paste0(RF_mod_name, "_", i, ".rds")
  
  RF_obj <- h2o.loadModel(file.path(model_in_path, RF_obj_nm))
  
  out <- make_h2o_predictions(RF_obj, dataset, predictors)
  
  out[out < 0] <- 0
  
  if(shut_h2o) {
    h2o.shutdown(prompt = FALSE)
  }
  
  out  

}

get_boot_sample_and_fit_RF <- function(i, 
                                       boot_ls, 
                                       y_var, 
                                       my_preds, 
                                       no_trees, 
                                       min_node_size, 
                                       out_path, 
                                       psAb_val, 
                                       all_wgt, 
                                       wgt_limits,
                                       start_h2o,
                                       shut_h2o) {
  
  adm_dts_boot <- boot_ls[[i]]
  
  adm_dts_boot[adm_dts_boot$type == "pseudoAbsence", y_var] <- psAb_val
  
  adm_dts_boot$new_weight <- all_wgt
  pAbs_wgt <- get_area_scaled_wgts(adm_dts_boot, wgt_limits)
  adm_dts_boot[adm_dts_boot$type == "pseudoAbsence", "new_weight"] <- pAbs_wgt
  
  training_dataset <- adm_dts_boot[, c(y_var, my_preds, "new_weight")]
  
  a <- paste0("RF_model_object_", i, ".rds")
  
  if(start_h2o) {
    h2o.init()
  }
  
  RF_obj <- fit_h2o_RF(dependent_variable = y_var, 
                       predictors = my_preds, 
                       training_dataset = training_dataset, 
                       no_trees = no_trees, 
                       min_node_size = min_node_size,
                       my_weights = "new_weight",
                       model_nm = a)
  
  h2o.saveModel(RF_obj, out_path, force = TRUE)
  
  if(shut_h2o) {
    h2o.shutdown(prompt = FALSE)
  }
  
}

load_predict_and_save <- function(i, 
                                  RF_obj_path, 
                                  my_preds, 
                                  no_fits, 
                                  out_file_path,
                                  in_path,
                                  start_h2o,
                                  shut_h2o){
  
  #browser()
  
  pxl_dts_nm <- paste0("env_vars_20km_", i, ".rds")
  
  RF_obj_nm <- paste0("RF_model_object_", i, ".rds")
  
  pxl_dts_boot <- readRDS(file.path(in_path, pxl_dts_nm))
  
  if(start_h2o) {
    h2o.init()
  }
  
  # RF_obj <- readRDS(file.path(RF_obj_path, RF_obj_nm))
  RF_obj <- h2o.loadModel(file.path(RF_obj_path, RF_obj_nm))
  
  p_i <- make_h2o_predictions(
    mod_obj = RF_obj, 
    dataset = pxl_dts_boot, 
    sel_preds = my_preds)
  
  pxl_dts_boot$p_i <- p_i
  
  
  # ---------------------------------------- 
  
  
  out_file_name <- paste0("env_vars_and_foi_20km_", seq_len(no_fits), ".rds")
  
  a <- out_file_name[i]
  
  write_out_rds(pxl_dts_boot, out_file_path, a)
  
  if(shut_h2o) {
    h2o.shutdown(prompt = FALSE)
  }
  
}

get_area_scaled_wgts <- function(foi_data, wgt_limits){
  
  x <- foi_data[foi_data$type== "pseudoAbsence", "Shape_Area"]
  
  area_limits <- c(min(x), max(x))
  
  y <- rep(0, length(x))
  
  y[which(x==min(x))] <- wgt_limits[1]
  y[which(x==max(x))] <- wgt_limits[2] 
  
  between_lims_ids <- which(y == 0) 
  between_lims <- y[between_lims_ids]
  
  look_up_t <- cbind(x, y) 
  
  interp_wgts <- vapply(look_up_t[between_lims_ids, "x"], 
                        approx_one,
                        numeric(1),
                        a = area_limits,
                        b = wgt_limits)
  
  look_up_t[between_lims_ids, "y"] <- interp_wgts
  
  look_up_t[,"y"]
  
}

approx_one <- function(i, a, b){
  approx(a, b, xout = i)$y
}

attach_pred_different_scale_to_data <- function(i, 
                                                model_path, 
                                                foi_data,
                                                adm_dts, 
                                                predictors, 
                                                all_sqr_preds,
                                                sqr_dts, 
                                                tile_ids,
                                                bt_samples,
                                                out_path,
                                                grp_fields){
  
  
  #browser()
  
  # -------------------------------------- start h2o up
  
  
  h2o.init()
  
  
  # -------------------------------------- define variables
  
  
  RF_obj_nm <- paste0("RF_obj_sample_", i, ".rds")
  
  out_name <- paste0("all_scale_predictions_", i, ".rds")
  
  
  # -------------------------------------- load data
  
  
  RF_obj <- h2o.loadModel(file.path(model_path, RF_obj_nm))
  
  
  # -------------------------------------- process admin predictions
  
  
  names(adm_dts)[names(adm_dts) == "ID_0"] <- grp_fields[1]
  names(adm_dts)[names(adm_dts) == "ID_1"] <- grp_fields[2] 
  
  adm_dts_2 <- remove_NA_rows(adm_dts, predictors)
  
  adm_dts_2$admin <- make_h2o_predictions(RF_obj, adm_dts_2, predictors)
  
  fltr_adm <- inner_join(
    adm_dts_2, 
    foi_data[, grp_fields])
  
  
  # -------------------------------------- process square predictions
  
  
  sqr_preds <- all_sqr_preds[, i]
  
  sqr_dts <- cbind(sqr_dts[, c(grp_fields, "population")],
                   square = sqr_preds)
  
  average_sqr <- average_up(
    pxl_df = sqr_dts,
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
  
  
  # --------------------------------------
  
  
  bt_dts <- bt_samples[[i]]
  
  ids <- unique(bt_dts$data_id)
  
  train_ids <- rep(0, nrow(foi_data))
  
  train_ids[ids] <- 1
  
  join_all$train <- train_ids
  
  
  # -------------------------------------- save 
  
  
  write_out_rds(join_all, out_path, out_name)
  
  
  # -------------------------------------- close h2o down 
  
  
  h2o.shutdown(prompt = FALSE)
  
  
  # --------------------------------------
  
}
