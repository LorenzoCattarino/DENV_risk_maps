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
  out_path, out_fl_nm, no_fits){
  
  foi <- wrapper_to_make_preds(
    no_fits = no_fits, 
    model_in_path = model_in_path, 
    dataset = dat, 
    predictors = sel_preds, 
    parallel = parallel)  
  
  foi[foi < 0] <- 0
  
  write_out_rds(foi, out_path, out_fl_nm)
  
}

get_boot_sample_and_fit_RF <- function(i, boot_ls, y_var, my_preds, no_trees, min_node_size, out_path, psAb_val, all_wgt, pAbs_wgt, pAbs_wgt_AUS) {
  
  adm_dts_boot <- boot_ls[[i]]
  
  adm_dts_boot[adm_dts_boot$type == "pseudoAbsence", y_var] <- psAb_val
  
  adm_dts_boot$new_weight <- all_wgt
  adm_dts_boot[adm_dts_boot$type == "pseudoAbsence", "new_weight"] <- pAbs_wgt
  adm_dts_boot[adm_dts_boot$type == "pseudoAbsence" & adm_dts_boot$ID_0 == 15, "new_weight"] <- pAbs_wgt_AUS
  
  training_dataset <- adm_dts_boot[, c(y_var, my_preds, "new_weight")]
  
  a <- paste0("RF_model_object_", i, ".rds")
  
  h2o.init()
  
  RF_obj <- fit_h2o_RF(dependent_variable = y_var, 
                       predictors = my_preds, 
                       training_dataset = training_dataset, 
                       no_trees = no_trees, 
                       min_node_size = min_node_size,
                       my_weights = "new_weight",
                       model_nm = a)
  
  h2o.saveModel(RF_obj, out_path, force = TRUE)
  
  h2o.shutdown(prompt = FALSE)
  
}

load_predict_and_save <- function(
  i, RF_obj_path, 
  my_preds, no_fits, out_file_path){
  
  #browser()
  
  pxl_dts_path <- file.path("output", "EM_algorithm", "env_variables", "boot_samples")
  
  pxl_dts_nm <- paste0("env_vars_20km_sample_", i, ".rds")
  
  RF_obj_nm <- paste0("RF_model_object_", i, ".rds")
  
  pxl_dts_boot <- readRDS(file.path(pxl_dts_path, pxl_dts_nm))
  
  h2o.init()
  
  # RF_obj <- readRDS(file.path(RF_obj_path, RF_obj_nm))
  RF_obj <- h2o.loadModel(file.path(RF_obj_path, RF_obj_nm))
  
  p_i <- make_h2o_predictions(
    mod_obj = RF_obj, 
    dataset = pxl_dts_boot, 
    sel_preds = my_preds)
  
  pxl_dts_boot$p_i <- p_i
  
  
  # ---------------------------------------- 
  
  
  out_file_name <- paste0("covariates_and_foi_20km_", seq_len(no_fits), ".rds")
  
  a <- out_file_name[i]
  
  write_out_rds(pxl_dts_boot, out_file_path, a)
  
  h2o.shutdown(prompt = FALSE)
  
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
