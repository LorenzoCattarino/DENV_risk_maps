attach_pred_different_scale_to_data <- function(
  i, model_path, foi_data,
  adm_dts, predictors, all_sqr_preds,
  sqr_dts, tile_ids,
  bt_samples, out_path, grp_fields){
  
  
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
