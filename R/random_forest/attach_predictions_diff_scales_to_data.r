attach_pred_different_scale_to_data <- function(
  i, model_path, sqr_dts_path, 
  bt_sqr_preds_path, bt_samples, adm_dts, 
  predictors, all_sqr_preds, tile_ids, 
  in_path){
  
  #browser()
  
  h2o.init()
  
  
  # -------------------------------------- define variables
  
  
  RF_obj_nm <- paste0("RF_obj_sample_", i, ".rds")
  
  bt_sqr_dts_nm <- paste0("aggreg_pixel_level_env_vars_20km_sample_", i, ".rds")
  
  bt_sqr_preds_nm <- paste0("dd_debug_", i, ".rds")
  
  
  # -------------------------------------- load data
  
  
  RF_obj <- h2o.loadModel(file.path(model_path, RF_obj_nm))
  
  bt_sqr_dts <- readRDS(file.path(sqr_dts_path, bt_sqr_dts_nm))
    
  bt_sqr_preds <- readRDS(file.path(bt_sqr_preds_path, bt_sqr_preds_nm))
  
  
  # -------------------------------------- process the original bootstrap sample  
  
  
  foi_data <- boot_samples[[i]]
  
  # # remove pseudo absences 
  # foi_data <- subset(foi_data, type != "pseudoAbsence")
  
  # rename FOI field
  names(foi_data)[names(foi_data) == "FOI"] <- "o_j"
  
  
  # -------------------------------------- process admin predictions
  
  
  adm_dts_2 <- remove_NA_rows(adm_dts, predictors)
  
  adm_dts_2$adm_pred <- make_h2o_predictions(RF_obj, adm_dts_2, predictors)
  
  
  # -------------------------------------- process square predictions 
  
  
  bt_sqr_dts <- cbind(bt_sqr_dts[, c("data_id", "ADM_0", "ADM_1", "population")], pred = bt_sqr_preds$p_i)
  
  average_sqr <- average_up(
    pxl_df = bt_sqr_dts,
    grp_flds = c("data_id", "ADM_0", "ADM_1"),
    var_names = "pred")
  
  names(average_sqr)[names(average_sqr) == "ADM_0"] <- "ID_0"
  names(average_sqr)[names(average_sqr) == "ADM_1"] <- "ID_1" 
  names(average_sqr)[names(average_sqr) == "pred"] <- "mean_square_pred" 

    
  # -------------------------------------- process 1 km predictions 
  
  #[c(140, 141, 170, 171)]
  
  tile_prds <- loop(
    seq_along(tile_ids)[c(140, 141, 170, 171)],
    load_predict_filter,
    ids_vec = tile_ids, 
    in_path = in_path,
    predictors = predictors, 
    RF_obj = RF_obj, 
    foi_dts = foi_data,
    parallel = FALSE)
  
  tile_prds_rb <- do.call("rbind", tile_prds)
  
  average_pxl <- average_up(
    pxl_df = tile_prds_rb, 
    grp_flds = c("data_id", "ADM_0", "ADM_1"), 
    var_names = "pred")
  
  names(average_pxl)[names(average_pxl) == "ADM_0"] <- "ID_0"
  names(average_pxl)[names(average_pxl) == "ADM_1"] <- "ID_1" 
  names(average_pxl)[names(average_pxl) == "pred"] <- "mean_pxl_pred" 
  
  
  # -------------------------------------- join admin, square and pixel level preictions 
  
  
  browser()
  
  names(foi_data)[names(foi_data) == "ADM_0"] <- "ID_0"
  names(foi_data)[names(foi_data) == "ADM_1"] <- "ID_1"
  
  m_1 <- merge(
    foi_data[, c("data_id", "ID_0", "ID_1", "o_j")],
    adm_dts_2[, c("ID_0", "ID_1", "adm_pred")],
    by = c("ID_0", "ID_1"),
    all.y = FALSE)
  
  m_2 <- merge(
    m_1,
    average_sqr[, c("data_id", "ID_0", "ID_1", "mean_square_pred")],
    by = c("data_id", "ID_0", "ID_1"),
    all.y = FALSE)
  
  m_final <- merge(
    m_2,
    average_pxl[, c("data_id", "ID_0", "ID_1", "mean_pxl_pred")],
    by = c("data_id", "ID_0", "ID_1"),
    all.y = FALSE)
  
  
  # -------------------------------------- 
  
  
  h2o.shutdown(prompt = FALSE)

  
  # --------------------------------------
  
  
  m_final
  
}
