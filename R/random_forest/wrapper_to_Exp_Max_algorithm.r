exp_max_algorithm_boot <- function(
  i, pxl_dts_nm_root, pxl_dts_path,
  boot_ls, original_data, 
  y_var, my_preds, no_trees, 
  min_node_size, grp_flds, niter, 
  all_wgt, pAbs_wgt, out_model_name, 
  out_pred_name, pxl_dataset_full, 
  model_out_path, pred_out_path){
  
  
  #browser()
  
  pxl_dts_nm <- paste0(pxl_dts_nm_root, i, ".rds")
  
  pxl_dataset <- readRDS(file.path(pxl_dts_path, pxl_dts_nm))
  
  adm_dataset <- boot_ls[[i]]
  
  no_data <- nrow(original_data)
    
  valid_point_pos <- get_validating_point_positions(no_data, adm_dataset, "data_id")
  
  my_weights <- adm_dataset$new_weight 
  
  training_dataset <- adm_dataset[, c(y_var, my_preds)]
  
  y_data <- original_data[, "FOI"]
  
  x_data <- original_data[, my_preds]
  
  RF_fit <- spatial.cv.rf(
    preds = my_preds, 
    y_var = y_var, 
    train_set = training_dataset, 
    no_trees = no_trees, 
    min_node_size = min_node_size, 
    x_data = x_data, 
    y_data = y_data, 
    valid_points = valid_point_pos, 
    my_weights = my_weights)
  
  RF_obj <- RF_fit$obj
  
  p_i <- make_predictions(
    mod_obj = RF_obj, 
    dataset = pxl_dataset, 
    sel_preds = my_preds)
  
  pxl_dataset$p_i <- p_i
  
  adm_dataset <- adm_dataset[, c(grp_flds, y_var, "new_weight")]
  
  names(pxl_dataset_full)[names(pxl_dataset_full) == "ADM_0"] <- grp_flds[1]
  names(pxl_dataset_full)[names(pxl_dataset_full) == "ADM_1"] <- grp_flds[2]

  names(pxl_dataset)[names(pxl_dataset) == "ADM_0"] <- grp_flds[1]
  names(pxl_dataset)[names(pxl_dataset) == "ADM_1"] <- grp_flds[2]

  px_adm <- pxl_dataset %>% group_by_(.dots = grp_flds)
  
  adm_pop <- px_adm %>% summarise(adm_pop = sum(population))
  
  pxl_dataset <- left_join(pxl_dataset, adm_pop)
  
  pxl_dataset$pop_weight <- pxl_dataset$population / pxl_dataset$adm_pop
  
  pxl_dataset$new_weight <- all_wgt
  
  pxl_dataset[pxl_dataset$type == "pseudoAbsence", "new_weight"] <- pAbs_wgt
  
  pxl_dataset <- inner_join(pxl_dataset, adm_dataset[, c(grp_flds, y_var)])
  
  a <- out_model_name[i]
  b <- out_pred_name[i]
  
  exp_max_algorithm(niter = niter, 
                    adm_dataset = adm_dataset, 
                    pxl_dataset = pxl_dataset,
                    pxl_dataset_full = pxl_dataset_full,
                    no_trees = no_trees, 
                    min_node_size = min_node_size,
                    my_predictors = my_preds, 
                    grp_flds = grp_flds, 
                    out_model_name = a, 
                    out_pred_name = b, 
                    model_out_path = model_out_path, 
                    pred_out_path = pred_out_path)
  
  
}
