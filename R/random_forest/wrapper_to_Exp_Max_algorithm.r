exp_max_algorithm_boot <- function(
  i, pxl_dts_path, adm_dts_orig, 
  pxl_dataset_orig, y_var, my_preds, 
  no_trees, min_node_size, grp_flds, niter, 
  all_wgt, pAbs_wgt,
  RF_obj_path, RF_obj_name,
  diagn_tab_path, diagn_tab_name){
  
  
  #browser()
  
  
  # ---------------------------------------- load pxl level dataset 
  
  
  pxl_dts_nm <- paste0("All_FOI_estimates_disaggreg_20km_sample_", i, ".rds")
  pxl_dts_boot <- readRDS(file.path(pxl_dts_path, pxl_dts_nm))
  
  
  # ---------------------------------------- get output name 
  
  
  a <- RF_obj_name[i]
  b <- diagn_tab_name[i]
  
  
  # ---------------------------------------- for tracking training and validating set points 
  
  
  no_data <- nrow(pxl_dataset_orig)
  train_point_pos <- get_training_point_positions(no_data, pxl_dts_boot, "data_id")
  valid_point_pos <- get_validating_point_positions(no_data, pxl_dts_boot, "data_id")
  
  
  # ---------------------------------------- pre process pxl level dataset
  
  
  names(pxl_dts_boot)[names(pxl_dts_boot) == "ADM_0"] <- grp_flds[1]
  names(pxl_dts_boot)[names(pxl_dts_boot) == "ADM_1"] <- grp_flds[2]
  
  #px_adm <- pxl_dts_boot %>% group_by_(.dots = grp_flds)
  
  #adm_pop <- px_adm %>% summarise(adm_pop = sum(population))
  
  #pxl_dts_boot <- left_join(pxl_dts_boot, adm_pop)
  
  pxl_dts_boot$pop_weight <- pxl_dts_boot$population / pxl_dts_boot$adm_pop
  
  pxl_dts_boot$new_weight <- all_wgt
  
  pxl_dts_boot[pxl_dts_boot$type == "pseudoAbsence", "new_weight"] <- pAbs_wgt
  
  
  # ---------------------------------------- attach adm level prediction to pxl level dataset
  
  
  pxl_dts_boot <- inner_join(pxl_dts_boot, adm_dts_orig[, c(grp_flds, y_var)])
  
  
  # ---------------------------------------- run the EM 
  
  
  square_preds <- exp_max_algorithm(
    niter = niter, 
    adm_dataset = adm_dts_orig, 
    pxl_dataset = pxl_dts_boot,
    pxl_dataset_full = pxl_dataset_orig,
    no_trees = no_trees, 
    min_node_size = min_node_size,
    my_predictors = my_preds, 
    grp_flds = grp_flds, 
    RF_obj_path = RF_obj_path,
    RF_obj_name = a,
    diagn_tab_path = diagn_tab_path, 
    diagn_tab_name = b)
  
  list(square_preds, train_point_pos, valid_point_pos)
}
