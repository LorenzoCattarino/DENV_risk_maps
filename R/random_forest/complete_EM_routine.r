complete_EM_routine_multi_run <- function(
  i, foi_data, gr_size,
  my_predictors, y_var, no_trees,
  min_node_size, x_data, y_data,
  pxl_dataset, grp_flds, niter,
  no_trees, min_node_size, out_md_nm,
  out_prd_nm, md_out_pth, prd_out_pth){
  
  
  ### 1. Fit RF to bootstrapped sample of original data
  
  RF_fit <- wrapper_to_core_fun( 
    model_dataset = foi_data, 
    grid_size = gr_size, 
    predictors = my_predictors,
    dependent_variable = y_var, 
    no_trees = no_trees, 
    min_node_size = min_node_size, 
    x_data = x_data, 
    y_data = y_data)
  
  RF_obj <- RF_fit$obj
  
  
  ### 2. Make initial square-level  predictions
  
  p_i <- make_predictions(
    mod_obj = RF_obj, 
    dataset = pxl_dataset, 
    sel_preds = my_predictors)
  
  pxl_dataset$p_i <- p_i
  
  
  ### 3. EM pre-processing 
  
  names(pxl_dataset)[names(pxl_dataset) == "ADM_0"] <- grp_flds[1]
  names(pxl_dataset)[names(pxl_dataset) == "ADM_1"] <- grp_flds[2]
  
  px_adm <- pxl_dataset %>% group_by_(.dots = grp_flds)
  
  adm_pop <- px_adm %>% summarise(adm_pop = sum(population))
  
  pxl_dataset <- left_join(pxl_dataset, adm_pop)
  
  pxl_dataset$pop_weight <- pxl_dataset$population / pxl_dataset$adm_pop
  
  
  ### 4. Run EM algorithm
  
  exp_max_algorithm(
    niter = niter, 
    adm_dataset = foi_data,
    pxl_dataset = pxl_dataset, 
    no_trees = no_trees, 
    min_node_size = min_node_size,
    my_predictors = my_predictors, 
    grp_flds = grp_flds,
    out_model_name = out_md_nm,
    out_pred_name = out_prd_nm,
    model_out_path = md_out_pth,
    pred_out_path = prd_out_pth)
  
}
