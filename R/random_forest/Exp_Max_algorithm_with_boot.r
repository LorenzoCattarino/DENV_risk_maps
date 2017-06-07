exp_max_algorithm_multi_run <- function(
  i, pxl_dataset, gr_size,
  niter, foi_data, no_trees, 
  min_node_size, my_predictors, grp_flds,
  out_md_nm, out_prd_nm, md_out_pth, 
  prd_out_pth){
  
  
  ### 1. get a bootstrapped sample of the pxl dataset
  
  # overlay squared grid on data points 
  gridded_dataset <- grid_up(pxl_dataset, gr_size, rnd_dist = TRUE)
  
  # get a bootstrapped sample of the df indices  
  bt_ids <- do_boostrap(gridded_dataset)
  
  
  ### 2. run the EM algorithm 
  
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
    pred_out_path = prd_out_pth,
    boot_inds = bt_ids)
  
}
