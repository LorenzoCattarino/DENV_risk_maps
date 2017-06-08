exp_max_algorithm_boot <- function(
  i, pxl_dataset, gr_size,
  niter, foi_data, no_trees, 
  min_node_size, my_predictors, grp_flds,
  out_model_name, out_pred_name,
  model_out_path, pred_out_path){
  
  a <- out_model_name[i]
  b <- out_pred_name[i]
  
  exp_max_algorithm(
    niter = niter, 
    adm_dataset = foi_data,
    pxl_dataset = pxl_dataset, 
    no_trees = no_trees, 
    min_node_size = min_node_size,
    my_predictors = my_predictors, 
    grp_flds = grp_flds,
    out_model_name = a,
    out_pred_name = a,
    model_out_path = model_out_path,
    pred_out_path = pred_out_path,
    gr_size = gr_size)
  
}
