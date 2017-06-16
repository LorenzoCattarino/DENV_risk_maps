get_boot_sample_and_fit_RF <- function(i, boot_ls, y_var, my_preds, no_trees, min_node_size, out_path) {
  
  adm_dts_boot <- boot_ls[[i]]
  
  my_weights <- adm_dts_boot$new_weight 
  
  training_dataset <- adm_dts_boot[, c(y_var, my_preds)]
  
  RF_obj <- fit_RF(dependent_variable = y_var, 
                   training_dataset = training_dataset, 
                   no_trees = no_trees, 
                   min_node_size = min_node_size,
                   my_weights = my_weights)
  
  a <- paste0("RF_obj_sample_", i, ".rds")
  
  write_out_rds(RF_obj, out_path, a)

}
