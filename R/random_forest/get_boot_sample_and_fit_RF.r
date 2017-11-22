get_boot_sample_and_fit_RF <- function(i, boot_ls, y_var, my_preds, no_trees, min_node_size, out_path, psAb_val, all_wgt, pAbs_wgt) {
  
  adm_dts_boot <- boot_ls[[i]]
  
  adm_dts_boot[adm_dts_boot$type == "pseudoAbsence", y_var] <- psAb_val
  
  adm_dts_boot$new_weight <- all_wgt
  adm_dts_boot[adm_dts_boot$type == "pseudoAbsence", "new_weight"] <- pAbs_wgt
  
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
