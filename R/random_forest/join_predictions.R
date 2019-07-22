join_predictions <- function(parms, 
                             foi_dataset, 
                             RF_obj, 
                             adm_dataset,
                             my_predictors, 
                             all_sqr_predictions, 
                             sqr_dataset){
  
  var_to_fit <- parms$dependent_variable
  foi_offset <- parms$foi_offset
  grp_fields <- parms$grp_flds
  id_field <- parms$id_fld
  all_wgt <- parms$all_wgt
  
  names(foi_dataset)[names(foi_dataset) == var_to_fit] <- "o_j"

  adm_pred <- make_ranger_predictions(RF_obj, adm_dataset, my_predictors)
  
  if(var_to_fit == "FOI"){
    
    foi_dataset$o_j <- foi_dataset$o_j - foi_offset
    adm_pred <- adm_pred - foi_offset
    all_sqr_predictions <- all_sqr_predictions - foi_offset
    
  }
  
  if(var_to_fit == "Z"){
    
    foi_dataset$o_j <- foi_dataset$o_j * foi_dataset$birth_rate * 35
    adm_pred <- (adm_pred - foi_offset) * adm_dataset$birth_rate * 35
    all_sqr_predictions <- (all_sqr_predictions - foi_offset) * sqr_dataset$birth_rate * 35
    
  }
  
  sqr_preds <- all_sqr_predictions
  
  sqr_dataset$p_i <- sqr_preds
  
  adm_dataset$admin <- adm_pred
  
  fltr_adm <- inner_join(adm_dataset, foi_dataset[, grp_fields])
  
  average_sqr <- average_up(pxl_df = sqr_dataset,
                            grp_flds = grp_fields,
                            var_names = "p_i")
  
  average_sqr <- rename(average_sqr, mean_p_i = p_i)

  df_lst <- list(foi_dataset[, c(grp_fields, "type", "o_j", "new_weight")],
                 fltr_adm[, c(grp_fields, "admin")],
                 average_sqr[, c(grp_fields, "mean_p_i")])
  
  join_all <- Reduce(function(...) left_join(...), df_lst)
  
  join_all_2 <- fitted_sero_cell_to_adm(join_all, 
                                        sqr_dataset, 
                                        c(id_field, "p_i"), 
                                        c(grp_fields, "type", "new_weight", "o_j", "admin", "mean_p_i"))
  join_all_2
  
}