function(pxl_data_covariates, all_squares){
  
  
 # output dir -----------------------------------------------------------------
    
  
  figure_out_path <- file.path("figures", 
                               "EM_algorithm", 
                               "best_fit_models",
                               model_type, 
                               "diagnostics")
  
  preds_out_path <- file.path("output", 
                              "EM_algorithm", 
                              "best_fit_models", 
                              model_type)
  
  all_pred_out_path <- file.path("output",
                                 "EM_algorithm",
                                 "best_fit_models",
                                 model_type,
                                 "predictions_data")
  
  global_predictions_out_path <- file.path("output", 
                                           "predictions_world", 
                                           "best_fit_models", 
                                           model_type)
  
  # ---------------------------------------------------------------------------
    
    
  foi_data_2 <- preprocess_adm_dta(parameters, foi_data)
  
  pxl_data_2 <- preprocess_pxl_data(parms, foi_data_2, pxl_data)
  
  my_predictors <- predictor_rank$name[1:number_of_predictors]
  
  training_dataset <- foi_data[, c(var_to_fit, my_predictors, "new_weight")]
  
  RF_obj <- fit_ranger_RF(parms = parameters, 
                          dependent_variable = var_to_fit,
                          predictors = my_predictors, 
                          training_dataset = training_dataset, 
                          my_weights = "new_weight")
  
  p_i <- make_ranger_predictions(mod_obj = RF_obj, 
                                 dataset = pxl_data_2, 
                                 sel_preds = my_predictors)
  
  pxl_data_2$p_i <- p_i
  
  names(foi_data_2)[names(foi_data_2) == var_to_fit] <- "o_j"
  
  pxl_data_3 <- inner_join(pxl_data_2, foi_data_2[, c(grp_flds, "o_j")])
  
  pxl_dts_grp <- pxl_data_3 %>% 
    group_by(.dots = grp_flds) %>% 
    summarise(pop_sqr_sum = sum(population))
  
  pxl_data_4 <- left_join(pxl_data_3, pxl_dts_grp)
  
  pxl_data_4$pop_weight <- pxl_data_4$population / pxl_data_4$pop_sqr_sum
  
  RF_obj_optim <- exp_max_algorithm(parms = parameters,
                                    orig_dataset = foi_data_2,
                                    pxl_dataset = pxl_data_4, 
                                    my_predictors = my_predictors, 
                                    grp_flds = grp_flds,
                                    RF_obj_path = RF_out_pth,
                                    RF_obj_name = out_md_nm,
                                    diagn_tab_path = diag_t_pth, 
                                    diagn_tab_name = diag_t_nm,
                                    map_path = map_pth, 
                                    sct_plt_path = sct_plt_pth,
                                    train_dts_path = train_dts_pth, 
                                    train_dts_name = tra_dts_nm,
                                    adm_dataset = adm_covariates)
  
  data_to_plot <- readRDS(file.path(diag_t_pth, "diagno_table.rds"))

  plot_EM_diagnostics(data_to_plot, figure_out_path, "diagnostics.png")
  
  prediction_set <- make_ranger_predictions(RF_obj_optim, pxl_data_covariates, my_predictors)

  write_out_rds(prediction_set, preds_out_path, "square_predictions_all_data.rds")
  
  join_all <- join_predictions(parms = parameters, 
                               foi_dataset = foi_data_2, 
                               RF_obj = RF_obj_optim, 
                               adm_dataset = adm_covariates,
                               my_predictors = my_predictors, 
                               all_sqr_predictions = prediction_set, 
                               sqr_dataset = pxl_data_covariates)
  
  write_out_rds(join_all, all_pred_out_path, "all_scale_predictions.rds")
  
  global_predictions <- make_ranger_predictions(RF_obj_optim, 
                                                all_squares, 
                                                my_predictors)
  
  if(var_to_fit == "FOI"){
    
    global_predictions <- global_predictions - foi_offset
    
  }
  
  if(var_to_fit == "Z"){
    
    global_predictions <- (global_predictions - foi_offset) * all_squares$birth_rate * 35
    
  }
  
  global_predictions[global_predictions < 0] <- 0
  
  ret <- cbind(all_squares[, base_info], best = global_predictions)
  
  write_out_rds(ret, global_predictions_out_path, "response.rds")
  
}