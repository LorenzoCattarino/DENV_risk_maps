full_routine <- function(x, 
                         parms,
                         foi_data,
                         adm_covariates,
                         all_squares,
                         all_predictors){
  
  
  j <- x$exp_id 
  var_to_fit <- x$var
  number_of_predictors <- x$no_pred
  
  parms$no_predictors <- number_of_predictors
  parms$dependent_variable <- var_to_fit
  
  cat("exp id =", j, "\n")
  cat("response variable =", var_to_fit, "\n")
  cat("number of predictors =", number_of_predictors, "\n")
  
  model_type <- paste0("model_", j)  
  grp_flds <- parms$grp_flds
  base_info <- parms$base_info
  foi_offset <- parms$foi_offset
  
  
  # output dir -----------------------------------------------------------------
  
  
  RF_out_path <- file.path("output", 
                           "EM_algorithm", 
                           "best_fit_models", 
                           model_type, 
                           "optimized_model_objects")
  
  diagno_out_path <- file.path("output", 
                               "EM_algorithm", 
                               "best_fit_models",
                               model_type, 
                               "diagnostics")
  
  diagno_fig_path <- file.path("figures", 
                               "EM_algorithm", 
                               "best_fit_models",
                               model_type, 
                               "diagnostics")
  
  train_dts_path <- file.path("output", 
                              "EM_algorithm", 
                              "best_fit_models", 
                              model_type, 
                              "training_datasets")
  
  all_pred_out_path <- file.path("output",
                                 "EM_algorithm",
                                 "best_fit_models",
                                 model_type,
                                 "data_admin_predictions")
  
  global_predictions_out_path <- file.path("output", 
                                           "predictions_world", 
                                           "best_fit_models", 
                                           model_type)
  
  
  # ---------------------------------------------------------------------------
    

  my_predictors <- all_predictors[1:number_of_predictors]
  
  foi_data_2 <- preprocess_adm_dta(parms, foi_data)
  
  pxl_data_2 <- preprocess_pxl_data(parms, foi_data_2, all_squares)
  
  training_dataset <- foi_data_2[, c(var_to_fit, my_predictors, "new_weight")]
  
  RF_obj <- fit_ranger_RF(parms = parms, 
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
  
  RF_obj_optim <- exp_max_algorithm(parms = parms,
                                    orig_dataset = foi_data_2,
                                    pxl_dataset = pxl_data_4, 
                                    my_predictors = my_predictors, 
                                    RF_obj_path = RF_out_path,
                                    RF_obj_name = "RF_obj.rds",
                                    diagn_tab_path = diagno_out_path, 
                                    diagn_tab_name = "diagno_table.rds",
                                    train_dts_path = train_dts_path, 
                                    train_dts_name = "train_dts.rds",
                                    adm_dataset = adm_covariates)
  
  data_to_plot <- readRDS(file.path(diagno_out_path, "diagno_table.rds"))

  plot_EM_diagnostics(data_to_plot, diagno_fig_path, "diagnostics.png")
  
  prediction_set <- make_ranger_predictions(RF_obj_optim, 
                                            pxl_data_2, 
                                            my_predictors)

  join_all <- join_predictions(parms = parms, 
                               foi_dataset = foi_data_2, 
                               RF_obj = RF_obj_optim, 
                               adm_dataset = adm_covariates,
                               my_predictors = my_predictors, 
                               all_sqr_predictions = prediction_set, 
                               sqr_dataset = pxl_data_2)
  
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