
# -----------------------------------------------------------------------------
#
#
# Wrapper functions to conduct the bootstrap analysis
#
#
# -----------------------------------------------------------------------------



get_bsample_and_preprocess <- function(i, 
                                       parms,
                                       boot_samples, 
                                       all_squares,
                                       out_file_path_1, 
                                       out_file_path_2,
                                       out_file_name){
  
  # load
  foi_data <- boot_samples[[i]]
  
  run_preprocess <- preprocess(parms, foi_data, all_squares)
  
  foi_data_2 <- run_preprocess[[1]]
    
  all_squares_2 <- run_preprocess[[2]]
  
  a <- out_file_name[i]
  
  # save
  write_out_rds(foi_data_2, out_file_path_1, a)
  write_out_rds(all_squares_2, out_file_path_2, a)
  
}
  

# -----------------------------------------------------------------------------


get_bsample_and_fit_RF <- function(i, 
                                   parms,
                                   boot_ls, 
                                   my_preds, 
                                   out_path) {
  
  y_var <- parms$dependent_variable
  psAb_val <- parms$pseudoAbs_value[y_var]
  foi_offset <- parms$foi_offset
  
  aa <- paste0("sample_", i, ".rds")
  
  adm_dts_boot <- boot_ls[[i]]
  
  # adm_dts_boot[adm_dts_boot$type == "pseudoAbsence", y_var] <- psAb_val
  # 
  # if(y_var == "FOI"){
  #   
  #   adm_dts_boot[, y_var] <- adm_dts_boot[, y_var] + foi_offset
  #   
  # }
  
  training_dataset <- adm_dts_boot[, c(y_var, my_preds, "new_weight")]
  
  RF_obj <- fit_ranger_RF(parms = parms,
                          dependent_variable = y_var, 
                          predictors = my_preds, 
                          training_dataset = training_dataset, 
                          my_weights = "new_weight")
  
  write_out_rds(RF_obj, out_path, aa)
  
}


# -----------------------------------------------------------------------------


get_bsample_and_predict <- function(i, 
                                    RF_obj_path, 
                                    my_preds, 
                                    out_path,
                                    in_path){
  
  aa <- paste0("sample_", i, ".rds")
  
  pxl_dts_boot <- readRDS(file.path(in_path, aa))
  
  RF_obj <- readRDS(file.path(RF_obj_path, aa))
  
  p_i <- make_ranger_predictions(RF_obj, pxl_dts_boot, my_preds)
  
  pxl_dts_boot$p_i <- p_i
  
  write_out_rds(pxl_dts_boot, out_path, aa)
  
}


# -----------------------------------------------------------------------------


get_bsample_and_EM_fit <- function(i, 
                                   parms,
                                   boot_samples, 
                                   my_preds, 
                                   grp_flds, 
                                   RF_obj_path,
                                   diagn_tab_path,
                                   map_path, 
                                   sct_plt_path,
                                   adm_dataset, 
                                   pxl_dts_pt,
                                   train_dts_path,
                                   data_squares,
                                   all_squares,
                                   data_sqr_predictions_out_path,
                                   all_sqr_predictions_out_path){
  
  
  # define variables ---------------------------------------------------------- 
  
  
  var_to_fit <- parms$dependent_variable
  
  
  # get output name -----------------------------------------------------------  
  
  
  aa <- paste0("sample_", i, ".rds")
  cc <- file.path(map_path, aa)
  ff <- file.path(sct_plt_path, aa)
  
  
  # load bootstrapped data sets -----------------------------------------------  
  
  
  foi_data <- boot_samples[[i]]
  
  pxl_data <- readRDS(file.path(pxl_dts_pt, aa))
  
  
  # pre process --------------------------------------------------------------- 
  
  
  names(foi_data)[names(foi_data) == var_to_fit] <- "o_j"
  
  pxl_data_2 <- inner_join(pxl_data, foi_data[, c(grp_flds, "o_j")])
  
  
  # calculate population weights
  
  pxl_dts_grp <- pxl_data_2 %>% group_by(.dots = grp_flds) 
  
  aa <- pxl_dts_grp %>% summarise(pop_sqr_sum = sum(population))
  
  pxl_data_3 <- left_join(pxl_data_2, aa)
  
  pxl_data_3$pop_weight <- pxl_data_3$population / pxl_data_3$pop_sqr_sum
  
  
  # run the EM ----------------------------------------------------------------  
  
  
  RF_obj_optim <- exp_max_algorithm(parms = parms, 
                                    orig_dataset = foi_data, 
                                    pxl_dataset = pxl_data_3,
                                    my_predictors = my_preds, 
                                    grp_flds = grp_flds, 
                                    RF_obj_path = RF_obj_path,
                                    RF_obj_name = aa,
                                    diagn_tab_path = diagn_tab_path, 
                                    diagn_tab_name = aa,
                                    map_path = cc, 
                                    sct_plt_path = ff,
                                    train_dts_path = train_dts_path,
                                    train_dts_name = aa,
                                    adm_dataset = adm_dataset)
  
  p_i_all <- make_ranger_predictions(RF_obj_optim, data_squares, my_preds)
  
  global_predictions <- make_ranger_predictions(RF_obj_optim, all_squares, my_preds)
  
  write_out_rds(p_i_all, data_sqr_predictions_out_path, aa)
  
  write_out_rds(global_predictions, all_sqr_predictions_out_path, aa)
  
}
