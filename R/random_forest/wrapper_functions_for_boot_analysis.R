
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
                                       out_file_path_2){
  
  parms$grp_flds <- c(parms$grp_flds, "data_id")
    
  # load
  foi_data <- boot_samples[[i]]
  
  foi_data_2 <- preprocess_adm_data(parms, foi_data)
    
  all_squares_2 <- preprocess_pxl_data(parms, foi_data_2, all_squares)
  
  aa <- paste0("sample_", i, ".rds")
  
  # save
  write_out_rds(foi_data_2, out_file_path_1, aa)
  write_out_rds(all_squares_2, out_file_path_2, aa)
  
}
  

# -----------------------------------------------------------------------------


get_bsample_and_fit_RF <- function(i, 
                                   parms,
                                   foi_data_path, 
                                   my_preds, 
                                   out_path) {
  
  y_var <- parms$dependent_variable
  psAb_val <- parms$pseudoAbs_value[y_var]
  foi_offset <- parms$foi_offset
  
  aa <- paste0("sample_", i, ".rds")
  
  adm_dts_boot <- readRDS(file.path(foi_data_path, aa))
  
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
                                   foi_data_path,
                                   my_preds, 
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
  grp_flds <- parms$grp_flds
  
  # get output name -----------------------------------------------------------  
  
  
  tag <- paste0("sample_", i)
  aa <- paste0(tag, ".rds")
  cc <- file.path(map_path, tag)
  ff <- file.path(sct_plt_path, tag)
  
  
  # load bootstrapped data sets -----------------------------------------------  
  
  
  foi_data <- readRDS(file.path(foi_data_path, aa))
  
  pxl_data <- readRDS(file.path(pxl_dts_pt, aa))
  
  
  # pre process --------------------------------------------------------------- 
  
  
  names(foi_data)[names(foi_data) == var_to_fit] <- "o_j"
  
  pxl_data_2 <- inner_join(pxl_data, foi_data[, c(grp_flds, "o_j")])
  
  
  # calculate population weights
  
  pxl_dts_grp <- pxl_data_2 %>% 
    group_by(.dots = grp_flds) %>% 
    summarise(pop_sqr_sum = sum(population))
  
  pxl_data_3 <- left_join(pxl_data_2, pxl_dts_grp)
  
  pxl_data_3$pop_weight <- pxl_data_3$population / pxl_data_3$pop_sqr_sum
  
  
  # run the EM ----------------------------------------------------------------  
  

  RF_obj_optim <- exp_max_algorithm(parms = parms, 
                                    orig_dataset = foi_data, 
                                    pxl_dataset = pxl_data_3,
                                    my_predictors = my_preds, 
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

get_bsample_and_join_predictions <- function(i, 
                                             model_path, 
                                             original_foi,
                                             boot_foi_data_path,
                                             adm_dts, 
                                             predictors, 
                                             data_sqr_predictions_path,
                                             sqr_dts, 
                                             out_path,
                                             parms){
  
    
  aa <- paste0("sample_", i, ".rds")
  
  
  # load model and bootstrapped data sets -------------------------------------
  
  
  RF_obj <- readRDS(file.path(model_path, aa))
  
  boot_foi_data <- readRDS(file.path(boot_foi_data_path, aa))
    
  sqr_preds <- readRDS(file.path(data_sqr_predictions_path, aa))
  
  
  # run -----------------------------------------------------------------------
  

  join_all <- join_predictions(parms = parms, 
                               foi_dataset = original_foi, 
                               RF_obj = RF_obj, 
                               adm_dataset = adm_dts,
                               my_predictors = predictors, 
                               all_sqr_predictions = sqr_preds, 
                               sqr_dataset = sqr_dts)
  
  ids <- unique(boot_foi_data$data_id)
  
  train_ids <- rep(0, nrow(original_foi))
  
  train_ids[ids] <- 1
  
  join_all$train <- train_ids
  
  write_out_rds(join_all, out_path, aa)
  
  
}
