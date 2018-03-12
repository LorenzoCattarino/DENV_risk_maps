# The following functions are used for running a stepwise variable ADDITION routine 
# to assess the importance of env variables as predictors in spatially cross-validated 
# random forest models

stepwise_addition_boot <- function(i, 
                                   boot_ls, 
                                   y_var, 
                                   psAb_val, 
                                   all_wgt, 
                                   wgt_limits, 
                                   predictors, 
                                   no_steps_L1,
                                   no_steps_L2,
                                   no_trees,
                                   min_node_size,
                                   foi_data,
                                   start_h2o,
                                   shut_h2o,
                                   out_path){
  
  ID_run <- i
  
  adm_dts_boot <- boot_ls[[ID_run]]
  
  adm_dts_boot[adm_dts_boot$type == "pseudoAbsence", y_var] <- psAb_val
  
  adm_dts_boot$new_weight <- all_wgt
  pAbs_wgt <- get_area_scaled_wgts(adm_dts_boot, wgt_limits)
  adm_dts_boot[adm_dts_boot$type == "pseudoAbsence", "new_weight"] <- pAbs_wgt
  
  ret_level_1 <- multi_steps_wrapper(dataset = adm_dts_boot, 
                                     predictors = predictors, 
                                     no_steps = no_steps_L1, 
                                     level_num = 1,
                                     addition = TRUE,
                                     y_var = y_var, 
                                     no_trees = no_trees, 
                                     min_node_size = min_node_size, 
                                     foi_data = foi_data,
                                     start_h2o = start_h2o,
                                     shut_h2o = shut_h2o, 
                                     ID_run = ID_run,
                                     out_path = out_path)
  
  df_name_level_1 <- sprintf("all_steps_output_%s%s", 
                             paste0("level", 1, sep="_"), 
                             ".csv")  
  
  my_out_path <- file.path(out_pth, paste("run", ID_run, sep="_"))
  
  
  write_out_csv(ret_level_1, my_out_path, df_name_level_1)
  
  subset_of_predictors <- ret_level_1$name
  
  ret_level_2 <- multi_steps_wrapper(dataset = adm_dts_boot, 
                                     predictors = subset_of_predictors, 
                                     no_steps = no_steps_L2, 
                                     level_num = 2,
                                     addition = TRUE,
                                     y_var = y_var, 
                                     no_trees = no_trees, 
                                     min_node_size = min_node_size, 
                                     foi_data = foi_data,
                                     start_h2o = start_h2o,
                                     shut_h2o = shut_h2o, 
                                     ID_run = ID_run,
                                     out_path = out_path)
  
  df_name_level_2 <- sprintf("all_steps_output_%s%s", 
                             paste0("level", 2, sep="_"), 
                             ".csv")
  
  write_out_csv(ret_level_1, my_out_path, df_name_level_2)
  
  fact_combs_df_name <- "factor_combinations.csv"
  
  write_out_csv(factor_combs_df, out_path, fact_combs_df_name)
  
  list(ret_level_1, ret_level_2)
  
}

multi_steps_wrapper <- function(dataset, 
                                predictors, 
                                no_steps, 
                                level_num,
                                addition,
                                y_var, 
                                no_trees, 
                                min_node_size, 
                                foi_data,
                                start_h2o,
                                shut_h2o, 
                                ID_run, 
                                out_pth){
  
  # Transform character to numeric
  vector_of_predictors <- which(names(dataset) %in% predictors)
  
  if (length(vector_of_predictors) < no_steps) {
    
    warning("Number of predictors smaller than number of addition steps", call. = FALSE)
    
    no_steps <- length(vector_of_predictors)         
  
  }
  
  cat("level of stepwise addition = ", level_num, "\n")
  
  # create empty vectors to store selected predictors and corr coeff value 
  changed_predictor <- NULL
  changed_predictor_rmse <- NULL
  
  if (addition) {
    NULL
  } else {
    no_steps <- length(vector_of_predictors)-1
  }
  
  for (i in seq_len(no_steps)) {
    
    cat("step of change = ", i, "\n")
    
    # create combination of predictors by removing one predictor at the time from the vector of selected predictors
    
    if (addition) {
      
      variables_to_change <- vector_of_predictors [!vector_of_predictors %in% changed_predictor]    
      
      get_combs_fun <- function(x, changed_predictor){
        out <- c(changed_predictor,x)
      }
      
      combinations_of_predictors <- lapply(as.list(variables_to_change), get_combs_fun, changed_predictor)
    
    } else {
      
      variables_to_change <- vector_of_predictors    
      
      get_combs_fun <- function(x, vector_of_predictors){
        vector_of_predictors <- as.numeric(vector_of_predictors)
        out <- vector_of_predictors[-which(vector_of_predictors==x)] 
      }
      
      combinations_of_predictors <- lapply(as.list(as.numeric(variables_to_change)), 
                                           get_combs_fun, 
                                           vector_of_predictors)
    
    }
    
    predictor_importance_test_output_local <- parallel::clusterApply(NULL, 
                                                                     combinations_of_predictors, 
                                                                     predictor_combs_wrapper, 
                                                                     dataset = dataset, 
                                                                     y_var = y_var, 
                                                                     no_trees = no_trees, 
                                                                     min_node_size = min_node_size, 
                                                                     foi_data = foi_data,
                                                                     start_h2o - start_h2o,
                                                                     shut_h2o - shut_h2o)
    
    # extract results 
    diagnostics <- as.data.frame(do.call("rbind", lapply(predictor_importance_test_output_local, "[[", 1)))

    # create matrix of all combinations of predictors used
    combinations_of_predictors_mat <- do.call(rbind, combinations_of_predictors)
    combinations_of_predictors_mat <- cbind(combinations_of_predictors_mat, 
                                            as.numeric(variables_to_change), 
                                            rep(0, nrow(combinations_of_predictors_mat)))
    colnames(combinations_of_predictors_mat) <- 1:dim(combinations_of_predictors_mat)[2]
    colnames(combinations_of_predictors_mat) <- c(paste0("x", 1:length(combinations_of_predictors[[1]])), 
                                                  "changed_predictor", 
                                                  "rmse_valid")
    
    # combine together run outputs and matrix of predictor combs 
    combinations_of_predictors_mat[,"rmse_valid"] <- diagnostics$rmse.valid
    final_output_df <- as.data.frame(combinations_of_predictors_mat, stringsAsFactors = FALSE)
    
    # sort df by rmse value
    final_output_df_sorted <- final_output_df[order(final_output_df$rmse_valid, decreasing = FALSE),]
    
    # get column index in dataset of predictor which contributed to the smallest increase in rmse 
    most_important_predictor_col_index <- final_output_df_sorted[1,"changed_predictor"]
    cat("index of changed predictor = ", most_important_predictor_col_index, "\n")
    
    # get name of predictor which contributed to the smallest rmse
    most_important_changed_predictor_name <- names(dataset)[most_important_predictor_col_index]
    cat("most important changed predictor name = ", most_important_changed_predictor_name, "\n")
    
    if (addition) {
      NULL
    } else {
      vector_of_predictors <- vector_of_predictors[!vector_of_predictors %in% most_important_predictor_col_index]  
    }
    
    changed_predictor [i] <- most_important_predictor_col_index
      
    changed_predictor_rmse [i] <- final_output_df_sorted[1, "rmse_valid"] 
    
    
    # save --------------------------------
    
    
    # get name and save df
    df_name_ext <- sprintf("per_step_output_%s_%s_%s%s", 
                           paste("level", level_num, sep="_"), 
                           paste("step", i, sep="_"), 
                           ".rds")
    
    my_out_path <- file.path(out_pth, paste("run", ID_run, sep="_"))
    
    write_out_rds(final_output_df_sorted, my_out_path, df_name_ext)
    
  
  }
  
  data.frame(Step = seq_len(no_steps), 
             changed_predictor = changed_predictor, 
             name = names(dataset)[changed_predictor], 
             rmse_valid = changed_predictor_rmse)

}

predictor_combs_wrapper <- function(x, 
                                    dataset, 
                                    y_var, 
                                    no_trees, 
                                    min_node_size, 
                                    psAb_val, 
                                    all_wgt, 
                                    wgt_limits,
                                    foi_data,
                                    start_h2o,
                                    shut_h2o){
  
  cat("combination of predictors = ", x, "\n")
  
  get_bsam_fit_predict_and_error(boot_data = dataset, 
                                 y_var = y_var, 
                                 my_preds = x, 
                                 no_trees = no_trees, 
                                 min_node_size = min_node_size, 
                                 foi_data = foi_data,
                                 start_h2o = start_h2o,
                                 shut_h2o = shut_h2o)
}
