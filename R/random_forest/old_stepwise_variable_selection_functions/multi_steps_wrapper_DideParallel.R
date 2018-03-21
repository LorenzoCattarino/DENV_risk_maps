###
# function for running a stepwise removal of predictors for spatially cross-validated random forest models 
###
multi_steps_wrapper <- function(dataset, grid_size, vector_of_predictors_chr, no_steps, no_fits, no_trees, cell_fraction, train_fraction, level_num, ID_exp, ID_run, addition)
{
  # Transform character to numeric
  vector_of_predictors <- which(names(dataset) %in% vector_of_predictors_chr)
  
  if(length(vector_of_predictors) < no_steps) 
  {
    warning("Number of predictors smaller than number of addition steps", call. = FALSE)
    no_steps <- length(vector_of_predictors)         
  }
  
  cat("level of stepwise addition = ", level_num, "\n")
  
  # create empty vectors to store selected predictors and corr coeff value 
  changed_predictor <- NULL
  changed_predictor_rmse <- NULL
  
  if(addition == TRUE)
  {
    NULL
  }else{
    no_steps <- length(vector_of_predictors)-1
  }
  
  for (i in 1:no_steps)
  {
    cat("step of change = ", i, "\n")
    
    # create combination of predictors by removing one predictor at the time from the vector of selected predictors
    if(addition == TRUE)
    {
      variables_to_change <- vector_of_predictors [!vector_of_predictors %in% changed_predictor]    
      
      get_combs_fun <- function(x, changed_predictor)
      {
        out <- c(changed_predictor,x)
      }
      
      combinations_of_predictors <- lapply(as.list(variables_to_change), get_combs_fun, changed_predictor)
      
      output_folder <- "addition"
      
    }else{
      variables_to_change <- vector_of_predictors    
      
      get_combs_fun <- function(x, vector_of_predictors)
      {
        vector_of_predictors <- as.numeric(vector_of_predictors)
        out <- vector_of_predictors[-which(vector_of_predictors==x)] 
      }
      
      combinations_of_predictors <- lapply(as.list(as.numeric(variables_to_change)), get_combs_fun, vector_of_predictors)
      
      output_folder <- "removal"
      
    }
    
    predictor_importance_test_output_local <- parallel::clusterApply(NULL, combinations_of_predictors, 
                                                                     predictor_combs_wrapper, 
                                                                     dataset, 
                                                                     grid_size, no_fits, no_trees, cell_fraction, train_fraction)

    # extract results 
    diagnostics <- as.data.frame(do.call("rbind", lapply(predictor_importance_test_output_local, "[[", 1)))
    
    # create matrix of all combinations of predictors used
    combinations_of_predictors_mat <- do.call(rbind, combinations_of_predictors)
    combinations_of_predictors_mat <- cbind(combinations_of_predictors_mat, 
                                            as.numeric(variables_to_change), 
                                            rep(0, nrow(combinations_of_predictors_mat)))
    colnames(combinations_of_predictors_mat) <- 1:dim(combinations_of_predictors_mat)[2]
    colnames(combinations_of_predictors_mat) <- c(paste("x", 1:length(combinations_of_predictors[[1]]), sep=""), "changed_predictor", "rmse_valid")
    
    # combine together run outputs and matrix of predictor combs 
    combinations_of_predictors_mat[,"rmse_valid"] <- diagnostics$rmse.valid
    final_output_df <- as.data.frame(combinations_of_predictors_mat, stringsAsFactors = FALSE)
    
    # sort df by rmse value
    final_output_df_sorted <- final_output_df[order(final_output_df$rmse_valid, decreasing = FALSE),]
    
    # get name and save df
    df_name_ext <- sprintf("predictor_importance_test_step_output_%s_%s_%s_%s%s", paste("exp", ID_exp, sep="_"), 
                           paste("run", ID_run, sep="_"), paste("level", level_num, sep="_"), paste("step", i, sep="_"), ".rds")
    saveRDS(final_output_df_sorted, file.path("output", "dengue_dataset", "predictor_importance_test", output_folder, 
                                              paste("exp", ID_exp, sep="_"), paste("run", ID_run, sep="_"), df_name_ext))
    
    # get column index in dataset of predictor which contributed to the smallest increase in rmse 
    most_important_predictor_col_index <- final_output_df_sorted[1,"changed_predictor"]
    cat("index of changed predictor = ", most_important_predictor_col_index, "\n")
    
    # get name of predictor which contributed to the smallest rmse
    most_important_changed_predictor_name <- names(dataset)[most_important_predictor_col_index]
    cat("most important changed predictor name = ", most_important_changed_predictor_name, "\n")
    
    if(addition == TRUE)
    {
      NULL
    }else{
      vector_of_predictors <- vector_of_predictors[!vector_of_predictors %in% most_important_predictor_col_index]  
    }
    
    changed_predictor [i] <- most_important_predictor_col_index
    
    changed_predictor_rmse [i] <- final_output_df_sorted[1, "rmse_valid"] 
  }
  
  # get all data together in a df 
  predictor_importance_test_data_to_plot <- data.frame(Step = seq_len(no_steps), 
                                                       changed_predictor = changed_predictor, 
                                                       name = names(dataset)[changed_predictor], 
                                                       rmse_valid = changed_predictor_rmse)
}
