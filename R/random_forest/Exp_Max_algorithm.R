exp_max_algorithm <- function(
  niter, adm_dataset, pxl_dataset,
  no_trees, min_node_size,
  my_predictors, grp_flds, 
  out_model_name, out_pred_name,
  model_out_path, pred_out_path, gr_size){
  
  diagnostics <- c("RF_ms_i", "ss_i", "ss_j", "min_wgt", "max_wgt", "n_NA_pred")
  
  out_mat <- matrix(0, nrow = niter, ncol = length(diagnostics))
  
  colnames(out_mat) <- diagnostics
  
  #browser()
  
  for (i in seq_len(niter)){
    
    cat("iteration =", i, "\n")
    
    
    ### 1. calculate scaling factors (these ones are not constant - they change at i)
    
    p_i_by_adm <- pxl_dataset %>% group_by_(.dots = grp_flds)
    
    a_sum <- p_i_by_adm %>% summarise(a_sum = sum(pop_weight * p_i))
    
    dd <- left_join(pxl_dataset, a_sum)
    
    #dd$wgt_prime <- (dd$pop_weight / dd$p_i) * dd$a_sum
    dd$wgt_prime <- dd$pop_weight 
    
    
    ### 2. modify the scaling factors to account for background data
    
    dd$wgt_prime <- dd$wgt_prime * dd$new_weight
     

    ### 3. calculate new pseudo data value
    
    psAbs <- dd$a_sum == 0
    
    u_i <- rep(0, nrow(dd))
      
    u_i[!psAbs] <- (dd$o_j[!psAbs] * dd$p_i[!psAbs]) / dd$a_sum[!psAbs] 
    
    dd$u_i <- u_i
    
    if(sum(is.na(dd$u_i)) > 0) browser(text="u_i NA")
    
    
    ### 4. fit RF model
    
    case_weights <- dd$wgt_prime

    min_wgt <- min(case_weights)
    max_wgt <- max(case_weights)
    
    training_dataset <- dd[, c("u_i", my_predictors)]
	  #write.csv(training_dataset,"debug.csv")
    
    RF_obj <- ranger(
      formula = u_i ~ ., 
      data = training_dataset, 
      num.trees = no_trees, 
      case.weights = case_weights, 
      write.forest = TRUE, 
      min.node.size = min_node_size,
      verbose = TRUE)
    
    RF_ms_i <- RF_obj$prediction.error
    
    
    ### 5. make new pixel level predictions
    
    p_i <- make_predictions(RF_obj, dd, my_predictors)
      
    n_NA_pred <- sum(is.na(p_i))
    
    dd$p_i <- ifelse(is.na(p_i), 0, p_i)
    
	
    if(sum(is.na(dd$p_i)) > 0) browser(text="pred NA")

    
    ### 6. calculate pixel level sum of square
    
    ss_i <- sum(dd$wgt_prime * (dd$p_i - dd$u_i)^2)
    
    
    ### 7. calculate population weighted mean of pixel level predictions
    
    p_i_by_adm <- dd %>% group_by_(.dots = grp_flds)
    
    mean_p_i <- p_i_by_adm %>% summarise(mean_p_i = sum(p_i * pop_weight))
    
    aa <- inner_join(adm_dataset, mean_p_i)
    
    
    ### 8. calculate admin unit level sum of square
    
    ss_j <- sum(aa$new_weight * (aa$mean_p_i - aa$o_j)^2)
    
    out_mat[i,] <- c(RF_ms_i, ss_i, ss_j, min_wgt, max_wgt, n_NA_pred)
    
    
    pxl_dataset$p_i <- dd$p_i
    
  }
  
  pxl_dataset_full$p_i <- make_predictions(RF_obj, pxl_dataset_full, my_predictors)
  
  #write_out_rds(RF_obj, model_out_path, out_model_name)  
  
  write_out_rds(pxl_dataset_full, pred_out_path, out_pred_name)

  list(RF_obj, out_mat)
}
