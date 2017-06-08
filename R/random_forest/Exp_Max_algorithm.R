exp_max_algorithm <- function(
  niter, adm_dataset, pxl_dataset,
  no_trees, min_node_size,
  my_predictors, grp_flds, 
  out_model_name, out_pred_name,
  model_out_path, pred_out_path, boot_inds){
  
  diagnostics <- c("RF_ms_i", "ss_i", "ss_j", "min_wgt", "max_wgt", "n_NA_pred")
  
  out_mat <- matrix(0, nrow = niter, ncol = length(diagnostics))
  
  colnames(out_mat) <- diagnostics
  
  for (i in seq_len(niter)){
    
    cat("iteration =", i, "\n")
    
    browser()

    
    ### 1. fix zero predictions
    
    #pxl_dataset$p_i[pxl_dataset$p_i == 0] <- 0.00001
    
    
    ### 2. calculate scaling factors (these ones are not constant - they change at i)
    
    p_i_by_adm <- pxl_dataset %>% group_by_(.dots = grp_flds)
    
    a_sum <- p_i_by_adm %>% summarise(a_sum = sum(pop_weight * p_i))
    
    dd <- left_join(pxl_dataset, a_sum)
    
    #dd$wgt_prime <- (dd$pop_weight / dd$p_i) * dd$a_sum
    dd$wgt_prime <- dd$pop_weight 
    
    
    ### 3. modify the scaling factors to account for background data
    
    dd$wgt_prime <- dd$wgt_prime * dd$new_weight
     

    ### 4. calculate new pseudo data value
    
    psAbs <- dd$o_j == -0.02
    
    u_i <- rep(0, nrow(dd))
      
    u_i[!psAbs] <- (dd$o_j[!psAbs] * dd$p_i[!psAbs]) / dd$a_sum[!psAbs] 
    
    dd$u_i <- u_i
    
    if(sum(is.na(dd$u_i)) > 0) browser(text="u_i NA")
    
    
    ### 5. fit RF model
    
    case_weights <- dd$wgt_prime

    min_wgt <- min(case_weights)
    max_wgt <- max(case_weights)
    
    training_dataset <- dd[unlist(boot_inds), c("u_i", my_predictors)]
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
    
    
    ### 6. make new pixel level predictions
    
    x_data <- dd[, my_predictors]
    
    run_predict <- predict(RF_obj, x_data)
    
    p_i <- run_predict$predictions
		
    p_i[p_i < 0] <- 0
    
    n_NA_pred <- sum(is.na(p_i))
    
    dd$p_i <- ifelse(is.na(p_i), 0, p_i)
    
	
    if(sum(is.na(dd$p_i)) > 0) browser(text="pred NA")

    
    ### 7. calculate pixel level sum of square
    
    ss_i <- sum(dd$wgt_prime * (dd$p_i - dd$u_i)^2)
    
    
    ### 8. calculate population weighted mean of pixel level predictions
    
    p_i_by_adm <- dd %>% group_by_(.dots = grp_flds)
    
    mean_p_i <- p_i_by_adm %>% summarise(mean_p_i = sum(p_i * pop_weight))
    
    aa <- left_join(adm_dataset, mean_p_i)
    
    
    ### 9. calculate admin unit level sum of square
    
    ss_j <- sum(aa$new_weight * (aa$mean_p_i - aa$o_j)^2)
    
    out_mat[i,] <- c(RF_ms_i, ss_i, ss_j, min_wgt, max_wgt, n_NA_pred)
    
    
    pxl_dataset$p_i <- dd$p_i
    
  }
  
  write_out_rds(RF_obj, model_out_path, out_model_name)  
  
  write_out_rds(pxl_dataset, pred_out_path, out_pred_name)

  out_mat
}
