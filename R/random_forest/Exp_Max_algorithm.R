exp_max_algorithm <- function(
  niter, adm_dataset, pxl_dataset,
  pxl_dataset_full, no_trees, min_node_size,
  my_predictors, grp_flds, 
  RF_obj_path, RF_obj_name,
  diagn_tab_path, diagn_tab_name,
  map_path, map_name, 
  sq_pr_path, sq_pr_name){
  
  diagnostics <- c("RF_ms_i", "ss_i", "ss_j", "min_wgt", "max_wgt", "n_NA_pred")
  
  out_mat <- matrix(0, nrow = niter, ncol = length(diagnostics))
  
  colnames(out_mat) <- diagnostics
  
  h2o.init()
  
  for (i in seq_len(niter)){
    
    #browser()
    
    cat("iteration =", i, "\n")
    
    
    # -------------------------------------- 1. calculate scaling factors
    
    
    p_i_by_adm <- pxl_dataset %>% group_by_(.dots = grp_flds)
    
    a_sum <- p_i_by_adm %>% summarise(a_sum = sum(pop_weight * p_i))
    
    dd <- left_join(pxl_dataset, a_sum)
    
    dd$wgt_prime <- dd$pop_weight
    
    # dd$wgt_prime <- (dd$pop_weight / dd$p_i) * dd$a_sum
    # isfin_log <- is.finite(dd$wgt_prime)
    # max_fin_wgt <- max(dd$wgt_prime[isfin_log])
    # dd$wgt_prime <- ifelse(is.infinite(dd$wgt_prime), max_fin_wgt, dd$wgt_prime) 
    
    
    # -------------------------------------- 2. modify the scaling factors to account for background data
    
    
    dd$wgt_prime <- dd$wgt_prime * dd$new_weight
     

    # -------------------------------------- 3. calculate new pseudo data value
    
    
    psAbs <- dd$a_sum == 0
    
    u_i <- rep(0, nrow(dd))
      
    u_i[!psAbs] <- (dd$o_j[!psAbs] * dd$p_i[!psAbs]) / dd$a_sum[!psAbs] 
    
    dd$u_i <- u_i
    
    if(sum(is.na(dd$u_i)) > 0) browser(text="u_i NA")
    
    
    # -------------------------------------- 4. fit RF model
    
    
    min_wgt <- min(dd$wgt_prime)
    max_wgt <- max(dd$wgt_prime)
    
    training_dataset <- dd[, c("u_i", my_predictors, "wgt_prime")]
    
    RF_obj <- fit_h2o_RF(dependent_variable = "u_i", 
                         predictors = my_predictors, 
                         training_dataset = training_dataset, 
                         no_trees = no_trees, 
                         min_node_size = min_node_size, 
                         my_weights = "wgt_prime", 
                         model_nm = RF_obj_name)
      
    RF_ms_i <- h2o.mse(RF_obj)
    
    
    # -------------------------------------- 5. make new pixel level predictions
    
    
    p_i <- make_h2o_predictions(RF_obj, dd, my_predictors)
    
    n_NA_pred <- sum(is.na(p_i))
    
    dd$p_i <- ifelse(is.na(p_i), 0, p_i)
	
    if(sum(is.na(dd$p_i)) > 0) browser(text="pred NA")

    mp_nm <- sprintf("%s_iter_%s%s", map_name, i, ".png")
    
    quick_raster_map(dd, map_path, mp_nm) 
      
    
    # -------------------------------------- 6. calculate pixel level sum of square
    
    
    ss_i <- sum(dd$wgt_prime * (dd$p_i - dd$u_i)^2)
    
    
    # --------------------------------------  7. calculate population weighted mean of pixel level predictions
    
    
    p_i_by_adm <- dd %>% group_by_(.dots = grp_flds)
    
    mean_p_i <- p_i_by_adm %>% summarise(mean_p_i = sum(p_i * pop_weight))
    
    aa <- inner_join(adm_dataset, mean_p_i)
    
    
    # -------------------------------------- 8. calculate admin unit level sum of square
    
    
    ss_j <- sum(aa$new_weight * (aa$mean_p_i - aa$o_j)^2)
    
    
    # --------------------------------------
    
    
    out_mat[i,] <- c(RF_ms_i, ss_i, ss_j, min_wgt, max_wgt, n_NA_pred)
    
    pxl_dataset$p_i <- dd$p_i
    
  }
  
  write_out_rds(dd, sq_pr_path, sq_pr_name)
  h2o.saveModel(RF_obj, RF_obj_path, force = TRUE)
  write_out_rds(out_mat, diagn_tab_path, diagn_tab_name)
  
  out <- make_h2o_predictions(RF_obj, pxl_dataset_full, my_predictors)
  
  h2o.shutdown(prompt = FALSE)
  
  out
}
