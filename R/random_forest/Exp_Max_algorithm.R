exp_max_algorithm <- function(
  niter, orig_dataset, pxl_dataset,
  pxl_dataset_full, l_f,
  my_predictors, grp_flds, 
  RF_obj_path, RF_obj_name,
  diagn_tab_path, diagn_tab_name,
  map_path, map_name, 
  sct_plt_path, adm_dataset = NULL){
  
  diagnostics <- c("RF_ms_i", "ss_i", "ss_j", "min_wgt", "max_wgt", "n_NA_pred")
  
  out_mat <- matrix(0, nrow = niter, ncol = length(diagnostics))
  
  colnames(out_mat) <- diagnostics
  
  h2o.init(ignore_config = TRUE)
    
  for (i in seq_len(niter)){
    
    #browser()
    
    cat("iteration =", i, "\n")
    
    
    # -------------------------------------- 1. calculate scaling factors
    
    
    p_i_by_adm <- pxl_dataset %>% group_by_(.dots = grp_flds)
    
    a_sum <- p_i_by_adm %>% summarise(a_sum = sum(pop_weight * p_i))
    
    dd <- left_join(pxl_dataset, a_sum)
    
    dd$wgt_prime <- dd$pop_weight
    
    
    # -------------------------------------- 2. modify the scaling factors to account for background data
    
    
    dd$wgt_prime <- dd$wgt_prime * dd$new_weight
     

    # -------------------------------------- 3. calculate new pseudo data value
    
    
    psAbs <- dd$type == "pseudoAbsence"
  
    u_i <- rep(0, nrow(dd))
      
    u_i[!psAbs] <- (((dd$o_j[!psAbs] - l_f) * (dd$p_i[!psAbs] - l_f)) / (dd$a_sum[!psAbs] - l_f)) + l_f 
    
    u_i[psAbs] <- ifelse(dd$p_i[psAbs] > 0, l_f, dd$p_i[psAbs])
    
    #u_i <- (((dd$o_j - l_f) * (dd$p_i - l_f)) / (dd$a_sum - l_f)) + l_f
    
    dd$u_i <- u_i
    
    
    # -------------------------------------- 4. fit RF model
    
    
    min_wgt <- min(dd$wgt_prime)
    max_wgt <- max(dd$wgt_prime)
    
    training_dataset <- dd[, c("u_i", my_predictors, "wgt_prime")]
    
    RF_obj <- fit_h2o_RF(dependent_variable = "u_i", 
                         predictors = my_predictors, 
                         training_dataset = training_dataset, 
                         no_trees = 500, 
                         min_node_size = 20, 
                         my_weights = "wgt_prime", 
                         model_nm = RF_obj_name)
      
    RF_ms_i <- h2o.mse(RF_obj)
    
    
    # -------------------------------------- 5. make new pixel level predictions
    
    
    p_i <- make_h2o_predictions(RF_obj, dd, my_predictors)
    
    n_NA_pred <- sum(is.na(p_i))
    
    dd$p_i <- p_i

    
    # -------------------------------------- map of square predictions 
    
    
    mp_nm <- sprintf("%s_iter_%s%s", map_name, i, ".png")
    
    quick_raster_map(dd, map_path, mp_nm) 
    
    
    # -------------------------------------- create a copy for obs vs preds plot and SS calculation  
    
    
    dd_2 <- dd
      
    
    # -------------------------------------- plot of observed vs predicted square values 

    
    dd_2$u_i[psAbs] <- 0
    dd_2$p_i[psAbs] <- ifelse(dd_2$p_i[psAbs] < 0, 0, dd_2$p_i[psAbs])
    
    sqr_sp_nm <- paste0("pred_vs_obs_sqr_iter_", i, ".png")
    
    generic_scatter_plot(df = dd_2, 
                         x = "u_i", 
                         y = "p_i", 
                         file_name = sqr_sp_nm, 
                         file_path = sct_plt_path)  
    
    
    # -------------------------------------- 6. calculate pixel level sum of square
    
    
    ss_i <- sum(dd_2$wgt_prime * (dd_2$p_i - dd_2$u_i)^2)
    
    
    # -------------------------------------- make admin unit level predictions 
    
    
    if(!is.null(adm_dataset)) {
      
      adm_dataset$adm_pred <- make_h2o_predictions(RF_obj, adm_dataset, my_predictors)
      
      cc <- inner_join(orig_dataset, adm_dataset[, c("ID_0", "ID_1", "adm_pred")])
      
    } else {
      
      cc <- orig_dataset
    
    }
    
    psAbs_adm <- cc$type == "pseudoAbsence" 
      
    cc$o_j[psAbs_adm] <- 0
    cc$adm_pred[psAbs_adm] <- ifelse(cc$adm_pred[psAbs_adm] < 0, 0, cc$adm_pred[psAbs_adm])
    
    
    # -------------------------------------- 7. calculate population weighted mean of pixel level predictions
    
    
    p_i_by_adm <- dd %>% group_by_(.dots = grp_flds)
    
    mean_p_i <- p_i_by_adm %>% summarise(mean_p_i = sum(p_i * pop_weight))
    
    aa <- inner_join(cc, mean_p_i)
    
    
    # -------------------------------------- plot of observed admin values vs pop-wgt average predicted square values 
    
    
    av_sqr_sp_nm <- paste0("pred_vs_obs_av_sqr_iter_", i, ".png")
    
    generic_scatter_plot(df = aa, 
                         x = "o_j", 
                         y = "mean_p_i", 
                         file_name = av_sqr_sp_nm, 
                         file_path = sct_plt_path)  
    
    
    # -------------------------------------- plot of observed vs predicted admin values
    
    
    adm_sp_nm <- paste0("pred_vs_obs_adm_iter_", i, ".png")
    
    generic_scatter_plot(df = aa, 
                         x = "o_j", 
                         y = "adm_pred", 
                         file_name = adm_sp_nm, 
                         file_path = sct_plt_path)  

        
    # -------------------------------------- 8. calculate admin unit level sum of square
    

    #neg_w_mean <- aa$mean_p_i < 0
    
    #S <- rep(0, nrow(aa)) 
      
    #S[!neg_w_mean] <- (aa$mean_p_i[!neg_w_mean] - aa$o_j[!neg_w_mean])^2 
    
    #ss_j <- sum(aa$new_weight * S)
    
    ss_j <- sum(aa$new_weight * (aa$mean_p_i - aa$o_j)^2)
    
    
    # --------------------------------------
    
    
    out_mat[i,] <- c(RF_ms_i, ss_i, ss_j, min_wgt, max_wgt, n_NA_pred)
    
    pxl_dataset$p_i <- dd$p_i
    
  }
  
  h2o.saveModel(RF_obj, RF_obj_path, force = TRUE)
  write_out_rds(out_mat, diagn_tab_path, diagn_tab_name)
  
  out <- make_h2o_predictions(RF_obj, pxl_dataset_full, my_predictors)
  
  h2o.shutdown(prompt = FALSE)
  
  out
}
