exp_max_algorithm <- function(parms, 
                              orig_dataset, 
                              pxl_dataset,
                              pxl_dataset_full, 
                              my_predictors, 
                              grp_flds, 
                              RF_obj_path, 
                              RF_obj_name,
                              diagn_tab_path, 
                              diagn_tab_name,
                              map_path, 
                              map_name, 
                              sct_plt_path, 
                              var_to_fit, 
                              train_dts_path, 
                              train_dts_name,
                              adm_dataset = NULL){
  
  
  niter <- parms$EM_iter
  l_f <- parms$pseudoAbs_value
  no_trees <- parms$no_trees
  min_node_size <- parms$min_node_size
  
  
  # ---------------------------------------------------------------------------
  
  
  diagnostics <- c("RF_ms_i", "ss_i", "ss_j", "min_wgt", "max_wgt", "n_NA_pred", "r_av_sqr", "r_adm")
  
  out_mat <- matrix(0, nrow = niter, ncol = length(diagnostics))
  
  colnames(out_mat) <- diagnostics
  
  # h2o.init(ignore_config = TRUE)
  
  for (i in seq_len(niter)){
    
    # browser()
    
    cat("iteration =", i, "\n")
    
    
    # 1. calculate scaling factors -------------------------------------------- 
    
    
    p_i_by_adm <- pxl_dataset %>% group_by_(.dots = grp_flds)
    
    a_sum <- p_i_by_adm %>% summarise(a_sum = sum(pop_weight * p_i))
    
    dd <- left_join(pxl_dataset, a_sum)
    
    dd$wgt_prime <- (dd$pop_weight / dd$p_i) * dd$a_sum
    # dd$wgt_prime <- dd$pop_weight
    
    
    # 2. modify the scaling factors to account for background data ------------ 
    
    
    dd$wgt_prime <- dd$wgt_prime * dd$new_weight
    
    
    # 3. calculate new pseudo data value -------------------------------------- 
    
    
    psAbs <- dd$type == "pseudoAbsence"
    
    u_i <- rep(0, nrow(dd))
    
    if(var_to_fit == "FOI"){
      
      u_i[!psAbs] <- (((dd$o_j[!psAbs] - l_f) * (dd$p_i[!psAbs] - l_f)) / (dd$a_sum[!psAbs] - l_f)) + l_f 
      u_i[psAbs] <- ifelse(dd$p_i[psAbs] > 0, l_f, dd$p_i[psAbs])
      
    } else {
      
      u_i[!psAbs] <- (dd$o_j[!psAbs] * dd$p_i[!psAbs]) / dd$a_sum[!psAbs]
      u_i[psAbs] <- ifelse(dd$p_i[psAbs] > 1, l_f, dd$p_i[psAbs])
      
    }
    
    dd$u_i <- u_i
    
    
    # 4. fit RF model ---------------------------------------------------------
    
    
    min_wgt <- min(dd$wgt_prime)
    max_wgt <- max(dd$wgt_prime)
    
    training_dataset <- dd[, c("u_i", my_predictors, "wgt_prime")]
    
    RF_obj <- fit_ranger_RF(dependent_variable = "u_i", 
                            predictors = my_predictors, 
                            training_dataset = training_dataset, 
                            no_trees = no_trees, 
                            min_node_size = min_node_size, 
                            my_weights = "wgt_prime")
    
    RF_ms_i <- RF_obj$prediction.error
    # RF_ms_i <- h2o.mse(RF_obj)
    
    
    # 5. make new pixel level predictions ------------------------------------- 
    
    
    p_i <- make_ranger_predictions(RF_obj, dd, my_predictors)
    
    n_NA_pred <- sum(is.na(p_i))
    
    dd$p_i <- p_i
    
    
    # map of square predictions -----------------------------------------------  
    
    
    mp_nm <- sprintf("%s_iter_%s%s", map_name, i, ".png")
    
    quick_raster_map(pred_df = dd, 
                     statistic = "p_i", 
                     out_pt = map_path, 
                     out_name = mp_nm) 
    

    # create a copy for obs vs preds plot and SS calculation ------------------   
    
    
    dd_2 <- dd
    
    
    # plot of observed vs predicted square values -----------------------------  
    
    
    if(var_to_fit == "FOI"){
      
      dd_2$u_i[psAbs] <- 0 
      dd_2$p_i[psAbs] <- ifelse(dd_2$p_i[psAbs] < 0, 0, dd_2$p_i[psAbs]) 
      
    } else {
      
      dd_2$u_i[psAbs] <- l_f
      dd_2$p_i[psAbs] <- ifelse(dd_2$p_i[psAbs] < 1, l_f, dd_2$p_i[psAbs])
      
    }
    
    # sqr_sp_nm <- paste0("pred_vs_obs_sqr_iter_", i, ".png")
    # 
    # generic_scatter_plot(df = dd_2, 
    #                      x = "u_i", 
    #                      y = "p_i", 
    #                      file_name = sqr_sp_nm, 
    #                      file_path = sct_plt_path)  
    
    
    # 6. calculate pixel level sum of square ---------------------------------- 
    
    
    ss_i <- sum(dd_2$wgt_prime * (dd_2$p_i - dd_2$u_i)^2)
    
    
    # make admin unit level predictions ---------------------------------------  
    
    
    if(!is.null(adm_dataset)) {
      
      adm_dataset$adm_pred <- make_ranger_predictions(RF_obj, 
                                                      adm_dataset, 
                                                      my_predictors)
      
      cc <- inner_join(orig_dataset, adm_dataset[, c("ID_0", "ID_1", "adm_pred")])
      
    } else {
      
      cc <- orig_dataset
      
    }
    
    psAbs_adm <- cc$type == "pseudoAbsence" 
    
    if(var_to_fit == "FOI"){
      
      cc$o_j[psAbs_adm] <- 0    
      cc$adm_pred[psAbs_adm] <- ifelse(cc$adm_pred[psAbs_adm] < 0, 
                                       0, 
                                       cc$adm_pred[psAbs_adm])
      
    } else{
      
      cc$adm_pred[psAbs_adm] <- ifelse(cc$adm_pred[psAbs_adm] < 1, 
                                       l_f, 
                                       cc$adm_pred[psAbs_adm])
      
    }
    
    # 7. calculate population weighted mean of pixel level predictions -------- 
    
    
    p_i_by_adm <- dd_2 %>% group_by_(.dots = grp_flds)
    
    mean_p_i <- p_i_by_adm %>% summarise(mean_p_i = sum(p_i * pop_weight))
    
    aa <- inner_join(cc, mean_p_i)
    
    
    # plot of observed admin values vs pop-wgt average predicted square values   
    

    av_sqr_sp_nm <- paste0("pred_vs_obs_av_sqr_iter_", i, ".png")
    
    generic_scatter_plot(df = aa, 
                         x = "o_j", 
                         y = "mean_p_i", 
                         file_name = av_sqr_sp_nm, 
                         file_path = sct_plt_path)  
    
    
    # plot of observed vs predicted admin values ------------------------------ 
    
    
    adm_sp_nm <- paste0("pred_vs_obs_adm_iter_", i, ".png")
    
    generic_scatter_plot(df = aa, 
                         x = "o_j", 
                         y = "adm_pred", 
                         file_name = adm_sp_nm, 
                         file_path = sct_plt_path)  
    
    
    # 8. calculate admin unit level sum of square ----------------------------- 
    
      
    ss_j <- sum(aa$new_weight * (aa$mean_p_i - aa$o_j)^2)
    
    
    # calculate correlation of obs vs pixel and adm predictions ---------------
    
    
    r_av_sqr <- calculate_wgt_cor(aa, "o_j", "mean_p_i")
    r_adm <- calculate_wgt_cor(aa, "o_j", "adm_pred")
    
    
    # --------------------------------------
    
  
    out_mat[i,] <- c(RF_ms_i, ss_i, ss_j, min_wgt, max_wgt, n_NA_pred, r_av_sqr, r_adm)
    
    pxl_dataset$p_i <- dd$p_i
    
  }
  
  # h2o.saveModel(RF_obj, RF_obj_path, force = TRUE)
  write_out_rds(RF_obj, RF_obj_path, RF_obj_name)
  write_out_rds(out_mat, diagn_tab_path, diagn_tab_name)
  write_out_rds(training_dataset, train_dts_path, train_dts_name)
  
  out <- make_ranger_predictions(RF_obj, pxl_dataset_full, my_predictors)
  
  # h2o.shutdown(prompt = FALSE)
  
  out
}

exp_max_algorithm_boot <- function(i, 
                                   parms,
                                   boot_samples, 
                                   pxl_dataset_orig, 
                                   my_preds, 
                                   grp_flds, 
                                   RF_obj_path, 
                                   RF_obj_name,
                                   diagn_tab_path, 
                                   diagn_tab_name,
                                   map_path, 
                                   map_name, 
                                   sct_plt_path,
                                   adm_dataset, 
                                   pxl_dts_pt,
                                   train_dts_path,
                                   train_dts_name){
  

  # browser()
  
  
  # ---------------------------------------- define variables
  
  
  psAbs <- parms$pseudoAbs_value
  var_to_fit <- parms$dependent_variable
  
  pxl_dts_nm <- paste0("env_vars_and_foi_20km_", i, ".rds")
  
  
  # ---------------------------------------- load bootstrapped data sets 
  
  
  pxl_dts_boot <- readRDS(file.path(pxl_dts_pt, pxl_dts_nm))
  
  foi_data_boot <- boot_samples[[i]]
  
  
  # ---------------------------------------- get output name 
  
  
  a <- RF_obj_name[i]
  b <- diagn_tab_name[i]
  cc <- map_path[i]  
  ee <- map_name[i] 
  ff <- sct_plt_path[i]
  gg <- train_dts_name[i]
  
  
  # ---------------------------------------- pre process the bootstrapped foi data set
  
  
  if(var_to_fit == "FOI"){
    
    names(foi_data_boot)[names(foi_data_boot) == "FOI"] <- "o_j"
    
  } else {
    
    names(foi_data_boot)[names(foi_data_boot) == var_to_fit] <- "o_j"
    
  }
  
  foi_data_boot[foi_data_boot$type == "pseudoAbsence", "o_j"] <- psAbs
  
  
  # ---------------------------------------- pre process the square data set
  
  
  pxl_dts_grp <- pxl_dts_boot %>% group_by_(.dots = grp_flds) 
  
  # aa <- pxl_dts_grp %>% summarise(pop_sqr_sum = sum(population))
  ncells <- pxl_dts_grp %>% summarise(n_sqr = n())
  
  # pxl_dts_boot <- left_join(pxl_dts_boot, aa)
  pxl_dts_boot <- left_join(pxl_dts_boot, ncells)
  
  # pxl_dts_boot$pop_weight <- pxl_dts_boot$population / pxl_dts_boot$pop_sqr_sum
  pxl_dts_boot$pop_weight <- 1 / pxl_dts_boot$n_sqr
  
  
  # ---------------------------------------- attach original data and weigths to square dataset
  
  
  pxl_dts_boot <- inner_join(pxl_dts_boot, foi_data_boot[, c(grp_flds, "o_j", "new_weight")])
  
  
  # ---------------------------------------- run the EM 
  
  
  exp_max_algorithm(parms = parms, 
                    orig_dataset = foi_data_boot, 
                    pxl_dataset = pxl_dts_boot,
                    pxl_dataset_full = pxl_dataset_orig,
                    my_predictors = my_preds, 
                    grp_flds = grp_flds, 
                    RF_obj_path = RF_obj_path,
                    RF_obj_name = a,
                    diagn_tab_path = diagn_tab_path, 
                    diagn_tab_name = b,
                    map_path = cc, 
                    map_name = ee,
                    sct_plt_path = ff,
                    var_to_fit = var_to_fit,
                    train_dts_path = train_dts_path,
                    train_dts_name = gg,
                    adm_dataset = adm_dataset)
  
}
