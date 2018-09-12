exp_max_algorithm <- function(parms, 
                              orig_dataset, 
                              pxl_dataset,
                              my_predictors, 
                              grp_flds, 
                              var_to_fit,
                              map_col,
                              RF_obj_path = NULL, 
                              RF_obj_name = NULL,
                              diagn_tab_path = NULL, 
                              diagn_tab_name = NULL,
                              map_path = NULL, 
                              map_name = NULL, 
                              sct_plt_path = NULL, 
                              train_dts_path = NULL, 
                              train_dts_name = NULL,
                              adm_dataset = NULL){
  
  
  niter <- parms$EM_iter
  l_f <- parms$pseudoAbs_value
  no_trees <- parms$no_trees
  min_node_size <- parms$min_node_size
  foi_offset <- parms$foi_offset
  
  l_f_2 <- l_f + foi_offset
  zero_2 <- foi_offset
    
  
  # pre processing ------------------------------------------------------------
  
  
  diagnostics <- c("RF_ms_i", "ss_i", "ss_j", "min_wgt", "max_wgt", "n_NA_pred", "r_av_sqr", "r_adm")
  
  out_mat <- matrix(0, nrow = niter, ncol = length(diagnostics))
  
  colnames(out_mat) <- diagnostics
  
  for (i in seq_len(niter)){
    
    # browser()
    
    cat("iteration =", i, "\n")
    
    
    # 1. calculate scaling factors -------------------------------------------- 
    
    
    p_i_by_adm <- pxl_dataset %>% group_by_(.dots = grp_flds)
    
    a_sum <- p_i_by_adm %>% summarise(a_sum = sum(pop_weight * p_i))
    
    dd <- left_join(pxl_dataset, a_sum)
    
    dd$wgt_prime <- (dd$pop_weight / dd$p_i) * dd$a_sum
    # dd$wgt_prime <- dd$pop_weight
    
    dd[dd$type == "serology" & dd$new_weight == 1, "wgt_prime"] <- 1
    
    
    # 2. modify the scaling factors to account for background data ------------ 
    
    
    dd$wgt_prime <- dd$wgt_prime * dd$new_weight

    
    # 3. calculate new pseudo data value -------------------------------------- 
    
    
    psAbs <- dd$type == "pseudoAbsence"
    
    u_i <- rep(0, nrow(dd))
    
    if(var_to_fit == "FOI"){
      
      # u_i[!psAbs] <- (((dd$o_j[!psAbs] - l_f) * (dd$p_i[!psAbs] - l_f)) / (dd$a_sum[!psAbs] - l_f)) + l_f # when using only pop prop weights
      u_i[!psAbs] <- (dd$o_j[!psAbs] * dd$p_i[!psAbs]) / dd$a_sum[!psAbs] # when using updating weights
      u_i[psAbs] <- ifelse(dd$p_i[psAbs] > zero_2, l_f_2, dd$p_i[psAbs])
      
    } else {
      
      u_i[!psAbs] <- (dd$o_j[!psAbs] * dd$p_i[!psAbs]) / dd$a_sum[!psAbs]
      u_i[psAbs] <- ifelse(dd$p_i[psAbs] > 1, l_f, dd$p_i[psAbs])
      
    }
    
    u_i[dd$type == "serology" & dd$new_weight == 1] <- dd$o_j[dd$type == "serology" & dd$new_weight == 1]
    
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
    

    # 5. make new pixel level predictions ------------------------------------- 
    
    
    p_i <- make_ranger_predictions(RF_obj, dd, my_predictors)
    
    n_NA_pred <- sum(is.na(p_i))
    
    dd$p_i <- p_i

    
    # map of square predictions -----------------------------------------------  
    
    
    dd_1 <- dd
    
    if(var_to_fit == "FOI"){
      
      dd_1$p_i <- dd$p_i - foi_offset  
    
    } 
    
    # write.csv(dd_1, paste0("dd_", i, ".csv"), row.names = FALSE)
    
    if(!is.null(map_path)){
      mp_nm <- sprintf("%s_iter_%s%s", map_name, i, ".png")
      
      quick_raster_map(pred_df = dd_1, 
                     statistic = "p_i", 
                     my_col = map_col,
                     out_pt = map_path, 
                     out_name = mp_nm) 
    }
    
    
    # create a copy for obs vs preds plot and SS calculation ------------------   
    
    
    dd_2 <- dd
    
    
    # fix 20 km predictions ---------------------------------------------------  
    
    
    if(var_to_fit == "FOI"){
      
      dd_2$u_i[psAbs] <- zero_2 
      dd_2$p_i[psAbs] <- ifelse(dd_2$p_i[psAbs] < zero_2, zero_2, dd_2$p_i[psAbs]) 
      
    } else {
      
      dd_2$u_i[psAbs] <- l_f
      dd_2$p_i[psAbs] <- ifelse(dd_2$p_i[psAbs] < 1, l_f, dd_2$p_i[psAbs])
      
    }
  

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
      
      cc$o_j[psAbs_adm] <- zero_2   
      cc$adm_pred[psAbs_adm] <- ifelse(cc$adm_pred[psAbs_adm] < zero_2, 
                                       zero_2, 
                                       cc$adm_pred[psAbs_adm])
      
    } else {
      
      cc$adm_pred[psAbs_adm] <- ifelse(cc$adm_pred[psAbs_adm] < 1, 
                                       l_f, 
                                       cc$adm_pred[psAbs_adm])
      
    }
    

    # 7. calculate population weighted mean of pixel level predictions -------- 
    

    p_i_by_adm <- dd_2 %>% group_by_(.dots = grp_flds)
    
    mean_p_i <- p_i_by_adm %>% summarise(mean_p_i = sum(p_i * pop_weight))
    
    aa <- inner_join(cc, mean_p_i)
    
    aa[aa$type == "serology" & aa$new_weight == 1, "mean_p_i"] <- dd_2[dd_2$type == "serology" & dd_2$new_weight == 1, "p_i"]
    # write.csv(aa, paste0("aa_", i, ".csv"), row.names = FALSE)
    
    if(!is.null(sct_plt_path)){
      
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
      
    }
    
    
    # 8. calculate admin unit level sum of square ----------------------------- 
    
      
    ss_j <- sum(aa$new_weight * (aa$mean_p_i - aa$o_j)^2)
    
    
    # calculate correlation of obs vs pixel and adm predictions ---------------
    
    
    r_av_sqr <- calculate_wgt_cor(aa, "o_j", "mean_p_i")
    r_adm <- calculate_wgt_cor(aa, "o_j", "adm_pred")
    
    
    # --------------------------------------
    
  
    out_mat[i,] <- c(RF_ms_i, ss_i, ss_j, min_wgt, max_wgt, n_NA_pred, r_av_sqr, r_adm)
    
    pxl_dataset$p_i <- dd$p_i
    
  }
  
  if(!is.null(RF_obj_path)){
    write_out_rds(RF_obj, RF_obj_path, RF_obj_name)
  }

  if(!is.null(diagn_tab_path)){
    write_out_rds(out_mat, diagn_tab_path, diagn_tab_name)
  }
  
  if(!is.null(train_dts_path)){
    write_out_rds(training_dataset, train_dts_path, train_dts_name)
  }
  
  RF_obj
}

exp_max_algorithm_boot <- function(i, 
                                   parms,
                                   boot_samples, 
                                   my_preds, 
                                   grp_flds, 
                                   map_col,
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
  
  
  # define variables ---------------------------------------------------------- 
  
  
  psAbs <- parms$pseudoAbs_value
  var_to_fit <- parms$dependent_variable
  foi_offset <- parms$foi_offset
  
  res <- (1 / 120) * parms$resample_grid_size
  
  pxl_dts_nm <- paste0("sample_", i, ".rds")
  
  
  # load bootstrapped data sets -----------------------------------------------  
  
  
  pxl_dts_boot <- readRDS(file.path(pxl_dts_pt, pxl_dts_nm))
  
  foi_data_boot <- boot_samples[[i]]
  
  
  # get output name -----------------------------------------------------------  
  
  
  a <- RF_obj_name[i]
  b <- diagn_tab_name[i]
  ee <- map_name[i] 
  gg <- train_dts_name[i]
  cc <- file.path(map_path, paste0("sample_", i))
  ff <- file.path(sct_plt_path, paste0("sample_", i))
  
  
  # pre process the bootstrapped foi data set --------------------------------- 
  
  
  if(var_to_fit == "FOI"){
    
    names(foi_data_boot)[names(foi_data_boot) == "FOI"] <- "o_j"
    
  } else {
    
    names(foi_data_boot)[names(foi_data_boot) == var_to_fit] <- "o_j"
    
  }
  
  foi_data_boot[foi_data_boot$type == "pseudoAbsence", "o_j"] <- psAbs
  
  if(var_to_fit == "FOI"){
    
    foi_data_boot[, "o_j"] <- foi_data_boot[, "o_j"] + foi_offset
    
  }
  
  
  # attach original data and weights to square dataset ------------------------ 
  
  
  pxl_dts_boot <- inner_join(pxl_dts_boot, foi_data_boot[, c(grp_flds, "type", "new_weight")])  
  
  
  # fix serology new_weights ----------------------------------------------------
  

  pxl_dts_boot[pxl_dts_boot$type == "serology", "new_weight"] <- 0
  
  sero_points <- foi_data_boot[foi_data_boot$type == "serology", ]
  
  pxl_dts_boot$lat.int <- round(pxl_dts_boot$latitude / res)
  pxl_dts_boot$long.int <- round(pxl_dts_boot$longitude / res)
  
  sero_points$lat.int <- round(sero_points$latitude / res)
  sero_points$long.int <- round(sero_points$longitude / res)
  
  sero_points$cell <- 0
  sero_points$no_square <- 0
  
  for (j in seq_len(nrow(sero_points))){
    
    sero_long <- sero_points[j, "long.int"]
    sero_lat <- sero_points[j, "lat.int"]
    
    matches <- pxl_dts_boot$type == "serology" & pxl_dts_boot$lat.int == sero_lat & pxl_dts_boot$long.int == sero_long
    
    if(sum(matches) != 0){
      
      message(j)
      
      cell_id <- which(matches == TRUE)[1]
      sero_points[j, "cell"] <- cell_id
      pxl_dts_boot[cell_id, "new_weight"] <- 1
      
    } else {
      
      sero_points[j, "no_square"] <- 1
      
    }
    
  }

  missing_square <- sero_points[sero_points$no_square == 1, ]

  sero_pxl_no_dup <- pxl_dts_boot$type == "serology" & pxl_dts_boot$new_weight == 1
  
  pxl_dts_boot_2 <- pxl_dts_boot[!sero_pxl_no_dup, ]
  
  sero_pxl_dup <- pxl_dts_boot[sero_points$cell, ]
  
  sero_pxl_dup$unique_id <- sero_points$unique_id
  
  pxl_dts_boot_3 <- rbind(pxl_dts_boot_2, sero_pxl_dup)
  
  pxl_dts_boot_3 <- inner_join(pxl_dts_boot_3, foi_data_boot[, c(grp_flds, "o_j")])  
  
  
  # calculate population proportion weights ----------------------------------- 
  
  
  pxl_dts_grp <- pxl_dts_boot_3 %>% group_by_(.dots = grp_flds) 
  
  aa <- pxl_dts_grp %>% summarise(pop_sqr_sum = sum(population))
  
  pxl_dts_boot_3 <- left_join(pxl_dts_boot_3, aa)
  
  pxl_dts_boot_3$pop_weight <- pxl_dts_boot_3$population / pxl_dts_boot_3$pop_sqr_sum
  
  
  # run the EM ----------------------------------------------------------------  
  
  
  exp_max_algorithm(parms = parms, 
                    orig_dataset = foi_data_boot, 
                    pxl_dataset = pxl_dts_boot_3,
                    my_predictors = my_preds, 
                    grp_flds = grp_flds, 
                    var_to_fit = var_to_fit,
                    map_col = map_col,
                    RF_obj_path = RF_obj_path,
                    RF_obj_name = a,
                    diagn_tab_path = diagn_tab_path, 
                    diagn_tab_name = b,
                    map_path = cc, 
                    map_name = ee,
                    sct_plt_path = ff,
                    train_dts_path = train_dts_path,
                    train_dts_name = gg,
                    adm_dataset = adm_dataset)
  
}

EM_full_routine <- function(x, 
                            parms, 
                            all_squares, 
                            predictors, 
                            grp_flds_1, 
                            grp_flds_2,
                            adm_dataset, 
                            foi_data,
                            sqr_data){
  
  
  j <- x$exp_id 
  i <- x$rep_id
  var_to_fit <- x$var
  grid_size <- x$gs
  number_of_predictors <- x$no_pred
  
  cat("exp id =", j, "\n")
  cat("rep id =", i, "\n")
  cat("response variable =", var_to_fit, "\n")
  cat("grid size =", grid_size, "\n")
  cat("number of predictors =", number_of_predictors, "\n")
  
  no_trees <- parms$no_trees
  min_node_size <- parms$min_node_size
  psAbs <- parms$pseudoAbs_value
  foi_offset <- parms$foi_offset

  res <- (1 / 120) * parms$resample_grid_size
  
  my_dir <- paste0("grid_size_", grid_size)
  model_type <- paste0("boot_model_", j)
  
  in_path <- file.path("output", 
                       "EM_algorithm",
                       "bootstrap_models",
                       my_dir)
  
  out_name <- paste0("sample_", i, ".rds")
  
  all_pred_out_path <- file.path(in_path, model_type, "predictions_data")
  
  RF_out_path <- file.path(in_path, model_type, "optimized_model_objects")
  
  global_predictions_out_path <- file.path("output", 
                                           "predictions_world",
                                           "bootstrap_models",
                                           my_dir,
                                           model_type,
                                           "boot_samples")
  
  
  # load data -----------------------------------------------------------------
  
  
  boot_ls <- readRDS(file.path(in_path, "bootstrap_samples.rds"))
  
  
  # pre processing ------------------------------------------------------------
  
  
  foi_data_boot <- boot_ls[[i]]
  
  my_predictors <- predictors[seq_len(number_of_predictors)]
  
  cat(paste(c("My predictors are:", my_predictors), collapse = '\n'), "\n")
  
  if(var_to_fit == "FOI"){
    
    names(foi_data_boot)[names(foi_data_boot) == "FOI"] <- "o_j"
    
  } else {
    
    names(foi_data_boot)[names(foi_data_boot) == var_to_fit] <- "o_j"
    
  }
  
  foi_data_boot[foi_data_boot$type == "pseudoAbsence", "o_j"] <- psAbs
  
  if(var_to_fit == "FOI"){
    
    foi_data_boot[, "o_j"] <- foi_data_boot[, "o_j"] + foi_offset
    
  }
  
  training_dataset <- foi_data_boot[, c("o_j", my_predictors, "new_weight")]
  
  
  # filter, fit and predict ---------------------------------------------------
  
  
  jn_flds <- unique(c(grp_flds_1, grp_flds_2))
  
  sqr_data_boot <- inner_join(all_squares, foi_data_boot[, jn_flds])
  
  RF_obj <- fit_ranger_RF(dependent_variable = "o_j", 
                          predictors = my_predictors, 
                          training_dataset = training_dataset, 
                          no_trees = no_trees, 
                          min_node_size = min_node_size,
                          my_weights = "new_weight")
  
  p_i <- make_ranger_predictions(mod_obj = RF_obj, 
                                 dataset = sqr_data_boot, 
                                 sel_preds = my_predictors)
  
  sqr_data_boot$p_i <- p_i
  
  
  # attach original data and weights to square dataset ------------------------ 
  
  
  sqr_data_boot <- inner_join(sqr_data_boot, foi_data_boot[, c(grp_flds_1, "type", "new_weight")])  
  
  
  # fix serology new_weights --------------------------------------------------
  
  
  sqr_data_boot[sqr_data_boot$type == "serology", "new_weight"] <- 0
  
  sero_points <- foi_data_boot[foi_data_boot$type == "serology", ]
  
  sqr_data_boot$lat.int <- round(sqr_data_boot$latitude / res)
  sqr_data_boot$long.int <- round(sqr_data_boot$longitude / res)
  
  sero_points$lat.int <- round(sero_points$latitude / res)
  sero_points$long.int <- round(sero_points$longitude / res)
  
  sero_points$cell <- 0
  sero_points$no_square <- 0
  
  for (j in seq_len(nrow(sero_points))){
    
    sero_long <- sero_points[j, "long.int"]
    sero_lat <- sero_points[j, "lat.int"]
    
    matches <- sqr_data_boot$type == "serology" & sqr_data_boot$lat.int == sero_lat & sqr_data_boot$long.int == sero_long
    
    if(sum(matches) != 0){
      
      message(j)
      
      cell_id <- which(matches == TRUE)[1]
      sero_points[j, "cell"] <- cell_id
      sqr_data_boot[cell_id, "new_weight"] <- 1
      
    } else {
      
      sero_points[j, "no_square"] <- 1
      
    }
    
  }
  
  missing_square <- sero_points[sero_points$no_square == 1, ]

  sero_pxl_no_dup <- sqr_data_boot$type == "serology" & sqr_data_boot$new_weight == 1
  
  sqr_data_boot_2 <- sqr_data_boot[!sero_pxl_no_dup, ]
  
  sero_pxl_dup <- sqr_data_boot[sero_points$cell, ]
  
  sero_pxl_dup$unique_id <- sero_points$unique_id
  
  sqr_data_boot_3 <- rbind(sqr_data_boot_2, sero_pxl_dup)
  
  sqr_data_boot_3 <- inner_join(sqr_data_boot_3, foi_data_boot[, c(grp_flds_1, "o_j")])  
  
  
  # calculate population proportion weights ----------------------------------- 
  
  
  pxl_dts_grp <- sqr_data_boot_3 %>% group_by_(.dots = grp_flds) 
  
  aa <- pxl_dts_grp %>% summarise(pop_sqr_sum = sum(population))
  
  sqr_data_boot_3 <- left_join(sqr_data_boot_3, aa)
  
  sqr_data_boot_3$pop_weight <- sqr_data_boot_3$population / sqr_data_boot_3$pop_sqr_sum
  
  
  # run the EM ----------------------------------------------------------------
  
  
  RF_obj_optim <- exp_max_algorithm(parms = parms, 
                                    orig_dataset = foi_data_boot, 
                                    pxl_dataset = sqr_data_boot,
                                    pxl_dataset_full = sqr_data,
                                    my_predictors = my_predictors, 
                                    grp_flds = grp_flds_1, 
                                    var_to_fit = var_to_fit,
                                    RF_obj_path = RF_out_path, 
                                    RF_obj_name = RF_name,
                                    adm_dataset = adm_dataset)
  
  p_i_all <- make_ranger_predictions(RF_obj_optim, sqr_data, my_predictors)
  
  
  # pre process admin unit covariate dataset ----------------------------------
  
  
  adm_dts_2 <- remove_NA_rows(adm_dataset, my_predictors)
  
  adm_pred <- make_ranger_predictions(RF_obj_optim, adm_dts_2, my_predictors)
  
  if(var_to_fit == "FOI"){
    
    adm_pred <- adm_pred - foi_offset 
    p_i_all <- p_i_all - foi_offset
    
  }
  
  adm_dts_2$admin <- adm_pred
  
  fltr_adm <- inner_join(adm_dts_2, foi_data[, grp_flds_2])
  
  sqr_dts <- cbind(sqr_data[, c(grp_flds_2, "population")],
                   square = p_i_all)
  
  average_sqr <- average_up(sqr_dts, grp_flds_2, "square")
  
  df_lst <- list(foi_data[, c(grp_flds_2, "type", "o_j")],
                 fltr_adm[, c(grp_flds_2, "admin")],
                 average_sqr[, c(grp_flds_2, "square")])
  
  join_all <- Reduce(function(...) left_join(...), df_lst)
  
  ids <- unique(foi_data_boot$data_id)
  
  train_ids <- rep(0, nrow(foi_data))
  
  train_ids[ids] <- 1
  
  join_all$train <- train_ids
  
  write_out_rds(join_all, all_pred_out_path, out_name)
  
  global_predictions <- make_ranger_predictions(RF_obj_optim, all_squares, my_predictors)
  
  write_out_rds(global_predictions, global_predictions_out_path, out_name)
  
}
