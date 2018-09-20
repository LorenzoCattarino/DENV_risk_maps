EM_full_routine <- function(x, 
                            parms, 
                            data_squares,
                            all_squares, 
                            predictors, 
                            grp_flds_1, 
                            grp_flds_2,
                            adm_dataset, 
                            foi_data){
  
  
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
  
  if(var_to_fit == "FOI") {
    
    psAbs <- parms$pseudoAbs_value[1]
    
  } else {
    
    psAbs <- parms$pseudoAbs_value[2]
    
  }
  
  cat("pseudo absence value =", psAbs, "\n")
  
  foi_offset <- parms$foi_offset
  
  res <- (1 / 120) * parms$resample_grid_size
  
  my_dir <- paste0("grid_size_", grid_size)
  
  model_type <- paste0("model_", j)
  
  in_path <- file.path("output", 
                       "EM_algorithm",
                       "bootstrap_models")
  
  out_name <- paste0("sample_", i, ".rds")
  
  all_pred_out_path <- file.path(in_path, model_type, "data_admin_predictions")
  
  RF_out_path <- file.path(in_path, model_type, "optimized_model_objects")
  diagn_out_path <- file.path(in_path, model_type, "diagnostics")
  train_dts_path <- file.path(in_path, model_type, "training_datasets")
  
  map_pth <- file.path("figures", 
                       "EM_algorithm",
                       "bootstrap_models",
                       model_type, 
                       "maps")
  
  sct_plt_pth <- file.path("figures", 
                           "EM_algorithm",
                           "bootstrap_models",
                           model_type, 
                           "iteration_fits")
  
  sqr_data_predictions_out_path <- file.path(in_path, model_type, "square_data_predictions")
  
  global_predictions_out_path <- file.path("output", 
                                           "predictions_world",
                                           "bootstrap_models",
                                           model_type,
                                           "boot_samples")
  
  
  # load data -----------------------------------------------------------------
  
  
  boot_ls <- readRDS(file.path(in_path, my_dir, "bootstrap_samples.rds"))
  
  
  # pre process ---------------------------------------------------------------
  
  foi_data_boot <- boot_ls[[i]]
 
  names(foi_data_boot)[names(foi_data_boot) == var_to_fit] <- "o_j"
  
  foi_data_boot[foi_data_boot$type == "pseudoAbsence", "o_j"] <- psAbs
  
  if(var_to_fit == "FOI"){
    
    foi_data_boot[, "o_j"] <- foi_data_boot[, "o_j"] + foi_offset
    
  }
  
  names(foi_data)[names(foi_data) == var_to_fit] <- "o_j"
  
  foi_data[foi_data$type == "pseudoAbsence", "o_j"] <- psAbs
   
  my_predictors <- predictors[seq_len(number_of_predictors)]
  
  cat(paste(c("My predictors are:", my_predictors), collapse = '\n'), "\n")
  
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
  
  
  pxl_dts_grp <- sqr_data_boot_3 %>% group_by_(.dots = grp_flds_1) 
  
  aa <- pxl_dts_grp %>% summarise(pop_sqr_sum = sum(population))
  
  sqr_data_boot_3 <- left_join(sqr_data_boot_3, aa)
  
  sqr_data_boot_3$pop_weight <- sqr_data_boot_3$population / sqr_data_boot_3$pop_sqr_sum
  
  
  # run the EM ----------------------------------------------------------------
  
  
  RF_obj_optim <- exp_max_algorithm(parms = parms, 
                                    orig_dataset = foi_data_boot, 
                                    pxl_dataset = sqr_data_boot_3,
                                    my_predictors = my_predictors, 
                                    grp_flds = grp_flds_1, 
                                    var_to_fit = var_to_fit,
                                    RF_obj_path = RF_out_path, 
                                    RF_obj_name = out_name,
                                    diagn_tab_path = diagn_out_path,
                                    diagn_tab_name = out_name,
                                    map_path = map_pth,
                                    sct_plt_path = sct_plt_pth,
                                    train_dts_path = train_dts_path,
                                    train_dts_name = out_name,
                                    adm_dataset = adm_dataset)
  
  p_i_all <- make_ranger_predictions(RF_obj_optim, data_squares, my_predictors)
  
  
  # pre process admin unit covariate dataset ----------------------------------
  
  
  adm_dts_2 <- remove_NA_rows(adm_dataset, my_predictors)
  
  adm_pred <- make_ranger_predictions(RF_obj_optim, adm_dts_2, my_predictors)
  
  if(var_to_fit == "FOI"){
    
    adm_pred <- adm_pred - foi_offset 
    p_i_all <- p_i_all - foi_offset
    
  }
  
  adm_dts_2$admin <- adm_pred
  
  fltr_adm <- inner_join(adm_dts_2, foi_data[, grp_flds_2])
  
  sqr_dts_2 <- cbind(data_squares, square = p_i_all)
  
  average_sqr <- average_up(sqr_dts_2, grp_flds_2, "square")
  
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
  
  write_out_rds(p_i_all, sqr_data_predictions_out_path, out_name)
  
  write_out_rds(global_predictions, global_predictions_out_path, out_name)
  
}
