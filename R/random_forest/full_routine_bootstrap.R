full_routine_bootstrap <- function(x, 
                                   parms,
                                   original_foi_data, 
                                   adm_covariates,
                                   data_squares,
                                   all_squares, 
                                   all_predictors){
  
  
  j <- x$exp_id 
  i <- x$rep_id
  var_to_fit <- x$var
  grid_size <- x$gs
  number_of_predictors <- x$no_pred
  
  parms$no_predictors <- number_of_predictors
  parms$dependent_variable <- var_to_fit
  
  cat("exp id =", j, "\n")
  cat("rep id =", i, "\n")
  cat("response variable =", var_to_fit, "\n")
  cat("grid size =", grid_size, "\n")
  cat("number of predictors =", number_of_predictors, "\n")
  
  model_type <- paste0("model_", j)
  aa <- paste0("sample_", i, ".rds")
  bb <- paste0("sample_", i, ".png")
  my_dir <- paste0("grid_size_", grid_size)
  grp_flds <- parms$grp_flds
  
  
  # output dir -----------------------------------------------------------------
  
  
  RF_out_path <- file.path("output", 
                           "EM_algorithm",
                           "bootstrap_models", 
                           model_type, 
                           "optimized_model_objects")
  
  diagno_out_path <- file.path("output", 
                               "EM_algorithm",
                               "bootstrap_models",
                               model_type, 
                               "diagnostics")
  
  diagno_fig_path <- file.path("figures", 
                               "EM_algorithm", 
                               "bootstrap_models",
                               model_type, 
                               "diagnostics")
  
  train_dts_path <- file.path("output", 
                              "EM_algorithm",
                              "bootstrap_models", 
                              model_type, 
                              "training_datasets")
  
  all_pred_out_path <- file.path("output", 
                                 "EM_algorithm",
                                 "bootstrap_models", 
                                 model_type, 
                                 "adm_foi_predictions")
  
  data_sqr_predictions_out_path <- file.path("output", 
                                             "EM_algorithm",
                                             "bootstrap_models", 
                                             model_type, 
                                             "data_square_predictions")
  
  global_predictions_out_path <- file.path("output", 
                                           "predictions_world",
                                           "bootstrap_models",
                                           model_type,
                                           "boot_samples")
  
  
  # load bootstrap samples ----------------------------------------------------
  
  
  boot_samples <- readRDS(file.path("output", 
                                    "EM_algorithm",
                                    "bootstrap_models", 
                                    my_dir, 
                                    "bootstrap_samples.rds"))
  
  
  # ---------------------------------------------------------------------------
  
  
  my_predictors <- all_predictors[1:number_of_predictors]
  
  orig_foi <- preprocess_adm_data(parms, original_foi_data)
  
  parms_2 <- parms
  parms_2$grp_flds <- c(parms_2$grp_flds, "data_id")
  
  foi_data <- boot_samples[[i]]
  
  foi_data_2 <- preprocess_adm_data(parms, foi_data)
  
  pxl_data_2 <- preprocess_pxl_data(parms_2, foi_data_2, all_squares)
  
  training_dataset <- foi_data_2[, c(var_to_fit, my_predictors, "new_weight")]
  
  RF_obj <- fit_ranger_RF(parms = parms,
                          dependent_variable = var_to_fit, 
                          predictors = my_predictors, 
                          training_dataset = training_dataset, 
                          my_weights = "new_weight")
  
  p_i <- make_ranger_predictions(RF_obj, pxl_data_2, my_predictors)
  
  pxl_data_2$p_i <- p_i
  
  names(foi_data_2)[names(foi_data_2) == var_to_fit] <- "o_j"
  
  pxl_data_3 <- inner_join(pxl_data_2, foi_data_2[, c(grp_flds, "o_j")])
  
  pxl_dts_grp <- pxl_data_3 %>% 
    group_by(.dots = grp_flds) %>% 
    summarise(pop_sqr_sum = sum(population))
  
  pxl_data_4 <- left_join(pxl_data_3, pxl_dts_grp)
  
  pxl_data_4$pop_weight <- pxl_data_4$population / pxl_data_4$pop_sqr_sum

  RF_obj_optim <- exp_max_algorithm(parms = parms, 
                                    orig_dataset = foi_data_2, 
                                    pxl_dataset = pxl_data_4,
                                    my_predictors = my_predictors, 
                                    RF_obj_path = RF_out_path,
                                    RF_obj_name = aa,
                                    diagn_tab_path = diagno_out_path, 
                                    diagn_tab_name = aa,
                                    train_dts_path = train_dts_path,
                                    train_dts_name = aa,
                                    adm_dataset = adm_covariates)
  
  prediction_set <- make_ranger_predictions(RF_obj_optim, 
                                            data_squares, 
                                            my_predictors)
  
  global_predictions <- make_ranger_predictions(RF_obj_optim, 
                                                all_squares, 
                                                my_predictors)
  
  data_to_plot <- readRDS(file.path(diagno_out_path, aa))
  
  plot_EM_diagnostics(data_to_plot, diagno_fig_path, bb)
  
  join_all <- join_predictions(parms = parms, 
                               foi_dataset = orig_foi, 
                               RF_obj = RF_obj_optim, 
                               adm_dataset = adm_covariates,
                               my_predictors = my_predictors, 
                               all_sqr_predictions = prediction_set, 
                               sqr_dataset = data_squares)
  
  ids <- unique(foi_data_2$data_id)
  
  train_ids <- rep(0, nrow(orig_foi))
  
  train_ids[ids] <- 1
  
  join_all$train <- train_ids
  
  write_out_rds(join_all, all_pred_out_path, aa)
  
  write_out_rds(global_predictions, global_predictions_out_path, aa)
  
}
