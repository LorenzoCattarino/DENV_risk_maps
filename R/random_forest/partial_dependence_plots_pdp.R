wrapper_over_factor_combs <- function(x, predictor_rank, parms){
  
  model_type <- paste0("model_", x$exp_id)
  no_pred <- x$no_pred
  single_job_id <- x$rep_id
  
  cat("model type =", model_type, "\n")
  cat("number of predictors =", no_pred, "\n")
  cat("bootstrap sample =", single_job_id, "\n")
  
  model_in_pt <- file.path("output",
                           "EM_algorithm",
                           "bootstrap_models",
                           model_type,
                           "optimized_model_objects")
  
  train_dts_in_pt <- file.path("output",
                               "EM_algorithm",
                               "bootstrap_models",
                               model_type,
                               "training_datasets")
  
  pdp_out_pt <- file.path("output",
                          "EM_algorithm",
                          "bootstrap_models",
                          model_type,
                          "partial_dependence")
  
  v_imp_out_pt <- file.path("output",
                            "EM_algorithm",
                            "bootstrap_models",
                            model_type,
                            "variable_importance")
  
  my_predictors <- predictor_rank$name[1:no_pred]
  
  wrapper_over_bsamples(i = single_job_id,
                        parms = parms,
                        RF_obj_pt = model_in_pt,
                        tr_dts_pt = train_dts_in_pt,
                        par_dep_pt = pdp_out_pt,
                        var_imp_pt = v_imp_out_pt,
                        variables = my_predictors)
  
}

wrapper_over_bsamples <- function(i, 
                                  parms,
                                  RF_obj_pt, 
                                  tr_dts_pt, 
                                  par_dep_pt, 
                                  var_imp_pt, 
                                  variables){
  
  nm <- paste0("sample_", i, ".rds")

  parms$RF_obj_name <- nm
  parms$tr_dts_name <- nm
  parms$par_dep_name <- nm
  parms$var_imp_name <- nm
  
  calculate_par_dep(parms,
                    RF_obj_pt,
                    tr_dts_pt,
                    par_dep_pt,
                    var_imp_pt,
                    variables)  
  
}
  
calculate_par_dep <- function(parms,
                              RF_obj_path, 
                              tr_dts_path,
                              par_dep_path,
                              var_imp_path,
                              variables) {
  
  # browser()
  
  RF_obj_name <- parms$RF_obj_name                              
  tr_dts_name <- parms$tr_dts_name
  par_dep_name <- parms$par_dep_name
  var_imp_name <- parms$var_imp_name
  
  parallel_2 <- parms$parallel_2
  
  RF_obj_f_path <- file.path(RF_obj_path, RF_obj_name)
  tr_dts_f_path <- file.path(tr_dts_path, tr_dts_name)
  
  RF_obj <- readRDS(RF_obj_f_path)
  
  var_importances <- RF_obj$variable.importance
  
  dat <- readRDS(tr_dts_f_path)
  
  helper <- function(i, ...){
    partial(pred.var = i, ...) 
  }
  
  pdps <- lapply(variables, helper, object = RF_obj, train = dat, parallel = parallel_2)
  
  write_out_rds(pdps, par_dep_path, par_dep_name)
  write_out_rds(var_importances, var_imp_path, var_imp_name)
  
}

extract_pd <- function(i, variables, all_tables){  
  
  #browser()
  var <- variables[i]
  
  all_pd_tables <- lapply(all_tables, "[[" , i)
  n <- nrow(all_tables[[1]][[1]])
  all_x <- vapply(all_pd_tables, "[[", numeric(n), var)  
  all_y <- vapply(all_pd_tables, "[[", numeric(n), "yhat")
  
  data.frame(x = apply(all_x, 1, median),
             q50 = apply(all_y, 1, median),
             q05 = apply(all_y, 1, quantile, 0.025),
             q95 = apply(all_y, 1, quantile, 0.975),
             var = var)
  
}

extract_vi <- function(i, variables, all_tables){  
  
  #browser()
  
  var <- variables[i]
  
  out <- rep(0, length(all_tables))
    
  for (j in seq_along(all_tables)){
    
    tar <- all_tables[[j]]
    #id <- which(tar[,"variable"] == var)
    #out[j] <- tar[id, "percentage"]
    out[j] <- tar[var]
    
  }

  out

}

edit_pd_list <- function(x){
  #browser()
  var <- names(x)[1]  
  x$var <- var
  names(x)[names(x) == var] <- "x"
  x
}

normalize_impurity <- function(x){
  
  (x / sum(x)) * 100

}
