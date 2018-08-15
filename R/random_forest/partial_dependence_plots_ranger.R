wrapper_over_bsamples <- function(i, RF_obj_pt, tr_dts_pt, par_dep_pt, var_imp_pt){
  
  RF_obj_nm <- paste0("RF_obj_sample", "_", i, ".rds")
  tr_dts_nm <- paste0("train_dts_", i, ".rds")
  par_dep_nm <- paste0("par_dep_", i, ".rds")
  var_imp_nm <- paste0("var_imp_", i, ".rds")
  
  calculate_par_dep(RF_obj_nm,
                    tr_dts_nm,
                    par_dep_nm,
                    var_imp_nm,
                    RF_obj_pt,
                    tr_dts_pt,
                    par_dep_pt,
                    var_imp_pt,
                    model_type, 
                    variables)  
  
}
  
calculate_par_dep <- function(RF_obj_name,
                              tr_dts_name,
                              par_dep_name,
                              var_imp_name,
                              RF_obj_path, 
                              tr_dts_path,
                              par_dep_path,
                              var_imp_path,
                              model_type, 
                              variables) {
  
  # browser()
  
  RF_obj_f_path <- file.path(RF_obj_path, RF_obj_name)
  tr_dts_f_path <- file.path(tr_dts_path, tr_dts_name)
  
  RF_obj <- readRDS(RF_obj_f_path)
  
  var_importances <- RF_obj$variable.importance
  
  dat <- readRDS(tr_dts_f_path)
  
  helper <- function(i, ...){
    partial(pred.var = i, ...) 
  }
  
  pdps <- lapply(variables, helper, object = RF_obj, train = dat, parallel = TRUE)
    
  write_out_rds(pdps, par_dep_path, par_dep_name)
  write_out_rds(var_importances, var_imp_path, var_imp_name)
  
}

extract_pd <- function(i, variables, all_tables){  
  
  var <- variables[i]
  
  all_pd_tables <- lapply(all_tables, "[[" , i)
  all_x <- vapply(all_pd_tables, "[[", numeric(20), var)  
  all_y <- vapply(all_pd_tables, "[[", numeric(20), "mean_response")
  
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
    id <- which(tar[,"variable"] == var)
    out[j] <- tar[id, "percentage"]
    
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
