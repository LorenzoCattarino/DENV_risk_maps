calculate_par_dep <- function(i, 
                              RF_mod_name, 
                              model_in_path, 
                              model_type, 
                              variables, 
                              out_path_1,
                              out_path_2) {
  
  h2o.init()
  
  RF_obj_nm <- paste0(RF_mod_name, "_", i, ".rds")
  
  RF_obj <- h2o.loadModel(file.path(model_in_path, RF_obj_nm))
  
  var_importances <- RF_obj@model$variable_importances
  
  dat <- readRDS(file.path("output", 
                           "EM_algorithm",
                           model_type,
                           "training_datasets",
                           paste0("train_dts_", i, ".rds")))
  
  dat_h2o <- as.h2o(dat)
  
  pdps <- h2o.partialPlot(RF_obj, dat_h2o, variables, plot = FALSE)
  
  out_name_1 <- paste0("par_dep_", i, ".rds")
  out_name_2 <- paste0("var_imp_", i, ".rds")
    
  write_out_rds(pdps, out_path_1, out_name_1)
  write_out_rds(var_importances, out_path_2, out_name_2)
  
  h2o.shutdown(prompt = FALSE)
  
}

extract_pd <- function(i, variables, all_tables){  
  
  var <- variables[i]
  
  all_pd_tables <- lapply(all_tables, "[[" , i)
  all_x <- vapply(all_pd_tables, "[[", numeric(20), var)  
  all_y <- vapply(all_pd_tables, "[[", numeric(20), "mean_response")
  
  data.frame(x = apply(all_x, 1, median),
             q50 = apply(all_y, 1, median),
             q05 = apply(all_y, 1, quantile, 0.05),
             q95 = apply(all_y, 1, quantile, 0.95),
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
