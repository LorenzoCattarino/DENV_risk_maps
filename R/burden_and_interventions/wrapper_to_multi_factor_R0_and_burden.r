wrapper_to_multi_factor_R0_and_burden <- function(x, 
                                                  foi_data, 
                                                  age_data,
                                                  age_band_tags, 
                                                  age_band_lower_bounds, 
                                                  age_band_upper_bounds,
                                                  parallel_2, 
                                                  var_names, 
                                                  FOI_values, 
                                                  FOI_to_Inf_list, 
                                                  FOI_to_C_list, 
                                                  prob_fun, 
                                                  var_to_fit, 
                                                  fit_type, 
                                                  base_info, 
                                                  lookup_path,
                                                  out_path, 
                                                  no_fits = NULL) {
  
  
  #browser()
  
  
  # ---------------------------------------- Extract factor values
  
  
  run_ID <- x$id
  cat("ID run =", run_ID, "\n")

  phi_1 <- x$phi1
  phi_2 <- x$phi2
  phi_3 <- x$phi3
  phi_4 <- x$phi4
  
  sf <- x$scaling_factor
    
  phi_set_id <- x$phi_set_id
  
  
  # ---------------------------------------- define variables
 

  vec_phis <- c(phi_1, phi_2, phi_3, phi_4)
  
  model_tp <- paste0(var_to_fit, "_", fit_type, "_model")
  
  
  # ---------------------------------------- create FOI -> R0 look up tables

  
  if(!file.exists(file.path(lookup_path, paste0("FOI_to_R0_lookup_tables_", phi_set_id ,".rds")))){
    
    R0_values <- loop(seq_len(nrow(age_data)), 
                      wrapper_to_lookup,
                      age_struct = age_data, 
                      tags = age_band_tags, 
                      FOI_values = FOI_values, 
                      my_fun = calculate_R0,
                      N = 1,
                      prob_fun = prob_fun,
                      age_band_lower_bounds = age_band_lower_bounds, 
                      age_band_upper_bounds = age_band_upper_bounds,
                      vec_phis = vec_phis,
                      parallel = TRUE)
    
    FOI_to_R0_list <- lapply(R0_values, cbind_FOI_to_lookup, FOI_values)
    
    FOI_to_R0_list <- lapply(FOI_to_R0_list, fix_R0_lookup_limits)
  
    saveRDS(FOI_to_R0_list, file.path(lookup_path, paste0("FOI_to_R0_lookup_tables_", phi_set_id ,".rds")))  
    
  } else {
    
    FOI_to_R0_list <- readRDS(file.path(lookup_path, paste0("FOI_to_R0_lookup_tables_", phi_set_id ,".rds")))
    
  } 
    
  
  # ---------------------------------------- calculates R0 values for different pixels  
  

  burden_estimates <- loop(
    seq_len(nrow(foi_data)),
    wrapper_to_replicate_R0_and_burden, 
    foi_data = foi_data, 
    age_struct = age_data,
    scaling_factor = sf,
    FOI_to_R0_list = FOI_to_R0_list,
    FOI_to_Inf_list = FOI_to_Inf_list,
    FOI_to_C_list = FOI_to_C_list,
    age_band_lower_bounds = age_band_lower_bounds,
    age_band_upper_bounds = age_band_upper_bounds,
    age_band_tags = age_band_tags,
    vec_phis = vec_phis, 
    prob_fun = prob_fun,
    no_fits = no_fits,
    var_to_fit = var_to_fit, 
    fit_type = fit_type,
    parallel = parallel_2)
  

  # ---------------------------------------- reshape and save


  if(fit_type == "boot"){
    
    for (b in seq_along(var_names)){
      
      ret1 <- lapply(burden_estimates, "[", var_names[b], TRUE)
      
      ret2 <- do.call("rbind", ret1)
      
      ret3 <- cbind(foi_data[, base_info], ret2)
      
      fl_nm <- paste0(var_names[b], "_all_squares_", run_ID, ".rds")
      
      write_out_rds(ret3, out_path, fl_nm)
      
    }
    
  } else {
    
    for (b in seq_along(var_names)){
      
      ret2 <- do.call("rbind", burden_estimates)
      
      ret3 <- cbind(foi_data[, base_info], best = ret2[,var_names[b]])
      
      fl_nm <- paste0(var_names[b], "_best_all_squares_", run_ID, ".rds")
      
      write_out_rds(as.data.frame(ret3), out_path, fl_nm)
      
    }
    
  }
  
}
