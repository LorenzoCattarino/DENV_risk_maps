wrapper_to_multi_factor_R0_and_burden <- function(x, 
                                                  foi_data, 
                                                  age_data,
                                                  age_band_tags, 
                                                  age_band_lower_bounds, 
                                                  age_band_upper_bounds,
                                                  FOI_to_Inf_list, 
                                                  FOI_to_C_list, 
                                                  FOI_to_C_list_fixed, 
                                                  FOI_to_HC_list,
                                                  FOI_to_HC_list_fixed,
                                                  FOI_to_R0_1_list,
                                                  FOI_to_R0_2_list,
                                                  FOI_to_R0_3_list,
                                                  prob_fun, 
                                                  parms, 
                                                  out_path,
                                                  var_to_fit){

  
  fit_type <- parms$fit_type
  parallel_2 <- parms$parallel_2
  base_info <- parms$base_info

  run_ID <- x$id
  sf <- x$scaling_factor
  
  cat("ID run =", run_ID, "\n")  
  
  if(fit_type == "boot"){
    
    if (var_to_fit == "FOI" | var_to_fit == "Z") {
      
      var_names <- c("response_r", "transformed_1_r", "transformed_2_r", "transformed_3_r", "I_num", "C_num", "H_num")
      
      burden_estimates <- loop(
        seq_len(nrow(foi_data)),
        wrapper_to_replicate_R0_and_burden, 
        foi_data = foi_data, 
        age_struct = age_data,
        scaling_factor = sf,
        FOI_to_R0_1_list = FOI_to_R0_1_list,
        FOI_to_R0_2_list = FOI_to_R0_2_list,
        FOI_to_R0_3_list = FOI_to_R0_3_list,
        FOI_to_Inf_list = FOI_to_Inf_list,
        FOI_to_C_list = FOI_to_C_list,
        FOI_to_HC_list = FOI_to_HC_list,
        age_band_lower_bounds = age_band_lower_bounds,
        age_band_upper_bounds = age_band_upper_bounds,
        age_band_tags = age_band_tags,
        vars = var_names,
        parms = parms,
        fixed_prop_sym = FALSE,
        var_to_fit = var_to_fit,
        parallel = parallel_2)
      
    } else {
      
      var_names <- c("response_r", "transformed_r", "I_num", "C_num", "H_num")
      
      burden_estimates <- loop(
        seq_len(nrow(foi_data)),
        wrapper_to_replicate_R0_and_burden, 
        foi_data = foi_data, 
        age_struct = age_data,
        scaling_factor = sf,
        FOI_to_R0_1_list = FOI_to_R0_1_list,
        FOI_to_R0_2_list = FOI_to_R0_2_list,
        FOI_to_R0_3_list = FOI_to_R0_3_list,
        FOI_to_Inf_list = FOI_to_Inf_list,
        FOI_to_C_list = FOI_to_C_list,
        FOI_to_HC_list = FOI_to_HC_list,
        age_band_lower_bounds = age_band_lower_bounds,
        age_band_upper_bounds = age_band_upper_bounds,
        age_band_tags = age_band_tags,
        vars = var_names,
        parms = parms,
        fixed_prop_sym = FALSE,
        var_to_fit = var_to_fit,
        parallel = parallel_2)
      
      if (sf == 1) {
        
        burden_estimates_2 <- loop(
          seq_len(nrow(foi_data)),
          wrapper_to_replicate_R0_and_burden, 
          foi_data = foi_data, 
          age_struct = age_data,
          scaling_factor = sf,
          FOI_to_R0_1_list = FOI_to_R0_1_list,
          FOI_to_R0_2_list = FOI_to_R0_2_list,
          FOI_to_R0_3_list = FOI_to_R0_3_list,
          FOI_to_Inf_list = FOI_to_Inf_list,
          FOI_to_C_list = FOI_to_C_list_fixed,
          FOI_to_HC_list = FOI_to_HC_list_fixed,
          age_band_lower_bounds = age_band_lower_bounds,
          age_band_upper_bounds = age_band_upper_bounds,
          age_band_tags = age_band_tags,
          vars = var_names,
          parms = parms,
          fixed_prop_sym = TRUE,
          var_to_fit = var_to_fit,
          parallel = parallel_2)
        
      }
      
    }
    
  }
  
  if(fit_type == "best"){
    
    if (var_to_fit == "FOI" | var_to_fit == "Z") {
      
      var_names <- c("response_r", "transformed_1_r", "transformed_2_r", "transformed_3_r", "I_num", "C_num", "H_num")
      
    } else {
      
      var_names <- c("response_r", "transformed_r", "I_num", "C_num", "H_num")
      
    }
    
    burden_estimates <- loop(
      seq_len(nrow(foi_data)),
      wrapper_to_replicate_R0_and_burden, 
      foi_data = foi_data, 
      age_struct = age_data,
      scaling_factor = sf,
      FOI_to_R0_1_list = FOI_to_R0_1_list,
      FOI_to_R0_2_list = FOI_to_R0_2_list,
      FOI_to_R0_3_list = FOI_to_R0_3_list,
      FOI_to_Inf_list = FOI_to_Inf_list,
      FOI_to_C_list = FOI_to_C_list_fixed,
      FOI_to_HC_list = FOI_to_HC_list_fixed,
      age_band_lower_bounds = age_band_lower_bounds,
      age_band_upper_bounds = age_band_upper_bounds,
      age_band_tags = age_band_tags,
      vars = var_names,
      parms = parms,
      fixed_prop_sym = TRUE,
      var_to_fit = var_to_fit,
      parallel = parallel_2)
    
  }
  
  
  # ---------------------------------------- reshape and save
  
  
  if (fit_type == "boot") {
    
    for (b in seq_along(var_names)) {
      
      ret1 <- lapply(burden_estimates, "[", var_names[b], TRUE)
      
      ret2 <- do.call("rbind", ret1)
      
      ret3 <- cbind(foi_data[, base_info], ret2)
      
      fl_nm <- paste0(var_names[b], "_wolbachia_", run_ID, ".rds")
      
      write_out_rds(ret3, out_path, fl_nm)
      
    }
    
  } else {
    
    for (b in seq_along(var_names)) {
      
      ret1 <- lapply(burden_estimates, "[", var_names[b], TRUE)
      
      ret2 <- do.call("rbind", ret1)
      
      ret3 <- cbind(as.data.frame(foi_data[, base_info]), best = ret2)
      
      fl_nm <- paste0(var_names[b], "_wolbachia_", run_ID, ".rds")
      
      write_out_rds(ret3, out_path, fl_nm)
      
    }
    
  }
  
  if (var_to_fit %in% c("R0_1", "R0_2", "R0_3") & sf == 1) {
    
    if (fit_type == "boot") {
      
      for (b in seq_along(var_names)) {
        
        ret1 <- lapply(burden_estimates_2, "[", var_names[b], TRUE)
        
        ret2 <- do.call("rbind", ret1)
        
        ret3 <- cbind(foi_data[, base_info], ret2)
        
        fl_nm <- paste0(var_names[b], "_wolbachia_", run_ID, "_fixed.rds")
        
        write_out_rds(ret3, out_path, fl_nm)
        
      }
      
    } else {
      
      for (b in seq_along(var_names)) {
        
        ret1 <- lapply(burden_estimates_2, "[", var_names[b], TRUE)
        
        ret2 <- do.call("rbind", ret1)
        
        ret3 <- cbind(as.data.frame(foi_data[, base_info]), best = ret2)
        
        fl_nm <- paste0(var_names[b], "_wolbachia_fixed_", run_ID, "_fixed.rds")
        
        write_out_rds(ret3, out_path, fl_nm)
        
      }
      
    }
  
  }
  
}
