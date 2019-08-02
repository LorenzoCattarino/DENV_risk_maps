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
                                                  parms, 
                                                  out_path){

  
  fit_type <- parms$fit_type
  parallel_2 <- parms$parallel_2
  base_info <- parms$base_info

  run_ID <- x$id
  sf <- x$scaling_factor
  
  cat("ID run =", run_ID, "\n")  
  
  if (fit_type == "best") {
    
    burden_estimates <- loop(
    seq_len(nrow(foi_data)),
    wrapper_to_replicate_R0_and_burden, 
    foi_data = foi_data, 
    age_struct = age_data,
    scaling_factor = sf,
    FOI_to_Inf_list = FOI_to_Inf_list,
    FOI_to_C_list = FOI_to_C_list,
    FOI_to_C_list_fixed = FOI_to_C_list_fixed,
    FOI_to_HC_list = FOI_to_HC_list,
    FOI_to_HC_list_fixed = FOI_to_HC_list_fixed,
    FOI_to_R0_1_list = FOI_to_R0_1_list,
    FOI_to_R0_2_list = FOI_to_R0_2_list,
    FOI_to_R0_3_list = FOI_to_R0_3_list,
    age_band_lower_bounds = age_band_lower_bounds,
    age_band_upper_bounds = age_band_upper_bounds,
    age_band_tags = age_band_tags,
    parms = parms,
    fixed_prop_sym = TRUE,
    parallel = parallel_2)
  
    var_names <- rownames(burden_estimates[[1]])
    
    for (b in seq_along(var_names)) {
      
      ret1 <- lapply(burden_estimates, "[", var_names[b], TRUE)
      
      ret2 <- do.call("rbind", ret1)
      
      ret3 <- cbind(as.data.frame(foi_data[, base_info]), best = ret2)
      
      fl_nm <- paste0(var_names[b], "_wolbachia_", run_ID, ".rds")
      
      write_out_rds(ret3, out_path, fl_nm)
      
    }
    
  } else {
    
    burden_estimates <- loop(
      seq_len(nrow(foi_data)),
      wrapper_to_replicate_R0_and_burden, 
      foi_data = foi_data, 
      age_struct = age_data,
      scaling_factor = sf,
      FOI_to_Inf_list = FOI_to_Inf_list,
      FOI_to_C_list = FOI_to_C_list,
      FOI_to_C_list_fixed = FOI_to_C_list_fixed,
      FOI_to_HC_list = FOI_to_HC_list,
      FOI_to_HC_list_fixed = FOI_to_HC_list_fixed,
      FOI_to_R0_1_list = FOI_to_R0_1_list,
      FOI_to_R0_2_list = FOI_to_R0_2_list,
      FOI_to_R0_3_list = FOI_to_R0_3_list,
      age_band_lower_bounds = age_band_lower_bounds,
      age_band_upper_bounds = age_band_upper_bounds,
      age_band_tags = age_band_tags,
      parms = parms,
      fixed_prop_sym = FALSE,
      parallel = parallel_2)
    
    var_names <- rownames(burden_estimates[[1]])
    
    for (b in seq_along(var_names)) {
      
      ret1 <- lapply(burden_estimates, "[", var_names[b], TRUE)
      
      ret2 <- do.call("rbind", ret1)
      
      ret3 <- cbind(foi_data[, base_info], ret2)
      
      fl_nm <- paste0(var_names[b], "_wolbachia_", run_ID, ".rds")
      
      write_out_rds(ret3, out_path, fl_nm)
      
    }  
    
  }

  if (fit_type == "boot" & sf == 1) {
    
     burden_estimates_2 <- loop(
      seq_len(nrow(foi_data)),
      wrapper_to_replicate_R0_and_burden, 
      foi_data = foi_data, 
      age_struct = age_data,
      scaling_factor = sf,
      FOI_to_Inf_list = FOI_to_Inf_list,
      FOI_to_C_list = FOI_to_C_list,
      FOI_to_C_list_fixed = FOI_to_C_list_fixed,
      FOI_to_HC_list = FOI_to_HC_list,
      FOI_to_HC_list_fixed = FOI_to_HC_list_fixed,
      FOI_to_R0_1_list = FOI_to_R0_1_list,
      FOI_to_R0_2_list = FOI_to_R0_2_list,
      FOI_to_R0_3_list = FOI_to_R0_3_list,
      age_band_lower_bounds = age_band_lower_bounds,
      age_band_upper_bounds = age_band_upper_bounds,
      age_band_tags = age_band_tags,
      parms = parms,
      fixed_prop_sym = TRUE,
      parallel = parallel_2)
    
    var_names <- rownames(burden_estimates_2[[1]])
    
    for (b in seq_along(var_names)) {
      
      ret1 <- lapply(burden_estimates_2, "[", var_names[b], TRUE)
      
      ret2 <- do.call("rbind", ret1)
      
      ret3 <- cbind(foi_data[, base_info], ret2)
      
      fl_nm <- paste0(var_names[b], "_wolbachia_", run_ID, "_fixed.rds")
      
      write_out_rds(ret3, out_path, fl_nm)
      
    }  
    
  }
  
}
