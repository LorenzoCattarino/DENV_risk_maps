create_lookup_tables <- function(i,
                                 target_fun,
                                 additional_dts,
                                 out_path,
                                 age_struct,
                                 age_band_tags,
                                 age_band_L_bounds,
                                 age_band_U_bounds,
                                 parms){
  
  cat("variable to look up =", i, "\n")
  
  FOI_values <- parms$FOI_grid
  parallel_2 <- parms$parallel_2
  
  if (i == "I") {
    
    my_fun <- target_fun$calculate_infections
    out_nm <- sprintf("FOI_to_%s_lookup_tables.rds", i)
    
    if (!file.exists(file.path(out_path, out_nm))) {  
    
      message("1D lookup")
      
      Infection_values <- loop(seq_len(nrow(age_struct)), 
                               wrapper_to_lookup,
                               age_struct = age_struct, 
                               tags = age_band_tags, 
                               FOI_values = FOI_values, 
                               my_fun = my_fun,
                               age_band_lower_bounds = age_band_L_bounds, 
                               age_band_upper_bounds = age_band_U_bounds,
                               parallel = parallel_2)
      
      lookup_list <- lapply(Infection_values, cbind_FOI_to_lookup, FOI_values)
      
      saveRDS(lookup_list, file.path(out_path, out_nm))
      
    }
  
  }
  
  if (i == "C") {
    
    my_fun <- target_fun$calculate_cases
    param_post <- additional_dts$prop_sym
    my_weights <- parms$prop_sympt
    out_nm <- sprintf("FOI_to_%s_lookup_tables.rds", i)
    out_nm_2 <- sprintf("FOI_to_%s_lookup_tables_fixed_params.rds", i)
    
    if (!file.exists(file.path(out_path, out_nm))) {
    
      message("2D lookup")
      
      lookup_list <- loop(seq_len(nrow(age_struct)), 
                          wrapper_to_lookup_2D,
                          age_struct = age_struct, 
                          param_post = param_post,
                          tags = age_band_tags, 
                          FOI_values = FOI_values, 
                          my_fun = my_fun,
                          age_band_lower_bounds = age_band_L_bounds,
                          age_band_upper_bounds = age_band_U_bounds,
                          parallel = parallel_2)
      
      saveRDS(lookup_list, file.path(out_path, out_nm))
      
    }
    
    if (!file.exists(file.path(out_path, out_nm_2))) {
      
      message("1D lookup")
      
      cat("weights vector =", my_weights, "\n")
      
      case_values <- loop(seq_len(nrow(age_struct)), 
                                         wrapper_to_lookup,
                                         age_struct = age_struct, 
                                         tags = age_band_tags, 
                                         FOI_values = FOI_values, 
                                         my_fun = my_fun,
                                         age_band_lower_bounds = age_band_L_bounds,
                                         age_band_upper_bounds = age_band_U_bounds,
                                         weights_vec = my_weights,
                                         parallel = parallel_2)
      
      lookup_list <- lapply(case_values, cbind_FOI_to_lookup, FOI_values)
      
      saveRDS(lookup_list, file.path(out_path, out_nm_2))
      
    }
  
  }
  
  if (i == "HC") {
    
    my_fun <- target_fun$calculate_hosp_cases
    param_post <- additional_dts$prop_sym
    my_weights <- parms$prop_sympt
    out_nm <- sprintf("FOI_to_%s_lookup_tables.rds", i)
    out_nm_2 <- sprintf("FOI_to_%s_lookup_tables_fixed_params.rds", i)
    
    if (!file.exists(file.path(out_path, out_nm))) {    
    
      message("2D lookup")
      
      lookup_list <- loop(seq_len(nrow(age_struct)), 
                          wrapper_to_lookup_2D,
                          age_struct = age_struct, 
                          param_post = param_post,
                          tags = age_band_tags, 
                          FOI_values = FOI_values, 
                          my_fun = my_fun,
                          age_band_lower_bounds = age_band_L_bounds,
                          age_band_upper_bounds = age_band_U_bounds,
                          parms = parms,
                          parallel = parallel_2)
      
      saveRDS(lookup_list, file.path(out_path, out_nm))
      
    }
    
    if (!file.exists(file.path(out_path, out_nm_2))) {
      
      message("1D lookup")
      
      cat("weights vector =", my_weights, "\n")
      
      HCase_values <- loop(seq_len(nrow(age_struct)), 
                                         wrapper_to_lookup,
                                         age_struct = age_struct, 
                                         tags = age_band_tags, 
                                         FOI_values = FOI_values, 
                                         my_fun = my_fun,
                                         age_band_lower_bounds = age_band_L_bounds,
                                         age_band_upper_bounds = age_band_U_bounds,
                                         parms = parms,
                                         weights_vec = my_weights,
                                         parallel = parallel_2)
      
      lookup_list <- lapply(HCase_values, cbind_FOI_to_lookup, FOI_values)
      
      saveRDS(lookup_list, file.path(out_path, out_nm_2))
      
    }
    
  }
  
  if (i == "R0_1") {
    
    my_fun <- target_fun$calculate_R0
    my_weights <- parms$vec_phis_R0_1
    out_nm <- sprintf("FOI_to_%s_lookup_tables.rds", i)
    
    if (!file.exists(file.path(out_path, out_nm))) {
    
      message("1D lookup")
      
      cat("weights vector =", my_weights, "\n")
      
      R0_values <- loop(seq_len(nrow(age_struct)), 
                        wrapper_to_lookup,
                        age_struct = age_struct, 
                        tags = age_band_tags, 
                        FOI_values = FOI_values, 
                        my_fun = my_fun,
                        age_band_lower_bounds = age_band_L_bounds, 
                        age_band_upper_bounds = age_band_U_bounds,
                        weights_vec = my_weights,
                        parallel = parallel_2)
      
      lookup_list <- lapply(R0_values, cbind_FOI_to_lookup, FOI_values)
      
      lookup_list <- lapply(lookup_list, fix_R0_lookup_limits)
      
      saveRDS(lookup_list, file.path(out_path, out_nm))
      
    }
    
  }
  
  if (i == "R0_2") {
    
    my_fun <- target_fun$calculate_R0
    my_weights <- parms$vec_phis_R0_2
    out_nm <- sprintf("FOI_to_%s_lookup_tables.rds", i)
    
    if (!file.exists(file.path(out_path, out_nm))) {
      
      message("1D lookup")
      
      cat("weights vector =", my_weights, "\n")
      
      R0_values <- loop(seq_len(nrow(age_struct)), 
                        wrapper_to_lookup,
                        age_struct = age_struct, 
                        tags = age_band_tags, 
                        FOI_values = FOI_values, 
                        my_fun = my_fun,
                        age_band_lower_bounds = age_band_L_bounds, 
                        age_band_upper_bounds = age_band_U_bounds,
                        weights_vec = my_weights,
                        parallel = parallel_2)
      
      lookup_list <- lapply(R0_values, cbind_FOI_to_lookup, FOI_values)
      
      lookup_list <- lapply(lookup_list, fix_R0_lookup_limits)
      
      saveRDS(lookup_list, file.path(out_path, out_nm))
      
    }
    
  }

  if (i == "R0_3") {
    
    my_fun <- target_fun$calculate_R0
    param_post <- additional_dts$inf_weights
    my_weights <- parms$vec_phis_R0_3
    out_nm <- sprintf("FOI_to_%s_lookup_tables.rds", i)
    
    if (!file.exists(file.path(out_path, out_nm))) {
      
      message("1D lookup")
      
      cat("weights vector =", my_weights, "\n")
      
      R0_values <- loop(seq_len(nrow(age_struct)), 
                        wrapper_to_lookup,
                        age_struct = age_struct, 
                        tags = age_band_tags, 
                        FOI_values = FOI_values, 
                        my_fun = my_fun,
                        age_band_lower_bounds = age_band_L_bounds,
                        age_band_upper_bounds = age_band_U_bounds,
                        weights_vec = my_weights,
                        parallel = parallel_2)
      
      lookup_list <- lapply(R0_values, cbind_FOI_to_lookup, FOI_values)
      
      lookup_list <- lapply(lookup_list, fix_R0_lookup_limits)
      
      saveRDS(lookup_list, file.path(out_path, out_nm))
      
    }
    
  }

}
