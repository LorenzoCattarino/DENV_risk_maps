wrapper_to_replicate_R0_and_burden <- function(i,
                                               foi_data, 
                                               age_struct,
                                               scaling_factor,
                                               FOI_to_R0_1_list = FOI_to_R0_1_list,
                                               FOI_to_R0_2_list = FOI_to_R0_2_list,
                                               FOI_to_R0_3_list = FOI_to_R0_3_list,
                                               FOI_to_Inf_list,
                                               FOI_to_C_list,
                                               FOI_to_HC_list,
                                               age_band_lower_bounds, 
                                               age_band_upper_bounds, 
                                               age_band_tags,
                                               vars,
                                               parms,
                                               fixed_prop_sym,
                                               var_to_fit){
  
  
  # browser()
  
  # ---------------------------------------------------------------------------
  # get the right look up table for each square  
  

  # cat("pixel number =", i, "\n")
  
  no_fits <- parms$no_samples 
  fit_type <- parms$fit_type
  FOI_grid <- parms$FOI_grid
  
  FOI_grid_res <- max(FOI_grid) / length(FOI_grid)
  
  idx <- foi_data[i, "age_id"]
    
  
  FOI_to_Inf <- FOI_to_Inf_list[[idx]]
  FOI_to_C <- FOI_to_C_list[[idx]]
  FOI_to_HC <- FOI_to_HC_list[[idx]] 
  
  
  # --------------------------------------------------------------------------- 
  # get vector of foi values and number of people in each square  
  
  
  if(!is.null(no_fits)){
    col_ids <- as.character(seq_len(no_fits))
  }
  
  m_j <- age_struct[age_struct$ADM_0 == foi_data[i, "ID_0"], age_band_tags]
  
  if(fit_type == "boot") {
    
    preds <- foi_data[i, col_ids]
    
  } else {
    
    preds <- foi_data[i, "best"]
  }

  N <- foi_data[i, "population"]
  
  
  # ---------------------------------------------------------------------------
  # calculates R0 values for different replicates of the same pixel    
  
  
  red_preds <- preds * scaling_factor
  
  if(var_to_fit == "FOI" | var_to_fit == "Z"){
    
    red_trans <- red_preds 
    
    ## added on 13052019
    FOI_to_R0 <- FOI_to_R0_1_list[[idx]]
    red_trans_1 <- approx(FOI_to_R0[, "x"], FOI_to_R0[, "y"], xout = red_trans)$y
    
    FOI_to_R0 <- FOI_to_R0_2_list[[idx]]
    red_trans_2 <- approx(FOI_to_R0[, "x"], FOI_to_R0[, "y"], xout = red_trans)$y
  
    FOI_to_R0 <- FOI_to_R0_3_list[[idx]]
    red_trans_3 <- approx(FOI_to_R0[, "x"], FOI_to_R0[, "y"], xout = red_trans)$y
  
  } else {
    
    FOI_to_R0_list <- get(sprintf("FOI_to_%s_list", var_to_fit))
    FOI_to_R0 <- FOI_to_R0_list[[idx]]
    red_trans <- approx(FOI_to_R0[, "y"], FOI_to_R0[, "x"], xout = red_preds)$y
    
  }

  Infections_pc <- approx(FOI_to_Inf[, "x"], FOI_to_Inf[, "y"], xout = red_trans)$y 
  
  if(fixed_prop_sym) {
    
    Cases_pc <- approx(FOI_to_C[, "x"], FOI_to_C[, "y"], xout = red_trans)$y
    Hosp_cases_pc <- approx(FOI_to_HC[, "x"], FOI_to_HC[, "y"], xout = red_trans)$y
    
  } else {
    
    rowIndices <- floor(red_trans / FOI_grid_res) # this gives a 0-based index
    rowIndices <- rowIndices + 1 # correct for 1-based R indexing
    
    rowIndices_next <- rowIndices + 1
    
    colIndices <- seq_len(no_fits)
    
    Cases_pc <- interpolate_using_mat_indices(lookup_mat = FOI_to_C,
                                              rowIndices = rowIndices, 
                                              colIndices = colIndices, 
                                              rowIndices_next = rowIndices_next,
                                              FOI_grid = FOI_grid, 
                                              FOI_values = red_trans)
    
    Hosp_cases_pc <- interpolate_using_mat_indices(lookup_mat = FOI_to_HC,
                                                   rowIndices = rowIndices, 
                                                   colIndices = colIndices, 
                                                   rowIndices_next = rowIndices_next,
                                                   FOI_grid = FOI_grid, 
                                                   FOI_values = red_trans)
    
  }
  
  Infections <- Infections_pc * N
  Cases <- Cases_pc * N
  HCases <- Hosp_cases_pc * N 
  
  if(var_to_fit == "FOI" | var_to_fit == "Z"){
    
    out <- rbind(red_preds, red_trans_1, red_trans_2, red_trans_3, Infections, Cases, HCases)
    rownames(out) <- vars
    
  } else {
    
    out <- rbind(red_preds, red_trans, Infections, Cases, HCases)
    rownames(out) <- vars
    
  }
  
  out
  
}
