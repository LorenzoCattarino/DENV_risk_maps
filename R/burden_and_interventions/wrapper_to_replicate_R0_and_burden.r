wrapper_to_replicate_R0_and_burden <- function(
  i, 
  foi_data, age_struct, 
  scaling_factor, var_names, 
  FOI_to_R0_list,
  FOI_to_Inf_list,
  FOI_to_C_list,
  age_band_lower_bounds, age_band_upper_bounds, age_band_tags,
  vec_phis, prob_fun, no_fits, reverse){
  
  
  #browser()
  
  
  # ---------------------------------------- get the right look up table for each square 
  
  
  idx <- foi_data[i, "age_id"]
    
  FOI_to_R0 <- FOI_to_R0_list[[idx]]
  FOI_to_Inf <- FOI_to_Inf_list[[idx]]
  FOI_to_C <- FOI_to_C_list[[idx]]
  
  
  # ---------------------------------------- get vector of foi values and number of people in each square
  
  
  col_ids <- as.character(seq_len(no_fits))
  
  FOI_values <- foi_data[i, col_ids]
  #FOI_values <- foi_data[i, "mean_pred"]
  
  sqr_pop <- foi_data[i, "population"]
  #cat("square population =", sqr_pop, "\n")
  
  m_j <- age_struct[age_struct$ADM_0 == foi_data[i, "ADM_0"], age_band_tags]
  
    
  # ---------------------------------------- calculates R0 values for different replicates of the same pixel  
  
  
  vars <- c("FOI_r", "R0_r", "I_num", "C_num", "I_inc", "C_inc")
  
  n <- length(vars)
  
  vapply(FOI_values,
         wrapper_to_R0_and_burden,
         numeric(n),
         n_j = m_j,
         vars = vars,
         age_band_lower_bounds = age_band_lower_bounds, 
         age_band_upper_bounds = age_band_upper_bounds,
         vec_phis = vec_phis, 
         prob_fun = prob_fun,
         scaling_factor = scaling_factor,
         FOI_to_R0 = FOI_to_R0, 
         FOI_to_Inf = FOI_to_Inf, 
         FOI_to_C = FOI_to_C,
         N = sqr_pop,
         reverse = reverse)
  
  # wrapper_to_R0_and_burden(FOI_values,
  #                          n_j = m_j,
  #                          age_band_lower_bounds = age_band_lower_bounds, 
  #                          age_band_upper_bounds = age_band_upper_bounds,
  #                          vec_phis = vec_phis, 
  #                          prob_fun = prob_fun,
  #                          scaling_factor = scaling_factor,
  #                          var_names = var_names,
  #                          FOI_to_R0 = FOI_to_R0, 
  #                          FOI_to_Inf = FOI_to_Inf, 
  #                          FOI_to_C = FOI_to_C,
  #                          N = sqr_pop,
  #                          reverse = reverse)  
}
