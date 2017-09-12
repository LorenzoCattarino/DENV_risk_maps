wrapper_to_replicate_R0_and_burden <- function(
  i, 
  foi_data, look_up, age_band_tags,
  age_band_lower_bounds, age_band_upper_bounds, 
  vec_phis, scaling_factor,
  rho, gamma_1, gamma_3,
  var_names, R0_to_FOI_list){
  
  
  #browser()
  
  
  idx <- foi_data$fun_idx
  
  adm0 <- foi_data$ADM_0
    
  R0_to_FOI <- R0_to_FOI_list[[idx]]
  
  
  # ---------------------------------------- get vector of foi values and number of people in each age group
  
  
  #FOI_values <- foi_data[i, ]
  FOI_values <- foi_data[i, "mean_pred"]
  
  n_j <- look_up[look_up$ADM_0 == adm0, age_band_tags]
  
  
  # ---------------------------------------- calculates R0 values for different replicates of the same pixel  
  
  
  # vapply(FOI_values,
  #        calculate_R0_and_burden,
  #        numeric(3),
  #        N = N, 
  #        age_struct = age_struct, 
  #        age_band_lower_bounds = age_band_lower_bounds, 
  #        age_band_upper_bounds = age_band_upper_bounds, 
  #        age_band_tags = age_band_tags,
  #        vec_phis = vec_phis,
  #        scaling_factor = scaling_factor,
  #        w_1 = w_1, 
  #        w_2 = w_2, 
  #        w_3 = w_3)
  
  wrapper_to_R0_and_burden(FOI_values,
                           n_j = n_j, 
                           age_band_lower_bounds = age_band_lower_bounds, 
                           age_band_upper_bounds = age_band_upper_bounds, 
                           vec_phis = vec_phis,
                           scaling_factor = scaling_factor,
                           rho = rho, 
                           gamma_1 = gamma_1,
                           gamma_3 = gamma_3,
                           var_names = var_names,
                           R0_to_FOI = R0_to_FOI)  
}
