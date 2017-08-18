wrapper_to_get_multi_foi_R0 <- function(
  i, 
  foi_data, orig_data, age_data,
  age_band_tags,
  age_band_lower_bounds, 
  age_band_upper_bounds, 
  vec_phis, scaling_factor,
  w_1, w_2, w_3){
  
  
  #browser()
  
  # ---------------------------------------- extract info for ONE foi value
  
  # vector of foi values 
  FOI_values <- foi_data[i, ]
  
  N <- orig_data[i, "population"]
  #cat("population =", N, "\n")
  
  age_struct <- age_data[i, age_band_tags]
  
  
  # ---------------------------------------- calculates R0 values for different replicates of the same pixel  
  
  
  vapply(FOI_values,
         wrapper_to_get_R0,
         numeric(5),
         N = N, 
         age_struct = age_struct, 
         age_band_lower_bounds = age_band_lower_bounds, 
         age_band_upper_bounds = age_band_upper_bounds, 
         age_band_tags = age_band_tags,
         vec_phis = vec_phis,
         scaling_factor = scaling_factor,
         w_1 = w_1, 
         w_2 = w_2, 
         w_3 = w_3)
  
}
