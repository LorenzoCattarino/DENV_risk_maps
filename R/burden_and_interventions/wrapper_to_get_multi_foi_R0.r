wrapper_to_get_multi_foi_R0 <- function(
  i, 
  foi_data, orig_data, age_data,
  age_band_tags,
  age_band_lower_bounds, 
  age_band_upper_bounds, 
  vec_phis){
  
  
  browser()
  
  # ---------------------------------------- extract info for ONE foi value
  
  
  FOI <- foi_data[i, "FOI"]
  #cat("FOI value =", FOI, "\n")
  
  ID_0 <- orig_data[i, "ID_0"]
  #cat("country code =", ID_0, "\n")
  
  N <- orig_data[i, "population"]
  #cat("population =", total_pop, "\n")
  
  age_struct <- age_data[i, age_band_tags]
  
  
  # ----------------------------------------
  
  
  no_rep <- length(i)
  
  vapply(seq_len(no_rep),
         wrapper_to_get_R0,
         numeric(1),
         FOI = FOI, 
         N = N, 
         age_struct = age_struct, 
         age_band_lower_bounds = age_band_lower_bounds, 
         age_band_upper_bounds = age_band_upper_bounds, 
         age_band_tags = age_band_tags,
         vec_phis = vec_phis)
  
}
