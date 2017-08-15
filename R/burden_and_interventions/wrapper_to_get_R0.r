wrapper_to_get_R0 <- function(
  i, df, age_data, 
  age_band_lower_bounds, age_band_upper_bounds, age_band_tags,
  vec_phis){
  
  #browser()
  
  # ---------------------------------------- extract info for ONE foi value
  
  
  FOI <- df[i, "FOI"]
  #cat("FOI value =", FOI, "\n")
  
  ID_0 <- df[i, "ID_0"]
  #cat("country code =", ID_0, "\n")
  
  total_pop <- df[i, "population"]
  #cat("population =", total_pop, "\n")
  
  ag_st <- age_data[i, age_band_tags]
  
  
  # ----------------------------------------
  
  
  # calculate n people in age group
  n_j <- ag_st * total_pop  
  
  
  # ---------------------------------------- get R0
  
  
  calculate_probs_and_R0(
    FOI = FOI, 
    age_band_lower_bounds = age_band_lower_bounds, 
    age_band_upper_bounds = age_band_upper_bounds,
    n_j = n_j, 
    vec_phis = vec_phis)  
  
}
