wrapper_to_get_R0 <- function(
  x, age_data, 
  age_band_lower_bounds, age_band_upper_bounds, age_band_tags,
  vec_phis){
  
  
  # ---------------------------------------- extract info for ONE foi value
  
  
  FOI <- x["FOI"]
  #cat("FOI value =", FOI, "\n")
  
  ID_0 <- x["ID_0"]
  #cat("country code =", ID_0, "\n")
  
  total_pop <- x["population"]
  #cat("population =", total_pop, "\n")
  
  calculate_probs_and_R0(
    age_data = age_data, 
    ID_0 = ID_0, 
    age_band_tags = age_band_tags, 
    FOI = FOI, 
    age_band_lower_bounds = age_band_lower_bounds, 
    age_band_upper_bounds = age_band_upper_bounds,
    total_pop = total_pop, 
    vec_phis = vec_phis)  
  
}
