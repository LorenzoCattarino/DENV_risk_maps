burden_multi_factor_wrapper <- function(
  x, foi_data, orig_data,
  age_band_tags, age_band_lower_bounds, 
  age_band_upper_bounds, 
  w_1, w_2, w_3,
  look_up, out_path){
  
  
  #browser()
  
  # ---------------------------------------- Extract factor values
  
  
  run_ID <- x$id
  cat("ID run =", run_ID, "\n")

  phi_1 <- x$phi1
  phi_2 <- x$phi2
  phi_3 <- x$phi3
  phi_4 <- x$phi4
  
  cat("phi 1 =", phi_1, "\n")  
  cat("phi 2 =", phi_2, "\n") 
  cat("phi 3 =", phi_3, "\n") 
  cat("phi 4 =", phi_4, "\n") 
  
  sf <- x$scaling_factor
  cat("scaling factor =", sf, "\n")
  
  
  # ---------------------------------------- define parameters 
  
  
  out_tags <- c("R0", 
                "number_of_infections", 
                "number_of_cases", 
                "incidence_of_infections", 
                "incidence_of_cases")
  
  
  # ---------------------------------------- define variables
 

  vec_phis <- c(phi_1, phi_2, phi_3, phi_4)

  
  # ---------------------------------------- calculates R0 values for different pixels  
  

  R0_values <- vapply(
    seq_len(nrow(foi_data)),
    wrapper_to_get_multi_foi_R0, 
    numeric(ncol(foi_data)),
    foi_data = foi_data, 
    orig_data = orig_data,
    age_data = look_up, 
    age_band_lower_bounds = age_band_lower_bounds, 
    age_band_upper_bounds = age_band_upper_bounds, 
    age_band_tags = age_band_tags,
    vec_phis = vec_phis)
  
  
  # ---------------------------------------- fixes NaN
  
  
  R0_values[is.na(R0_values)] <- 0 
  
    
  # ---------------------------------------- save
  
  
  R0_values_t <- t(R0_values)
  
  out_name <- paste0("R0_", run_ID, ".rds")
    
  write_out_rds(R0_values_t, out_path, out_name)

}
