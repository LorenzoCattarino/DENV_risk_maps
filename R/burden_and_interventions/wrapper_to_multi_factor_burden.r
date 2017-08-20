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
  
  
  out_tags <- c("R0", "I_num", "C_num")#, "I_inc", "C_inc")
  
  
  # ---------------------------------------- define variables
 

  vec_phis <- c(phi_1, phi_2, phi_3, phi_4)

  
  # ---------------------------------------- calculates R0 values for different pixels  
  

  burden_estimates <- lapply(
    seq_len(nrow(foi_data)),
    wrapper_to_get_multi_foi_R0, 
    foi_data = foi_data, 
    orig_data = orig_data,
    age_data = look_up, 
    age_band_lower_bounds = age_band_lower_bounds, 
    age_band_upper_bounds = age_band_upper_bounds, 
    age_band_tags = age_band_tags,
    vec_phis = vec_phis,
    scaling_factor = sf,
    w_1 = w_1, 
    w_2 = w_2, 
    w_3 = w_3)
  
  
  # ---------------------------------------- save

  
  for (b in seq_along(out_tags)){
    
    ret1 <- lapply(burden_estimates, "[", b, TRUE)
  
    ret2 <- do.call("rbind", ret1)  
    
    out_name <- paste0(out_tags[b], "_", run_ID, ".rds")
    
    write_out_rds(ret2, out_path, out_name)
  
  }
  
}
