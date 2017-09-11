burden_multi_factor_wrapper <- function(
  x, foi_data, orig_data,
  age_band_tags, age_band_lower_bounds, 
  age_band_upper_bounds, 
  w_1, w_2, w_3,
  look_up, parallel_2, var_names){
  
  
  #browser()
  
  # ---------------------------------------- Extract factor values
  
  
  run_ID <- x$id
  cat("ID run =", run_ID, "\n")

  phi_1 <- x$phi1
  phi_2 <- x$phi2
  phi_3 <- x$phi3
  phi_4 <- x$phi4
  
  sf <- x$scaling_factor
  
  # ---------------------------------------- define variables
 

  vec_phis <- c(phi_1, phi_2, phi_3, phi_4)

  
  # ---------------------------------------- calculates R0 values for different pixels  
  

  burden_estimates <- loop(
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
    w_3 = w_3,
    var_names = var_names,
    parallel = parallel_2)
  
  
  # ---------------------------------------- reshape and save


  out <- do.call("rbind", burden_estimates)
  
  var_names <- paste(var_names, run_ID, sep = "_")
  
  setNames(as.data.frame(out), var_names)
  
  # fl_nm <- paste0("R0_and_burden_", run_ID, ".rds")
  # 
  # write_out_rds(out2, 
  #               "output/predictions_world/boot_model_20km_cw",
  #               fl_nm)
  
  # ret <- vector("list", length = length(var_names))
  #   
  # for (b in seq_along(var_names)){
  #   
  #   ret1 <- lapply(burden_estimates, "[", b, TRUE)
  # 
  #   ret2 <- do.call("rbind", ret1)  
  #   
  #   ret[[b]] <- ret2
  # 
  # }
  # 
  # ret

}
