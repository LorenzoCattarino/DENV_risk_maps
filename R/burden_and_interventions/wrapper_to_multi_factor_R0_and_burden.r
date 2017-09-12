wrapper_to_multi_factor_R0_and_burden <- function(
  x, foi_data, orig_data,
  age_band_tags, age_band_lower_bounds, 
  age_band_upper_bounds, 
  rho, gamma_1, gamma_3,
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
    
    
  # ---------------------------------------- define parameters 
  
  
  prob_fun <- list("calculate_primary_infection_prob",
                   "calculate_secondary_infection_prob",
                   "calculate_tertiary_infection_prob",
                   "calculate_quaternary_infection_prob")
  
  
  # ---------------------------------------- define variables
 

  vec_phis <- c(phi_1, phi_2, phi_3, phi_4)

  max_FOI <- max(foi_data$mean_pred)
  
  
  # ---------------------------------------- create look up function to back map foi from R0

  
  R0_to_FOI_list <- apply(seq_len(nrow(look_up)), 
                          get_all_look_up_functions, 
                          look_up = look_up, 
                          age_band_tags = age_band_tags, 
                          vec_phis = vec_phis, 
                          prob_fun = prob_fun, 
                          max_FOI = max_FOI, 
                          age_band_lower_bounds = age_band_lower_bounds, 
                          age_band_upper_bounds = age_band_upper_bounds)
  
  
  # ---------------------------------------- calculates R0 values for different pixels  
  

  burden_estimates <- loop(
    seq_len(nrow(foi_data)),
    wrapper_to_replicate_R0_and_burden, 
    foi_data = foi_data, 
    look_up = look_up,
    age_band_tags = age_band_tags,
    age_band_lower_bounds = age_band_lower_bounds, 
    age_band_upper_bounds = age_band_upper_bounds, 
    vec_phis = vec_phis,
    scaling_factor = sf,
    rho = rho, 
    gamma_1 = gamma_1, 
    gamma_3 = gamma_3,
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
