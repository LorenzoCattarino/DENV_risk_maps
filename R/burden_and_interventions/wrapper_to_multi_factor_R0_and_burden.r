wrapper_to_multi_factor_R0_and_burden <- function(
  x, foi_data, age_data,
  age_band_tags, age_band_lower_bounds, age_band_upper_bounds,
  parallel_2, var_names, FOI_values, FOI_to_Inf_list, FOI_to_C_list, prob_fun, reverse){
  
  
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
  
  
  # ---------------------------------------- create FOI -> R0 look up tables

  
  R0_values <- loop(seq_len(nrow(age_data)), 
                    wrapper_to_lookup,
                    age_struct = age_data, 
                    tags = age_band_tags, 
                    FOI_values = FOI_values, 
                    my_fun = calculate_R0,
                    N = 1,
                    prob_fun = prob_fun,
                    age_band_lower_bounds = age_band_lower_bounds, 
                    age_band_upper_bounds = age_band_upper_bounds,
                    vec_phis = vec_phis,
                    parallel = TRUE)
  
  FOI_to_R0_list <- lapply(R0_values, function(i) cbind(x = FOI_values, y = i))
  
  FOI_to_R0_list <- lapply(FOI_to_R0_list, function(i) {
    i[1, "y"] <- 1
    rbind(c(x = 0, y = 0),i)})
  
  
  # ---------------------------------------- calculates R0 values for different pixels  
  

  burden_estimates <- loop(
    seq_len(nrow(foi_data)),
    wrapper_to_replicate_R0_and_burden, 
    foi_data = foi_data, 
    age_struct = age_data,
    scaling_factor = sf,
    var_names = var_names,
    FOI_to_R0_list = FOI_to_R0_list,
    FOI_to_Inf_list = FOI_to_Inf_list,
    FOI_to_C_list = FOI_to_C_list,
    age_band_lower_bounds = age_band_lower_bounds,
    age_band_upper_bounds = age_band_upper_bounds,
    age_band_tags = age_band_tags,
    vec_phis = vec_phis, 
    prob_fun = prob_fun,
    reverse = reverse,
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
