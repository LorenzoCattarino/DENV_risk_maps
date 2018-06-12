wrapper_to_R0_and_burden <- function(FOI, 
                                     n_j, 
                                     vars,
                                     age_band_lower_bounds, 
                                     age_band_upper_bounds,
                                     vec_phis, 
                                     prob_fun, 
                                     scaling_factor, 
                                     FOI_to_R0, 
                                     FOI_to_Inf, 
                                     FOI_to_C,
                                     var_to_fit){

  browser()
  
  out <- setNames(rep(0, length(vars)), vars)
  
  #cat("FOI =", FOI, "\n")
  #cat("scaling factor =", scaling_factor, "\n")
  
  if (FOI > 0) {
    

    # ---------------------------------------- calculate R0
    
    
    if(var_to_fit == "FOI"){
      
      R0 <- calculate_R0(
      FOI = FOI, 
      N = 1, 
      n_j = n_j, 
      age_band_lower_bounds = age_band_lower_bounds, 
      age_band_upper_bounds = age_band_upper_bounds,
      vec_phis = vec_phis, 
      prob_fun = prob_fun)
    
    } else {
      
      R0 <- FOI
    
    }
    
    #cat("R0 =", R0, "\n")
    
    
    # ---------------------------------------- calculate reduced R0 
    
    
    red_R0 <- R0 * scaling_factor
    #cat("reduced R0 =", red_R0, "\n")
    
    
    # ---------------------------------------- look up and linear interpolation to calculate reduced FOI
    
    
    red_FOI <- approx(FOI_to_R0[, "y"], FOI_to_R0[, "x"], xout = red_R0)$y
    #cat("reduced FOI =", red_FOI, "\n")
    
    
    # ---------------------------------------- look up and linear interpolation to calculate burden measures
    
    
    Infections_per_capita <- approx(FOI_to_Inf[, "x"], FOI_to_Inf[, "y"], xout = red_FOI)$y 
    Cases_per_capita <- approx(FOI_to_C[, "x"], FOI_to_C[, "y"], xout = red_FOI)$y


    # ----------------------------------------
    
    
    if(var_to_fit == "FOI"){
      
      out <- setNames(c(red_FOI, red_R0, Infections_per_capita, Cases_per_capita),
                      vars)
      
    } else {
      
      out <- setNames(c(red_R0, red_FOI, Infections_per_capita, Cases_per_capita),
                      vars)
      
    }
    
  }
  
  out
}
