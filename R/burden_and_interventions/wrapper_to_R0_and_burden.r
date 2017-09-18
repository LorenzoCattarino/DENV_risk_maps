wrapper_to_R0_and_burden <- function(
  FOI, n_j,
  age_band_lower_bounds, age_band_upper_bounds,
  vec_phis, prob_fun, scaling_factor, var_names, 
  FOI_to_R0, FOI_to_Inf, FOI_to_C,
  N){

  #browser()
  
  out <- rep(0, length(var_names))
  
  #cat("FOI =", FOI, "\n")
  #cat("scaling factor =", scaling_factor, "\n")
  
  if (FOI > 0) {
    

    # ---------------------------------------- calculate R0
    
    
    R0 <- calculate_R0(
      FOI = FOI, 
      N = 1, 
      n_j = n_j, 
      age_band_lower_bounds = age_band_lower_bounds, 
      age_band_upper_bounds = age_band_upper_bounds,
      vec_phis = vec_phis, 
      prob_fun = prob_fun)
    
    #cat("R0 =", R0, "\n")
    
    
    # ---------------------------------------- calculate reduced R0 
    
    
    red_R0 <- R0 * scaling_factor
    #cat("reduced R0 =", red_R0, "\n")
    
    
    # ---------------------------------------- look up and linear interpolation to calculate reduced FOI
    
    
    red_FOI <- approx(FOI_to_R0[, "y"], FOI_to_R0[, "x"], xout = red_R0)$y
    #cat("reduced FOI =", red_FOI, "\n")
    
    
    # ---------------------------------------- look up and linear interpolation to calculate burden measures
    
    
    # for one serotype
    Infections_per_capita <- approx(FOI_to_Inf[, "x"], FOI_to_Inf[, "y"], xout = red_FOI)$y 
    Cases_per_capita <- approx(FOI_to_C[, "x"], FOI_to_C[, "y"], xout = red_FOI)$y

    # for four serotypes
    Infections_per_capita <- Infections_per_capita * 4
    Cases_per_capita <- Cases_per_capita * 4
    
    Infections <- Infections_per_capita * N
    Cases <- Cases_per_capita * N
    
    Inc_infections <- Infections_per_capita * 1000
    Inc_cases <- Cases_per_capita * 1000
    

    # ----------------------------------------
    

    #cat("number of infections =", Infections, "\n")
    #cat("number of cases =", Cases, "\n")
    #cat("incidence of infections =", Inc_infections, "\n")
    #cat("incidence of cases =", Inc_cases, "\n")
    
    out <- setNames(c(red_R0, Infections, Cases, Inc_infections, Inc_cases),
                    c("R0", "I_num", "C_num", "I_inc", "C_inc"))
    
  }
  
  out[var_names]
}