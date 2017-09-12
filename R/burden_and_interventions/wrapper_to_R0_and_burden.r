wrapper_to_R0_and_burden <- function(
  FOI, n_j, 
  age_band_lower_bounds, age_band_upper_bounds, 
  vec_phis, scaling_factor,
  rho, gamma_1, gamma_3, 
  var_names, R0_to_FOI){

  browser()
  
  out <- rep(0, length(var_names))
  
  cat("FOI =", FOI, "\n")
  cat("infectiousness =", vec_phis, "\n")  
  cat("scaling factor =", scaling_factor, "\n")
  
  if (FOI > 0) {
    

    N <- sum(n_j)
    
    # ---------------------------------------- calculate R0
    
    
    R0 <- calculate_R0(
      FOI = FOI, 
      N = N, 
      n_j = n_j, 
      age_band_lower_bounds = age_band_lower_bounds, 
      age_band_upper_bounds = age_band_upper_bounds,
      vec_phis = vec_phis, 
      prob_fun = prob_fun)
    
    cat("R0 =", R0, "\n")
    
    
    # ---------------------------------------- calculate reduced R0 and back transform R0
    
    
    red_R0 <- R0 * scaling_factor
    cat("reduced R0 =", red_R0, "\n")
    
    red_FOI <- R0_to_FOI(red_R0)
    cat("reduced FOI =", red_FOI, "\n")
    
    
    # ---------------------------------------- recalculate everything 
    
    
    inf_probs <- lapply(
      prob_fun, 
      do.call,
      list(red_FOI, age_band_lower_bounds, age_band_upper_bounds))
    
    inc_rates <- lapply(
      inf_probs,
      calc_average_prob_infect,
      age_band_upper_bounds, 
      age_band_lower_bounds) 
    
    infec_j <- lapply(inc_rates, calculate_case_number, n_j)
    
    total_infec <- vapply(infec_j, sum, numeric(1))
    
    
    # ---------------------------------------- calculate burden measures
    
    
    # total number of infections in the population N
    a <- sum(total_infec)
    
    I1_rate <- inc_rates[[1]] 
    I2_rate <- inc_rates[[2]]
    I3_rate <- inc_rates[[3]] 
    I4_rate <- inc_rates[[4]]    
    
    # total incidence of cases per age group
    tot_incid_rate_j <- rho * I2_rate + (gamma_1 * I1_rate) + (gamma_3 * (I3_rate + I4_rate))  
    
    # total number of cases per age group
    case_number_j <- calculate_case_number(tot_incid_rate_j, n_j)
    
    # total number of cases in the population N
    b <- sum(case_number_j)
    
    c <- (a / N) * 1000
    
    d <- (b / N) * 1000
    
    
    # ----------------------------------------
    
    
    cat("number of infections =", a, "\n")
    cat("number of cases =", b, "\n")
    cat("incidence of infections =", c, "\n")
    cat("incidence of infections =", d, "\n")
    
    out <- c(red_R0, a, b, c, d)
    
  }
  
  out
}
