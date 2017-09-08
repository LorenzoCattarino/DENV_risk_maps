wrapper_to_get_R0 <- function(
  FOI, N, age_struct, 
  age_band_lower_bounds, age_band_upper_bounds, age_band_tags,
  vec_phis){
  
  fun_ls <- list(
    "calculate_primary_infection_prob",
    "calculate_secondary_infection_prob",
    "calculate_tertiary_infection_prob",
    "calculate_quaternary_infection_prob")
  
  out <- rep(0, 5)
  
  if (FOI != 0) {
    
    #browser()
    
    # ----------------------------------------
    
    
    #cat("FOI value =", FOI, "\n")
    
    
    # calculate n people in age group
    n_j <- age_struct * N  
    
    
    # ---------------------------------------- calculate incidence of infections
    
    
    inf_probs <- lapply(
      fun_ls, 
      do.call,
      list(FOI, age_band_lower_bounds, age_band_upper_bounds))
    
    inc_rates <- lapply(
      inf_probs,
      calc_average_prob_infect,
      age_band_upper_bounds, 
      age_band_lower_bounds) 
    
    infec_j <- lapply(inc_rates, calculate_case_number, n_j)
    
    total_infec <- vapply(infec_j, sum, numeric(1))
    
    
    # ---------------------------------------- calculate R0
    
    
    calculate_R0(
      FOI = FOI,
      N = N,
      vec_infections = total_infec, 
      vec_phis = vec_phis)

  }

}
