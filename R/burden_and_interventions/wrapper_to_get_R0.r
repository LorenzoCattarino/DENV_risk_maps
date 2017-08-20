wrapper_to_get_R0 <- function(
  FOI, N, age_struct, 
  age_band_lower_bounds, age_band_upper_bounds, age_band_tags,
  vec_phis, scaling_factor,
  w_1, w_2, w_3){
  
  
  out <- rep(0, 5)
    
  
  if (FOI != 0) {
    
    #browser()
    
    # ----------------------------------------
    
    
    #cat("FOI value =", FOI, "\n")
    
    
    # calculate n people in age group
    n_j <- age_struct * N  
    
    
    # ---------------------------------------- calculate incidence of infections
    
    
    inf_1_prob <- calculate_primary_infection_prob( 
      FOI, 
      age_band_lower_bounds, 
      age_band_upper_bounds)
    
    inf_2_prob <- calculate_secondary_infection_prob( 
      FOI,
      age_band_lower_bounds, 
      age_band_upper_bounds)
    
    inf_3_prob <- calculate_tertiary_infection_prob(
      FOI,
      age_band_lower_bounds, 
      age_band_upper_bounds)
    
    inf_4_prob <- calculate_quaternary_infection_prob(
      FOI, 
      age_band_lower_bounds, 
      age_band_upper_bounds)
    
    inc_1_rate <- calc_average_prob_infect(inf_1_prob, age_band_upper_bounds, age_band_lower_bounds)
    
    inc_2_rate <- calc_average_prob_infect(inf_2_prob, age_band_upper_bounds, age_band_lower_bounds)
    
    inc_3_rate <- calc_average_prob_infect(inf_3_prob, age_band_upper_bounds, age_band_lower_bounds)
    
    inc_4_rate <- calc_average_prob_infect(inf_4_prob, age_band_upper_bounds, age_band_lower_bounds)
    
    infec_1_j <- calculate_case_number(inc_1_rate, n_j)
    
    infec_2_j <- calculate_case_number(inc_2_rate, n_j)
    
    infec_3_j <- calculate_case_number(inc_3_rate, n_j)
    
    infec_4_j <- calculate_case_number(inc_4_rate, n_j)
    
    total_infec_1 <- sum(infec_1_j) 
    
    total_infec_2 <- sum(infec_2_j)
    
    total_infec_3 <- sum(infec_3_j)
    
    total_infec_4 <- sum(infec_4_j)
    
    
    # ------------------------------------- calculate R0
    

    R0 <- calculate_R0(
      FOI = FOI,
      N = N,
      vec_infections = c(total_infec_1, total_infec_2, total_infec_3, total_infec_4), 
      vec_phis = vec_phis)
    
    
    # ---------------------------------------- create look up function to transform R0 back to FOI
    
    
    R0_to_FOI <- approximate(
      target = calculate_R0,
      a = 0,
      b = 2,
      N = N,
      vec_infections = c(total_infec_1, total_infec_2, total_infec_3, total_infec_4),
      vec_phis = vec_phis,
      tol = 1e-5,
      target_vectorised = FALSE,
      inverse = TRUE)
    
    
    # ---------------------------------------- calculate reduced R0 and back transform R0
    
    
    red_R0 <- R0 * scaling_factor
    
    red_FOI <- R0_to_FOI(red_R0)
    
    red_FOI[red_FOI < 0] <- 0
    
    
    # ---------------------------------------- calculate burden measures
    
    
    # total number of infections in the population N
    a <- sum(total_infec_1, total_infec_2, total_infec_3, total_infec_4)
    
    # total incidence of cases per age group
    tot_incid_rate_j <- calculate_total_incidence_rate(
      I1_rate = inc_1_rate,
      I2_rate = inc_2_rate,
      I3_rate = inc_3_rate,
      I4_rate = inc_4_rate,
      rho = w_1,
      gamma_1 = w_2,
      gamma_3 = w_3)
    
    # total number of cases per age group
    case_number_j <- calculate_case_number(tot_incid_rate_j, n_j)
    
    # total number of cases in the population N
    b <- sum(case_number_j)
    
    c <- calculate_incidence_of_infections(a, N)
    
    d <- calculate_incidence_of_infections(b, N)
    
    
    # ----------------------------------------
    
    
    out <- c(R0, a, b, c, d)
    
  }
  
  out
  
}
