get_burden_measures_one_FOI <- function(
  x, age_struc_data, 
  age_band_lower_bounds, age_band_upper_bounds, age_band_tags,
  vec_phis, scaling_factor, 
  sympt_1_infec, sympt_2_infec, sympt_3_infec,
  info_1, info_2, info_3){
  
  
  # ---------------------------------------- extract info for ONE foi value
  
  
  FOI <- as.numeric(x["mean_pred"])
  #cat("FOI value =", FOI, "\n")
  
  adm0 <- x["ID_0"]
  #cat("country code =", adm0, "\n")
  
  total_pop <- as.numeric(x["population"])
  #cat("population =", total_pop, "\n")
  
  
  # ---------------------------------------- start
  
  
  out <- setNames(rep(0, length(info_3) + length(info_2) + length(info_1)),
                  c(info_3, info_2, info_1))
  
  # get age structure for the FOI location 
  ag_st <- age_struc_data[age_struc_data$ID_0 == adm0, age_band_tags]
  
  if(nrow(ag_st) != 0 && sum(ag_st) != 0){
    
    # calculate n people in age group
    n_j <- ag_st * total_pop  
    
    
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
      N = total_pop,
      vec_infections = c(total_infec_1, total_infec_2, total_infec_3, total_infec_4), 
      vec_phis = vec_phis) 
    
    
    # ------------------------------------- create look up functions to transform R0 back to FOI
    
    
    R0_to_FOI <- approximate(
      target = calculate_R0, 
      a = 0, 
      b = 2,
      N = total_pop,
      vec_infections = c(total_infec_1, total_infec_2, total_infec_3, total_infec_4), 
      vec_phis = vec_phis,
      tol = 1e-5,
      target_vectorised = FALSE,
      inverse = TRUE) 
    
    
    # ------------------------------------- calculate reduced R0 and back transform R0
    
    
    # Calculate reduced R0 values 
    red_R0 <- R0 * scaling_factor
    
    # Transform reduced R0 back to FOI 
    red_FOI <- R0_to_FOI(red_R0)
    
    # Assigning 0 to negative back transformed FOI values 
    red_FOI[red_FOI < 0] <- 0
      
    
    # ------------------------------------- calculate burden measures 
    
    
    # calculate total number of infections in the population N 
    a <- sum(total_infec_1, total_infec_2, total_infec_3, total_infec_4)
    
    # calculate total incidence of cases per age group 
    tot_incid_rate_j <- calculate_total_incidence_rate(
      I1_rate = inc_1_rate, 
      I2_rate = inc_2_rate,
      I3_rate = inc_3_rate,
      I4_rate = inc_4_rate, 
      rho = sympt_1_infec, 
      gamma_1 = sympt_2_infec, 
      gamma_3 = sympt_3_infec)
    
    # calculate total number of cases per age group 
    case_number_j <- calculate_case_number(tot_incid_rate_j, n_j)
    
    # calculate total number of cases in the population N
    b <- sum(case_number_j)
    
    c <- calculate_incidence_of_infections(a, total_pop)
    
    d <- calculate_incidence_of_infections(b, total_pop)
     
    out <- c(x[info_3], R0, red_R0, red_FOI, a, b, c, d)
    
  }
  
  out
  
}
