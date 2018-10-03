calculate_infections <- function(FOI, 
                                 n_j, 
                                 prob_fun, 
                                 age_band_lower_bounds, 
                                 age_band_upper_bounds){
  
  infection_probabilities <- lapply(
    prob_fun, 
    do.call,
    list(FOI, age_band_lower_bounds, age_band_upper_bounds))
  
  infection_incidences <- lapply(
    infection_probabilities,
    calc_average_prob_infect,
    age_band_upper_bounds, 
    age_band_lower_bounds) 
  
  infection_numbers_j <- lapply(infection_incidences, calculate_case_number, n_j)
  
  total_infection_number <- vapply(infection_numbers_j, sum, numeric(1))
  
  sum(total_infection_number) * 4
  
}

calculate_cases <- function(FOI, 
                            n_j, 
                            prob_fun, 
                            age_band_lower_bounds, 
                            age_band_upper_bounds,
                            parms){
  
  rho <- parms$rho 
  gamma_1 <- parms$gamma_1
  gamma_3 <- parms$gamma_3
  
  infection_probabilities <- lapply(
    prob_fun, 
    do.call,
    list(FOI, age_band_lower_bounds, age_band_upper_bounds))
  
  infection_incidences <- lapply(
    infection_probabilities,
    calc_average_prob_infect,
    age_band_upper_bounds, 
    age_band_lower_bounds) 
  
  I1_rate <- infection_incidences[[1]] 
  I2_rate <- infection_incidences[[2]]
  I3_rate <- infection_incidences[[3]] 
  I4_rate <- infection_incidences[[4]]    
  
  tot_incid_rate_j <- rho * I2_rate + (gamma_1 * I1_rate) + (gamma_3 * (I3_rate + I4_rate))  
  
  case_number_j <- calculate_case_number(tot_incid_rate_j, n_j)
  
  sum(case_number_j) * 4
  
}

calculate_hosp_cases <- function(FOI, 
                                 n_j, 
                                 prob_fun, 
                                 age_band_lower_bounds, 
                                 age_band_upper_bounds,
                                 parms){
  
  rho <- parms$rho 
  gamma_1 <- parms$gamma_1
  gamma_3 <- parms$gamma_3
  
  Q_1 <- parms$Q_1
  Q_2 <- parms$Q_2
  Q_3 <- parms$Q_3
  Q_4 <- parms$Q_4
    
  infection_probabilities <- lapply(
    prob_fun, 
    do.call,
    list(FOI, age_band_lower_bounds, age_band_upper_bounds))
  
  infection_incidences <- lapply(
    infection_probabilities,
    calc_average_prob_infect,
    age_band_upper_bounds, 
    age_band_lower_bounds) 
  
  I1_rate <- infection_incidences[[1]] 
  I2_rate <- infection_incidences[[2]]
  I3_rate <- infection_incidences[[3]] 
  I4_rate <- infection_incidences[[4]]    
  
  case_1_number_j <- calculate_case_number(gamma_1 * I1_rate, n_j)
  
  case_2_number_j <- calculate_case_number(rho * I2_rate, n_j)
  
  case_3_number_j <- calculate_case_number(gamma_3 * I3_rate, n_j)
  
  case_4_number_j <- calculate_case_number(gamma_3 * I4_rate, n_j)
  
  case_1_number <- sum(case_1_number_j) * 4
  case_2_number <- sum(case_2_number_j) * 4
  case_3_number <- sum(case_3_number_j) * 4
  case_4_number <- sum(case_4_number_j) * 4
  
  (Q_1 * case_1_number) + (Q_2 * case_2_number) + (Q_3 * case_3_number) + (Q_4 * case_4_number)  

}

wrapper_to_lookup <- function(i, 
                              age_struct, 
                              tags, 
                              FOI_values, 
                              my_fun, ...){
  
  m_j <- age_struct[i, tags]
  vapply(
    FOI_values,
    my_fun,
    numeric(1),
    n_j = m_j,
    ...)
  
}

fix_R0_lookup_limits <- function(i) {
  
  i[1, "y"] <- 1
  
  rbind(c(x = 0, y = 0), i)

}

cbind_FOI_to_lookup <- function(i, FOI_values) {
  
  cbind(x = FOI_values, y = i)

}
