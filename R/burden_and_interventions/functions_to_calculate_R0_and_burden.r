calculate_infections <- function(
  FOI, n_j, 
  prob_fun, age_band_lower_bounds, age_band_upper_bounds){
  
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
  
  sum(total_infection_number)
  
}

calculate_cases <- function(
  FOI, n_j, 
  prob_fun, age_band_lower_bounds, age_band_upper_bounds,
  rho, gamma_1, gamma_3){
  
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
  
  sum(case_number_j)
  
}

wrapper_to_lookup <- function(i, age_struct, tags, FOI_values, my_fun, ...){
  
  m_j <- age_struct[i, tags]
  vapply(
    FOI_values,
    my_fun,
    numeric(1),
    n_j = m_j,
    ...)
  
}
