# Calculates R0 using the 'at equilibrium' numbers of total primary to quaternary infections

calculate_primary_infection_prob <- function(
  
  FOI, start_ages_vec, end_ages_vec) {
  
  exp(-4 * FOI * start_ages_vec) - exp(-4 * FOI * end_ages_vec)
  
}  
  
calculate_secondary_infection_prob <- function(
  
  FOI, start_ages_vec, end_ages_vec) {

  4 * (exp(-3 * FOI * start_ages_vec) - exp(-3 * FOI * end_ages_vec)) - 
  3 * (exp(-4 * FOI * start_ages_vec) - exp(-4 * FOI * end_ages_vec))
  
}  

calculate_tertiary_infection_prob <- function(
  
  FOI, start_ages_vec, end_ages_vec) {
  
  6 * (exp(-2 * FOI * start_ages_vec) - exp(-2 * FOI * end_ages_vec)) + 
  8 * (exp(-3 * FOI * end_ages_vec) - exp(-3 * FOI * start_ages_vec)) + 
  3 * (exp(-4 * FOI * start_ages_vec) - exp(-4 * FOI * end_ages_vec))
  
}  

calculate_quaternary_infection_prob <- function(
  
  FOI, start_ages_vec, end_ages_vec) {
  
  4 * (exp(-FOI * start_ages_vec) - exp(-FOI * end_ages_vec) +
       exp(-3 * FOI * start_ages_vec) - exp(-3 * FOI * end_ages_vec)) + 
  6 * (exp(-2 * FOI * end_ages_vec) - exp(-2 * FOI * start_ages_vec)) + 
      (exp(-4 * FOI * end_ages_vec) - exp(-4 * FOI * start_ages_vec)) 
  
}  

calc_average_prob_infect <- function(
  infect_prob, a, b){
  
  (infect_prob / 4) / (a - b)
  
}

calculate_case_number <- function(
  incidence, age_band_pop){
  
  incidence * age_band_pop
  
}

get_age_band_bounds <- function(tags) {
  
  age_band_tags_split <- strsplit(tags, "[^0-9]+")
  
  age_band_tags_split_num <- lapply(age_band_tags_split, as.numeric)
  
  age_band_tags_split_num_mat <- do.call("rbind", age_band_tags_split_num)
  
  age_band_tags_split_num_mat[, 2:3]
  
}

calculate_R0 <- function(
  FOI, N, n_j, 
  prob_fun, age_band_lower_bounds, age_band_upper_bounds,
  vec_phis){
  
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
  
  total_infection_numbers <- vapply(infection_numbers_j, sum, numeric(1))
  
  FOI * N / (sum(total_infection_numbers * vec_phis))
  
}

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

calculate_infectiousness_wgts_for_sym_asym_assumption <- function(w_1, w_2, w_3){
  
  phi_2 <- 1
  phi_1 <- (w_1 * 2 + (1 - w_1)) / (w_2 * 2 + (1 - w_2)) 
  phi_3 <- phi_4 <- (w_3 * 2 + (1 - w_3)) / (w_2 * 2 + (1 - w_2))
  
  c(phi_1, phi_2, phi_3, phi_4)
}
