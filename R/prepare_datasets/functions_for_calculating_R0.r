# Functions to calculate R0 using the 'at equilibrium' numbers of total primary to quaternary infections

calculate_infectiousness_wgts_for_sym_asym_assumption <- function(parms){
  
  w_1 <- parms$gamma_1
  w_2 <- parms$rho
  w_3 <- parms$gamma_3
  
  phi_2 <- 1
  phi_1 <- (w_1 * 2 + (1 - w_1)) / (w_2 * 2 + (1 - w_2)) 
  phi_3 <- phi_4 <- (w_3 * 2 + (1 - w_3)) / (w_2 * 2 + (1 - w_2))
  
  c(phi_1, phi_2, phi_3, phi_4)
}

get_age_band_bounds <- function(tags) {
  
  age_band_tags_split <- strsplit(tags, "[^0-9]+")
  
  age_band_tags_split_num <- lapply(age_band_tags_split, as.numeric)
  
  age_band_tags_split_num_mat <- do.call("rbind", age_band_tags_split_num)
  
  age_band_tags_split_num_mat[, 2:3]
  
}

wrapper_to_multi_factor_R0 <- function(
  x, foi_data, age_struct, 
  age_band_tags, age_band_lower_bounds, age_band_upper_bounds, prob_fun){
  
  phi_1 <- x[1]
  phi_2 <- x[2]
  phi_3 <- x[3]
  phi_4 <- x[4]
  
  vec_phis <- c(phi_1, phi_2, phi_3, phi_4)
  
  n <- nrow(foi_data)
  
  vapply(seq_len(n),
         wrapper_to_R0, 
         numeric(1),
         foi_data = All_FOI_estimates_3, 
         age_struct = age_struct, 
         age_band_lower_bounds = age_band_L_bounds, 
         age_band_upper_bounds = age_band_U_bounds, 
         age_band_tags = age_band_tgs,
         vec_phis = vec_phis,
         prob_fun = prob_fun)
  
}

wrapper_to_R0 <- function(
  i, foi_data, age_struct, 
  age_band_tags, age_band_lower_bounds, age_band_upper_bounds, 
  vec_phis, prob_fun){
  
  m_j <- age_struct[age_struct$ID_0 == foi_data[i, "ID_0"], age_band_tags]
  FOI <- foi_data[i, "FOI"]
  
  calculate_R0(
    FOI = FOI, 
    N = 1, 
    n_j = m_j, 
    age_band_lower_bounds = age_band_lower_bounds, 
    age_band_upper_bounds = age_band_upper_bounds,
    vec_phis = vec_phis, 
    prob_fun = prob_fun)

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
