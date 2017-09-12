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
  incidence_rate, age_band_pop){
  
  incidence_rate * age_band_pop
  
}

get_age_band_bounds <- function(tags) {
  
  age_band_tags_split <- strsplit(tags, "[^0-9]+")
  
  age_band_tags_split_num <- lapply(age_band_tags_split, as.numeric)
  
  age_band_tags_split_num_mat <- do.call("rbind", age_band_tags_split_num)
  
  age_band_tags_split_num_mat[, 2:3]
  
}

calculate_R0 <- function(
  FOI, N, n_j, 
  age_band_lower_bounds, age_band_upper_bounds,
  vec_phis, prob_fun){
  
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
