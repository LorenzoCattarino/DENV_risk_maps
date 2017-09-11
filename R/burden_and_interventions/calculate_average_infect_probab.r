calc_average_prob_infect <- function(
  infect_prob, a, b){
  
  (infect_prob / 4) / (a - b)

}

calculate_case_number <- function(
  incidence_rate, age_band_pop){
  
  incidence_rate * age_band_pop
  
}
