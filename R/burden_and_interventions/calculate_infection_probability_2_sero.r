calculate_primary_infection_prob_2_sero <- function(
  
  FOI, start_ages_vec, end_ages_vec) {
  
  exp(-2 * FOI * start_ages_vec) - exp(-2 * FOI * end_ages_vec)
  
}  

calculate_secondary_infection_prob_2_sero <- function(
  
  FOI, start_ages_vec, end_ages_vec) {
  
  2 * (exp(-FOI * start_ages_vec) - exp(-FOI * end_ages_vec)) - 
    (exp(-2 * FOI * start_ages_vec) - exp(-2 * FOI * end_ages_vec))
  
}  
