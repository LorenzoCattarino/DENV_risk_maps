calculate_total_incidence_rate <- function(
  inc_rates_list, 
  rho, gamma_1, gamma_3){
  
  I1_rate <- inc_rates_list[[1]] 
  I2_rate <- inc_rates_list[[2]]
  I3_rate <- inc_rates_list[[3]] 
  I4_rate <- inc_rates_list[[4]]
  
  rho * I2_rate + (gamma_1 * I1_rate) + (gamma_3 * (I3_rate + I4_rate))
  
}
