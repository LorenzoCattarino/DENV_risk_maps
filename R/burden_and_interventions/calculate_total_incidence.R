calculate_total_incidence_rate <- function(
  I1_rate, I2_rate, I3_rate, I4_rate, 
  rho, gamma_1, gamma_3){
  
  rho * I2_rate + (gamma_1 * I1_rate) + (gamma_3 * (I3_rate + I4_rate))
  
}
