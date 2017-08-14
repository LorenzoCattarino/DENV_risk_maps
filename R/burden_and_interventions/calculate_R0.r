# Calculates R0 using the 'at equilibrium' numbers of total primary to quaternary infections 

calculate_R0 <- function(
  FOI, N, vec_infections, vec_phis){
    
  I1 <- vec_infections[1]
  I2 <- vec_infections[2]
  I3 <- vec_infections[3]
  I4 <- vec_infections[4]
  
  phi_1 <- vec_phis[1]
  phi_2 <- vec_phis[2]
  phi_3 <- vec_phis[3]
  phi_4 <- vec_phis[4]
  
  FOI * N / (phi_1 * I1 + phi_2 * I2 + phi_3 * I3 + phi_4 * I4)
  
}
