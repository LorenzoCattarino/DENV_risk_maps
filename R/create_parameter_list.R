create_parameter_list <- function(grid_size = 5,
                                  resample_grid_size = 20,
                                  no_samples = 200,
                                  vec_phis_R0_1 = c(1, 1, 0),
                                  vec_phis_R0_2 = c(1, 1, 1),
                                  prop_sympt = c(0.45, 0.85, 0.15),
                                  Q_1 = 0.04,
                                  Q_3 = 0.04,
                                  Q_2 = 0.1,
                                  FOI_grid = seq(0, 0.2, 0.0002),
                                  sf_vals = seq(1, 0.1, -0.1),
                                  extra_params) {
  
  pm_list <- list()
  
  pm_list$grid_size <- grid_size
  pm_list$resample_grid_size <- resample_grid_size
  pm_list$no_samples <- no_samples
  pm_list$vec_phis_R0_1 <- vec_phis_R0_1
  pm_list$vec_phis_R0_2 <- vec_phis_R0_2
  pm_list$prop_sympt <- prop_sympt
  pm_list$Q_1 <- Q_1
  pm_list$Q_3 <- Q_3
  pm_list$Q_2 <- Q_2
  pm_list$FOI_grid <- FOI_grid
  pm_list$sf_vals <- sf_vals
  
  c(pm_list, extra_params)
   
} 
