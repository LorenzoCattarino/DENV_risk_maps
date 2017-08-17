burden_multi_factor_wrapper <- function(
  x, foi_data, orig_data,
  age_band_tags, age_band_lower_bounds, 
  age_band_upper_bounds, sympt_weights,
  look_up){
  
  
  browser()
  
  
  # ---------------------------------------- define parameters 
  
  
  burden_measures_tags <- c(
    "number_of_infections", 
    "number_of_cases", 
    "incidence_of_infections", 
    "incidence_of_cases")
  
  interm_outs <- c("R0", "red_R0", "red_FOI")
  
  base_info <- c("OBJECTID", "ID_0", "ID_1", "population")
  
  
  # ---------------------------------------- Extract factor values
  
  
  run_ID <- x$ID_run
  cat("run ID =", run_ID, "\n")

  sf <- x$scaling_factor
  cat("scaling factor =", sf, "\n")
  
  infss <- x$phi
  cat("infectiousness =", infss, "\n")
  
  
  # ---------------------------------------- define other variables
  

  w_1 <- sympt_weights[1]
  w_2 <- sympt_weights[2]
  w_3 <- sympt_weights[3]
 
  if(infss == 1)
  {
    # Up to 2 infections
    phi_1 <- phi_2 <- 1
    phi_3 <- phi_4 <- 0    
  } 
  
  if(infss == 2)
  {
    # Up to 3 infections
    phi_1 <- phi_2 <- phi_3 <- 1
    phi_4 <- 0
  }

  if(infss == 3)
  {
    # Up to 4 infections 
    phi_1 <- phi_2 <- phi_3 <- phi_4 <- 1
    
  }
  
  if(infss == 4)
  {
    
    # up to 4, with symptomatic infections twice as infectious as asymptomatic ones
    phi_2 <- 1
    phi_1 <- (w_1 * 2 + (1 - w_1)) / (w_2 * 2 + (1 - w_2)) 
    phi_3 <- phi_4 <- (w_3 * 2 + (1 - w_3)) / (w_2 * 2 + (1 - w_2))
  
  }
  
  vec_phis <- c(phi_1, phi_2, phi_3, phi_4)

  
  # ---------------------------------------- get R0 values for different foi value  
  
  
  apply(
    predicted_FOI,
    1,
    wrapper_to_get_multi_foi_R0, 
    foi_data = , 
    orig_data = ,
    age_data = look_up, 
    age_band_lower_bounds = age_band_lower_bounds, 
    age_band_upper_bounds = age_band_upper_bounds, 
    age_band_tags = age_band_tags,
    vec_phis = vec_phis)
  
  
  # # ---------------------------------------- cleaning 
  # 
  # 
  # # get character fields 
  # chr_flds <- which(lapply(predicted_FOI, class) == "character")
  # 
  # # remove character fields
  # predicted_FOI_2 <- predicted_FOI[setdiff(names(predicted_FOI), names(chr_flds))]
  # 
  # predicted_FOI_2 <- as.matrix(predicted_FOI_2)
  # 
  # rownames(predicted_FOI_2) <- NULL
  # 
  # 
  # # ---------------------------------------- run 
  # 
  # 
  # out_mat <- apply(
  #   predicted_FOI_2, 
  #   1, 
  #   get_burden_measures_one_FOI, 
  #   age_struc_data, 
  #   age_band_lower_bounds, 
  #   age_band_upper_bounds,
  #   age_band_tags,
  #   vec_phis, 
  #   scaling_factor,
  #   sympt_1_infec = w_1, 
  #   sympt_2_infec = w_2, 
  #   sympt_3_infec = w_2,
  #   info_1 = burden_measures_tags, 
  #   info_2 = interm_outs, 
  #   info_3 = base_info)
  #   
  # out_mat <- t(out_mat)
  # 
  # colnames(out_mat) <- 1:dim(out_mat)[2]
  # 
  # colnames(out_mat) <- c(base_info, interm_outs, burden_measures_tags)
  #   
  # as.data.frame(out_mat)
  
}
