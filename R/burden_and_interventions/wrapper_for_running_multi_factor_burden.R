burden_multi_factor_wrapper <- function(
  x, list_of_data, my_path,
  age_band_tags, age_band_lower_bounds, 
  age_band_upper_bounds, sympt_weights, 
  age_struc_data){
  
  
  # ---------------------------------------- Extract factor values
  
  
  exp_ID <- x$ID.exp
  #cat("exp ID =", exp_ID, "\n")
  
  run_ID <- x$ID.run
  #cat("run ID =", run_ID, "\n")

  adm <- x$adm
  #cat("admin unit of predictions =", adm, "\n")

  scaling_factor <- x$scaling_factor
  #cat("scaling factor =", scaling_factor, "\n")
  
  infectiousness <- x$phi
  #cat("infectiousness =", infectiousness, "\n")
  
  
  # ---------------------------------------- define other variables
  

  w_1 <- sympt_weights[1]
  w_2 <- sympt_weights[2]
  w_3 <- sympt_weights[3]
  
  #cat("Proportion of symptomatic primary inf =", w_1, "\n")
  #cat("Proportion of symptomatic secondary inf =", w_2, "\n")
  #cat("Proportion of symptomatic tertiary inf =", w_3, "\n")
  
  if(infectiousness == 1)
  {
    # Up to 2 infections
    phi_1 <- phi_2 <- 1
    phi_3 <- phi_4 <- 0    
  } 
  
  if(infectiousness == 2)
  {
    # Up to 3 infections
    phi_1 <- phi_2 <- phi_3 <- 1
    phi_4 <- 0
  }

  if(infectiousness == 3)
  {
    # Up to 4 infections 
    phi_1 <- phi_2 <- phi_3 <- phi_4 <- 1
    
  }
  
  if(infectiousness == 4)
  {
    
    # up to 4, with symptomatic infections twice as infectious as asymptomatic ones
    phi_2 <- 1
    phi_1 <- (w_1 * 2 + (1 - w_1)) / (w_2 * 2 + (1 - w_2)) 
    phi_3 <- phi_4 <- (w_3 * 2 + (1 - w_3)) / (w_2 * 2 + (1 - w_2))
  
  }
  
  #cat("Infectiousness of primary inf =", phi_1, "\n")
  #cat("Infectiousness of secondary inf =", phi_2, "\n")
  #cat("Infectiousness of tertiary inf =", phi_3, "\n")
  #cat("Infectiousness of quaternary inf =", phi_4, "\n")
  
  vec_phis <- c(phi_1, phi_2, phi_3, phi_4)
  
  burden_measures_tags <- c(
    "number_of_infections", 
    "number_of_cases", 
    "incidence_of_infections", 
    "incidence_of_cases")
  
  interm_outs <- c("R0", "red_R0", "red_FOI")
  
  base_info <- c("OBJECTID", "ID_0", "ID_1", "population")
  
  
  # ---------------------------------------- cleaning 
    

  # get predictions
  predicted_FOI <- list_of_data[[adm]]
  
  # get character fields 
  chr_flds <- which(lapply(predicted_FOI, class) == "character")
  
  # remove character fields
  predicted_FOI_2 <- predicted_FOI[setdiff(names(predicted_FOI), names(chr_flds))]
  
  predicted_FOI_2 <- as.matrix(predicted_FOI_2)
  
  rownames(predicted_FOI_2) <- NULL
  
  
  # ---------------------------------------- run 
  
  
  out_mat <- apply(
    predicted_FOI_2, 
    1, 
    get_burden_measures_one_FOI, 
    age_struc_data, 
    age_band_lower_bounds, 
    age_band_upper_bounds,
    age_band_tags,
    vec_phis, 
    scaling_factor,
    sympt_1_infec = w_1, 
    sympt_2_infec = w_2, 
    sympt_3_infec = w_2,
    info_1 = burden_measures_tags, 
    info_2 = interm_outs, 
    info_3 = base_info)
    
  out_mat <- t(out_mat)
  
  colnames(out_mat) <- 1:dim(out_mat)[2]
  
  colnames(out_mat) <- c(base_info, interm_outs, burden_measures_tags)
    
  out_df <- as.data.frame(out_mat)
  
  
  # ---------------------------------------- save
  
  
  dir.create(my_path, FALSE, TRUE)
  
  out_file_tag <- paste0("burden_measures_exp_", 
                         exp_ID, "_run_", run_ID, ".rds")
  
  write_out_rds(dat = out_df, 
                my_path = my_path,
                file_name = out_file_tag)
  
  #out_df

}
