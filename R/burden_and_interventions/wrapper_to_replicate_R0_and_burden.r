wrapper_to_replicate_R0_and_burden <- function(i,
                                               foi_data, 
                                               age_struct,
                                               scaling_factor,
                                               FOI_to_R0_list,
                                               FOI_to_Inf_list,
                                               FOI_to_C_list,
                                               FOI_to_HC_list,
                                               age_band_lower_bounds, 
                                               age_band_upper_bounds, 
                                               age_band_tags,
                                               vec_phis, 
                                               prob_fun, 
                                               no_fits, 
                                               var_to_fit, 
                                               fit_type,
                                               vars){
  
  
  #browser()
  
  
  # ---------------------------------------- get the right look up table for each square 
  
  
  idx <- foi_data[i, "age_id"]
    
  FOI_to_R0 <- FOI_to_R0_list[[idx]]
  FOI_to_Inf <- FOI_to_Inf_list[[idx]]
  FOI_to_C <- FOI_to_C_list[[idx]]
  FOI_to_HC <- FOI_to_HC_list[[idx]] 
  
  
  # ---------------------------------------- get vector of foi values and number of people in each square
  
  
  if(!is.null(no_fits)){
    col_ids <- as.character(seq_len(no_fits))
  }
  
  m_j <- age_struct[age_struct$ADM_0 == foi_data[i, "ID_0"], age_band_tags]
  
  if(fit_type == "boot") {
    
    preds <- foi_data[i, col_ids]
    
  } else{
    
    preds <- foi_data[i, "best"]
  }
  
  N <- foi_data[i, "population"]
  
  
  # ---------------------------------------- calculates R0 values for different replicates of the same pixel  
  

  red_preds <- preds * scaling_factor
  red_trans <- approx(FOI_to_R0[, "y"], FOI_to_R0[, "x"], xout = red_preds)$y
  Infections_pc <- approx(FOI_to_Inf[, "x"], FOI_to_Inf[, "y"], xout = red_trans)$y 
  Cases_pc <- approx(FOI_to_C[, "x"], FOI_to_C[, "y"], xout = red_trans)$y
  Hosp_cases_pc <- approx(FOI_to_HC[, "x"], FOI_to_HC[, "y"], xout = red_trans)$y
  Infections <- Infections_pc * N
  Cases <- Cases_pc * N
  HCases <- Hosp_cases_pc * N 
  #Infections_inc <- Infections_pc * 1000
  #Cases_inc <- Cases_pc * 1000 
  out <- rbind(red_preds, red_trans, Infections, Cases, HCases)
  rownames(out) <- vars
  out
  
  
  # # n <- length(vars)
  # 
  # if(fit_type == "boot") {
  #   
  #   red_preds <- preds * scaling_factor
  #   red_trans <- approx(FOI_to_R0[, "y"], FOI_to_R0[, "x"], xout = red_preds)$y
  #   Infections_pc <- approx(FOI_to_Inf[, "x"], FOI_to_Inf[, "y"], xout = red_trans)$y 
  #   Cases_pc <- approx(FOI_to_C[, "x"], FOI_to_C[, "y"], xout = red_trans)$y
  #   Infections <- Infections_pc * N
  #   Cases <- Cases_pc * N
  #   Infections_inc <- Infections_pc * 1000
  #   Cases_inc <- Cases_pc * 1000 
  #   out <- rbind(red_preds, red_trans, Infections, Cases, Infections_inc, Cases_inc)
  #   rownames(out) <- vars
  #   out
  #   # vapply(FOI_values,
  #   #        wrapper_to_R0_and_burden,
  #   #        numeric(n),
  #   #        n_j = m_j,
  #   #        vars = vars,
  #   #        age_band_lower_bounds = age_band_lower_bounds, 
  #   #        age_band_upper_bounds = age_band_upper_bounds,
  #   #        vec_phis = vec_phis, 
  #   #        prob_fun = prob_fun,
  #   #        scaling_factor = scaling_factor,
  #   #        FOI_to_R0 = FOI_to_R0, 
  #   #        FOI_to_Inf = FOI_to_Inf, 
  #   #        FOI_to_C = FOI_to_C,
  #   #        var_to_fit = var_to_fit)
  #   
  # } else {
  #   
  #   wrapper_to_R0_and_burden(FOI_values,
  #                            n_j = m_j,
  #                            vars = vars,
  #                            age_band_lower_bounds = age_band_lower_bounds,
  #                            age_band_upper_bounds = age_band_upper_bounds,
  #                            vec_phis = vec_phis,
  #                            prob_fun = prob_fun,
  #                            scaling_factor = scaling_factor,
  #                            FOI_to_R0 = FOI_to_R0,
  #                            FOI_to_Inf = FOI_to_Inf,
  #                            FOI_to_C = FOI_to_C,
  #                            var_to_fit = var_to_fit)
  # }

}
