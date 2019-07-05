source(file.path("R", "burden_and_interventions", "functions_for_calculating_burden.r"))
source(file.path("R", "prepare_datasets", "functions_for_calculating_R0.r"))

foi_data <- read.csv(file.path("output", 
                               "foi", 
                               "All_FOI_estimates_and_predictors.csv"),
                     stringsAsFactors = FALSE) 

age_struct <- read.csv(file.path("output", 
                                 "datasets",
                                 "country_age_structure.csv"), 
                       header = TRUE) 

age_band_tgs <- grep("band", names(age_struct), value = TRUE)
age_band_bnds <- get_age_band_bounds(age_band_tgs)
l_lim <- age_band_bnds[, 1]
u_lim <- age_band_bnds[, 2] + 1

age_struct <- as.matrix(age_struct[,2:ncol(age_struct)])

prop_sym_parms <- c(0.45, 0.85, 0.15)
  
foi_data_2 <- foi_data[foi_data$FOI !=0, ]

R0_and_burden <- purrr::pmap(foi_data_2[, c("FOI", "population", "ID_0")], 
                             function(FOI, population, ID_0){
  
  n_j <- age_struct[which(age_struct[, "ID_0"] == ID_0), 3:ncol(age_struct)]
  
  R0_1 <- calculate_R0(FOI = FOI,
                       n_j = n_j,
                       age_band_lower_bounds = l_lim,
                       age_band_upper_bounds = u_lim,
                       weights_vec = c(1, 1, 0))
  
  R0_2 <- calculate_R0(FOI = FOI,
                       n_j = n_j,
                       age_band_lower_bounds = l_lim,
                       age_band_upper_bounds = u_lim,
                       weights_vec = c(1, 1, 1))
  
  phis_3 <- calculate_infectiousness_wgts_for_sym_asym_assumption(prop_sym_parms)
  
  R0_3 <- calculate_R0(FOI = FOI,
                       n_j = n_j,
                       age_band_lower_bounds = l_lim,
                       age_band_upper_bounds = u_lim,
                       weights_vec = phis_3)
  
  no_infections_pc <- calculate_infections(FOI = FOI, 
                                           n_j = n_j, 
                                           age_band_lower_bounds = l_lim, 
                                           age_band_upper_bounds = u_lim)
  
  no_infections <- no_infections_pc * population
  
  no_cases_pc <- calculate_cases(FOI = FOI, 
                                 n_j = n_j, 
                                 age_band_lower_bounds = l_lim, 
                                 age_band_upper_bounds = u_lim,
                                 weights_vec = prop_sym_parms)
  
  no_cases <- no_cases_pc * population
  
  no_hosp_cases_pc <- calculate_hosp_cases(FOI = FOI, 
                                           n_j = n_j, 
                                           age_band_lower_bounds = l_lim, 
                                           age_band_upper_bounds = u_lim,
                                           parms = list(Q_1 = 0.04, Q_2 = 0.1, Q_3 = 0.04),
                                           weights_vec = prop_sym_parms)
  
  no_hosp_cases <- no_hosp_cases_pc * population
  
  c(R0_1 = R0_1, 
    R0_2 = R0_2, 
    R0_3 = R0_3, 
    infections = no_infections, 
    cases = no_cases, 
    hosp = no_hosp_cases)

})

R0_and_burden_mat <- do.call("rbind", R0_and_burden)

foi_data_3 <- cbind(foi_data_2, R0_and_burden_mat)

write.table(x = foi_data_3, 
            file = file.path("output", "datasets", "R0_and_burden_calculation_test.txt"), 
            row.names = FALSE)
