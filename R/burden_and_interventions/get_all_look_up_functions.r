get_all_look_up_functions <- function(
  i, look_up, age_band_tags, 
  vec_phis, prob_fun, max_FOI, 
  age_band_lower_bounds, age_band_upper_bounds) {
  
  # number of people in each age group of the country 
  n_j <- look_up[i, age_band_tags]
  
  # total population in country 
  N <- look_up[i, "N"]
  
  approximate(
    target = calculate_R0,
    a = 0,
    b = max_FOI,
    N = N, 
    n_j = n_j, 
    age_band_lower_bounds = age_band_lower_bounds, 
    age_band_upper_bounds = age_band_upper_bounds,
    vec_phis = vec_phis, 
    prob_fun = prob_fun,
    tol = 1e-5,
    target_vectorised = FALSE,
    inverse = TRUE)
}
