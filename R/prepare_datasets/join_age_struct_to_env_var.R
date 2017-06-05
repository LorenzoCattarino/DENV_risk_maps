join_age_struc <- function(age_struct, env_var) {
  
  # Identify columns with age bands 
  numeric_columns <- grep("band", names(age_struct), value = TRUE)
  
  # Attach age structure data using the country code field
  env_var_plus_and_age <- merge(env_var, 
                                age_struct[, c("ISO_code", numeric_columns)], 
                                by.x = "country_code", 
                                by.y = "ISO_code", 
                                all.x = TRUE)
  
  env_var_plus_and_age <- env_var_plus_and_age[setdiff(names(env_var_plus_and_age), c("name_1", "name_2"))]
  
  env_var_plus_and_age
  
}
  