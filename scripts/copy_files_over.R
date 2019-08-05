# copy over files from shared drive

root <- file.path("Y:", "DENV_risk_maps")

paths_from <- list(
  file.path("output",
            "predictions_world",
            "bootstrap_models",
            "model_4",
            "adm_1",
            "p9_mean.rds"),
  file.path("output",
            "predictions_world",
            "bootstrap_models",
            "model_4",
            "adm_1",
            "p16_mean.rds"),
  file.path("output",
            "predictions_world",
            "bootstrap_models",
            "model_4",
            "adm_1",
            "response_endemic_mean.rds"),
  file.path("output",
            "predictions_world",
            "bootstrap_models",
            "model_4",
            "adm_1",
            "response_endemic.rds"),
  file.path("output",
            "shapefiles",
            "gadm28_adm1_eras.shp"),
  file.path("output", 
            "predictions_world", 
            "FOI_to_I_lookup_tables.rds"),
  file.path("output", 
            "predictions_world", 
            "FOI_to_C_lookup_tables.rds"),
  file.path("output", 
            "predictions_world", 
            "FOI_to_HC_lookup_tables.rds"),
  file.path("output", 
            "predictions_world", 
            "FOI_to_R0_1_lookup_tables.rds"),
  file.path("output", 
            "predictions_world", 
            "FOI_to_R0_2_lookup_tables.rds"),
  file.path("output", 
            "predictions_world", 
            "FOI_to_R0_3_lookup_tables.rds"),
  file.path("output", 
            "predictions_world", 
            "FOI_to_C_lookup_tables_fixed_params.rds"),
  file.path("output", 
            "predictions_world", 
            "FOI_to_HC_lookup_tables_fixed_params.rds"))

for (i in seq_along(paths_from)){
  
  path_i <- paths_from[[i]]
  
  message(path_i)
  
  path_from_i <- file.path(root, path_i) 
  
  # match everything before the last occurrence of /
  path_to_i <- sub("/([^/]*)$", "", path_i)
  
  if(!dir.exists(path_to_i)) dir.create(path_to_i, FALSE, TRUE)
  
  file.copy(path_from_i, path_to_i, overwrite = FALSE)

  last_three_digits <- sub("^([^.]*).", "", path_i)
  
  if(last_three_digits == "shp"){
    
    evr_but_last_three_digits <- sub(".([^.]*)$", "", path_i)
    
    path_from_i_1 <- file.path(root, paste0(evr_but_last_three_digits, ".dbf"))  
    file.copy(path_from_i_1, path_to_i, overwrite = FALSE)
    
    path_from_i_2 <- file.path(root, paste0(evr_but_last_three_digits, ".prj"))  
    file.copy(path_from_i_2, path_to_i, overwrite = FALSE)
    
    path_from_i_3 <- file.path(root, paste0(evr_but_last_three_digits, ".shx"))  
    file.copy(path_from_i_3, path_to_i, overwrite = FALSE)
    
  }
  
}
