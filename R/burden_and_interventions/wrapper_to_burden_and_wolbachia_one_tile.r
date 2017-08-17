burden_and_wolbachia_one_tile <- function(
  i, ids_vec, in_path,
  orig_in_path, xx, age_band_tgs,
  age_band_lower_bounds, age_band_upper_bounds,
  sympt_weights, age_struct){
  
  
  browser()
  
  one_id <- ids_vec[i]
  cat("tile id = ", one_id, "\n")
  
  tile_nm <- paste0("tile_", one_id)
  
  file_name <- paste0(tile_nm, ".txt")
  
  original_tile <- fread(file.path(orig_in_path, file_name),
                         header = TRUE,
                         sep = ",",
                         na.strings = c("NA", "-1.#IND", "Peipsi", "Moskva", "IJsselmeer", "Zeeuwse meren"),
                         fill = TRUE,
                         data.table = FALSE)
  
  
  # ---------------------------------------- create look up table for age structure
  
  
  look_up <- inner_join(
    age_struct,
    original_tile[, c("cell", "ADM_0")], 
    by = c("ID_0" = "ADM_0"))
  
  
  # ---------------------------------------- load foi predictions for all fits 
  
  
  tile_foi <- readRDS(file.path(in_path, tile_nm, "foi.rds"))
    
  
  # ---------------------------------------- calculates R0 for each foi value
  
  
  R_0_values <- loop(
    xx,
    burden_multi_factor_wrapper,
    predicted_FOI = tile_foi, 
    age_band_tags = age_band_tgs,
    age_band_lower_bounds = age_band_lower_bounds,
    age_band_upper_bounds = age_band_upper_bounds,
    sympt_weights = sympt_weights,
    look_up = look_up,
    parallel = TRUE)
  
}  
