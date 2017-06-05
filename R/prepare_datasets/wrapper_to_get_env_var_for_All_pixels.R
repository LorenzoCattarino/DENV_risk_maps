wrapper_to_get_env_var_for_pixels <- function (x, a, b, c, d, e, pop_raster){
  
  tile_id <- x$set_id
  cat("tile id =", tile_id, "\n")
  
  # 1) load all env variables for the tile
  all_env_var <- load_env_var(tile_id, a, b, c)
  
  # 2) assign NA to all the mode terms of the records with too small const_term value, for each FT variable
  all_env_var_2 <- lapply(seq_along(all_env_var), assign_NA, all_env_var)
  
  # 3) cbind all env variable to each other (also subset and change column names)
  all_env_var_combined <- append_env_var_by_tile(all_env_var_2, d, e)
  
  #### add population
  
  # find the row and col number of the Landscan pixel where Tini grid cells' lat and long fall
  T_lat <- all_env_var_combined$latitude  
  T_lon <- all_env_var_combined$longitude
  
  L_row = floor ((90.0 - T_lat) * 120.0)
  L_col = floor (120.0 * (180.0 + T_lon))
  
  cell_numbers <- cellFromRowCol(pop_raster, L_row, L_col)
  pop_values <- pop_raster[cell_numbers] 
  
  all_env_var_combined$population <- pop_values
  
  ####
  
  # create output dir 
  dir.create(file.path(a, "tiles"), FALSE, TRUE)
  
  # get name of output file 
  file_name <- paste0("tile_", tile_id, ".txt")
  
  # write out
  write.table(all_env_var_combined, 
              file.path(a, "tiles", file_name), 
              row.names = FALSE, sep = ",")
  
}
