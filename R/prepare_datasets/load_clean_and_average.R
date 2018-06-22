load_clean_and_average <- function(x, grp_flds, grid_size, env_var_names, out_path){
  
  # browser()
  
  tile <- fread(x,
                header = TRUE, 
                sep = ",",              
                na.strings = c("NA", "-1.#IND", "Peipsi", "Moskva", "IJsselmeer", "Zeeuwse meren"),
                fill = TRUE, 
                data.table = FALSE)
  
  if(is.character(tile$ID_0)) stop("ID_0 is a character")
  
  cc <- clean_and_average(tile, env_var_names, grid_size, grp_flds)
  
  names(cc)[names(cc) == "lat.grid"] <- "latitude"
  
  names(cc)[names(cc) == "long.grid"] <- "longitude"
  
  file_name <- basename(x) 
  
  dir.create(out_path, FALSE, TRUE)
  
  write.table(cc, 
              file.path(out_path, file_name),
              row.names = FALSE,
              sep = ",")
  
}
