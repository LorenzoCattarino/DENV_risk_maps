resample <- function(x, grp_flds, grid_size, env_var_names, out_path){
  
  #browser()
  
  tile <- fread(x,
                header = TRUE, 
                sep = ",",              
                na.strings = c("NA", "-1.#IND", "Peipsi", "Moskva", "IJsselmeer", "Zeeuwse meren"),
                fill = TRUE, 
                data.table = FALSE)
  
  if(is.character(tile$ADM_0)) stop("ADM_0 is a character")
  
  xx <- remove_NA_rows(tile, env_var_names)
  
  bb <- xx[!is.na(xx$population), ]

  if (nrow(bb) > 0) {
    
    bb[bb$population == 0, "population"] <- 1
  
  }
  
  yy <- grid_up(
    dataset = bb, 
    grid_size = grid_size, 
    rnd_dist = FALSE)
  
  cc <- average_up(
    pxl_df = yy, 
    grp_flds = grp_flds, 
    var_names = env_var_names)
  
  cc$lat.grid <- cc$lat.grid * grid_size
  
  cc$long.grid <- cc$long.grid * grid_size
  
  file_name <- basename(x) 
  
  dir.create(out_path, FALSE, TRUE)
  
  write.table(cc, 
              file.path(out_path, file_name),
              row.names = FALSE,
              sep = ",")
  
}
