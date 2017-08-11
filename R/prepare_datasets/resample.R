resample <- function(x, grp_flds, grid_size, env_var_names, out_path){
  
  #browser()
  xx <- fread(x,
              header = TRUE, 
              sep = ",",              
              na.strings = c("NA", "-1.#IND", "Peipsi", "Moskva", "IJsselmeer", "Zeeuwse meren"),
              fill = TRUE, 
              data.table = FALSE)
  
  if(is.character(xx$ADM_0)) stop("ADM_0 is a character")
  
  yy <- grid_up(
    dataset = xx, 
    grid_size = grid_size, 
    rnd_dist = FALSE)
  
  bb <- yy[!is.na(yy$population), ]
    
  bb[bb$population == 0, "population"] <- 1
  
  cc <- average_up(
    pxl_df = bb, 
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
