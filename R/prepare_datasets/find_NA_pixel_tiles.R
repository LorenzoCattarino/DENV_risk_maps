find_tiles_with_all_NA_pred_values <- function(
  i, ids_vec, in_path){
  
  #browser()
  
  one_id <- ids_vec[i]
  cat("tile id = ", one_id, "\n")
  
  file_name <- paste0("tile_", one_id, ".txt")
  
  tile <- fread(file.path(in_path, file_name),
                header = TRUE, 
                sep = ",",              
                na.strings = c("NA", "-1.#IND", "Peipsi", "Moskva", "IJsselmeer", "Zeeuwse meren"),
                fill = TRUE, 
                data.table = FALSE)  
  
  tile_dim <- dim(tile)
  
  if(tile_dim[1] == 0) {
    
    out <- TRUE
  
  } else {
    
    out<- FALSE
  
  }
  
  out

}
