find_tiles_with_all_NA_pred_values <- function(
  i, ids_vec, sel_preds){
  
  one_id <- ids_vec[i]
  cat("tile id = ", one_id, "\n")
  
  file_name <- paste0("tile_", one_id, ".txt")
  
  tile <- fread(file.path("output",
                          "env_variables",
                          "all_sets_0_1667_deg",
                          file_name))  
  
  tile_dim <- dim(tile)
  
  if(tile_dim[1] == 0) {
    
    out <- TRUE
  
  } else {
    
    out<- FALSE
  
  }
  
  out

}
