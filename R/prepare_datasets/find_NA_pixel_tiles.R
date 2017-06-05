find_tiles_with_all_NA_pred_values <- function(
  i, ids_vec, sel_preds){
  
  one_id <- ids_vec[i]
  cat("tile id = ", one_id, "\n")
  
  file_name <- paste0("tile_", one_id, ".txt")
  
  tile <- fread(file.path("data", 
                          "covariates",
                          file_name))  
  
  # Remove records with at least one NA predictor value
  tile_2 <- remove_NA_rows(tile, sel_preds)
  
  tile_dim <- dim(tile_2)
  
  if(tile_dim[1] == 0){
    
    out <- TRUE
  
  }else{
    
    out<- FALSE
  }
  
  out

}
