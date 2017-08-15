wrapper_to_load_tile_dataset <- function(
  i, ids_vec, in_path, 
  predictors, model_in_path, parallel,
  base_info, var_names, no_fits,
  average, model_type, out_path){
  
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
  
  out <- wrapper_to_make_preds(tile, 
                               predictors, 
                               model_in_path,
                               parallel, 
                               base_info, 
                               var_names, 
                               no_fits, 
                               average, 
                               model_type)
  
  dir.create(out_path, FALSE, TRUE)
  
  write.table(out, 
              file.path(out_path, file_name),
              row.names = FALSE,
              sep = ",")
  
}  
