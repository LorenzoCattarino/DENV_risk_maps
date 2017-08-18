wrapper_to_mean_across_fits <- function(
  i, ids_vec, in_path, orig_in_path,
  var_names, parallel, base_info,
  out_path){
  
  
  #browser()
  
  # ----------------------------------------
  
  
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
  
  a <- file.path(in_path, tile_nm)
  
  ret <- loop(seq_along(var_names), 
              mean_across_fits, 
              var_names = var_names, 
              fl_pth = a, 
              parallel = parallel)
  
  all_averaged_vars <- do.call("cbind", ret)
  
  out <- cbind(original_tile[, base_info], all_averaged_vars)
  
  zero_logic <- out$foi_mean == 0
  
  out_mz <- out[!zero_logic, ] 
  
  dir.create(out_path, FALSE, TRUE)
  
  write.table(out_mz,
              file.path(out_path, file_name),
              row.names = FALSE,
              sep = ",")
              
}
