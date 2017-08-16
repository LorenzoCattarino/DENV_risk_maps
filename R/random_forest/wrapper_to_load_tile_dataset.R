wrapper_to_load_tile_dataset <- function(
  i, ids_vec, in_path, 
  no_fits, model_in_path, predictors, 
  parallel, out_path, out_name){
  
  #browser()
  
  one_id <- ids_vec[i]
  cat("tile id = ", one_id, "\n")

  tile_nm <- paste0("tile_", one_id)
         
  file_name <- paste0(tile_nm, ".txt")

  tile <- fread(file.path(in_path, file_name),
                header = TRUE,
                sep = ",",
                na.strings = c("NA", "-1.#IND", "Peipsi", "Moskva", "IJsselmeer", "Zeeuwse meren"),
                fill = TRUE,
                data.table = FALSE)
  
  foi <- wrapper_to_make_preds(
    no_fits = no_fits,
    model_in_path = model_in_path, 
    dataset = tile, 
    predictors = predictors, 
    parallel = parallel)
    
  out_pt_one_tile <- out_path[i]  

  out_nm <- paste0(out_name, ".rds")
  
  write_out_rds(foi, out_pt_one_tile, out_nm)
              
}  
