wrapper_to_load_tile_dataset <- function(
  i, ids_vec, sel_preds, 
  model_in_path, out_path, 
  var_names, base_info, parallel, 
  no_fits, average, model_type){
  
  #browser()
  
  in_path <- file.path("output", "env_variables", "all_sets_0_1667_deg")
  
  one_id <- ids_vec[i]
  cat("tile id = ", one_id, "\n")

  file_name <- paste0("tile_", one_id, ".txt")

  tile <- fread(file.path(in_path, file_name),
                header = TRUE,
                sep = ",",
                na.strings = c("NA", "-1.#IND", "Peipsi", "Moskva", "IJsselmeer", "Zeeuwse meren"),
                fill = TRUE,
                data.table = FALSE)
  
  h2o.init()
  
  out <- wrapper_to_make_preds(
    dataset = tile, 
    predictors = sel_preds, 
    model_in_path = model_in_path,
    parallel = parallel,
    base_info = base_info, 
    var_names = var_names,
    no_fits = no_fits,
    average = average,
    model_type = model_type)  
  
  dir.create(out_path, FALSE, TRUE)
  
  write.table(out, 
              file.path(out_path, file_name),
              row.names = FALSE,
              sep = ",")
  
  h2o.shutdown(prompt = FALSE)
  
}  
