wrapper_to_load_tile_dataset <- function(
  i, ids_vec, in_path, 
  no_fits, model_in_path, predictors, 
  burden, age_struct, var_names,
  fctr_combs, age_band_tgs, 
  age_band_lower_bounds, age_band_upper_bounds,
  w_1, w_2, w_3, base_info,
  out_path){
  
  #browser()
  
  one_id <- ids_vec[i]
  cat("tile id =", one_id, "\n")

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
    parallel = FALSE)
    
  foi[foi < 0] <- 0
  
  look_up <- inner_join(
    age_struct,
    tile[, c("cell", "ADM_0")], 
    by = c("ID_0" = "ADM_0"))
  
  browser()
  
  if(burden & nrow(look_up)) {
    
    R0_and_burden <- loop(
      fctr_combs,
      burden_multi_factor_wrapper,
      foi_data = foi, 
      orig_data = tile,
      age_band_tags = age_band_tgs,
      age_band_lower_bounds = age_band_lower_bounds,
      age_band_upper_bounds = age_band_upper_bounds,
      w_1 = w_1, 
      w_2 = w_2, 
      w_3 = w_3,
      look_up = look_up,
      var_names = var_names,
      parallel = TRUE)
    
    out_ls <- c(list(foi), unlist(R0_and_burden, recursive = FALSE))
    
    no_runs <- length(fctr_combs)
    burden_tags <- vapply(var_names[2:length(var_names)], paste, character(4), 1:no_runs, sep = "_")
    burden_tags <- as.vector(burden_tags)
  
  } else {
    
    out_ls <- list(foi)
    
    burden_tags <- NULL
  
  }
  
  exp_v_nms <- c(var_names[1], burden_tags)  
  
  ret <- loop(
    seq_along(exp_v_nms),
    wrapper_to_mean_across_fits,
    exp_v_nms,
    out_ls,
    parallel = TRUE)
  
  all_averaged_vars <- do.call("cbind", ret)
  
  out <- cbind(tile[, base_info], all_averaged_vars)
  
  zero_logic <- out$foi_mean == 0
  
  out_mz <- out[!zero_logic, ] 
  
  dir.create(out_path, FALSE, TRUE)
  
  write.table(out_mz,
              file.path(out_path, file_name),
              row.names = FALSE,
              sep = ",")
  
}  
