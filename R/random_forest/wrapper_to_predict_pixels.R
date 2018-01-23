wrapper_to_predict_pixels <- function(i, 
                                      ids_vec, 
                                      in_path, 
                                      no_fits, 
                                      RF_mod_name, 
                                      model_in_path, 
                                      predictors, 
                                      fctr_combs,
                                      age_struct, 
                                      age_band_tgs, 
                                      age_band_lower_bounds,
                                      age_band_upper_bounds,
                                      var_names,
                                      FOI_values, 
                                      FOI_to_Inf_list,
                                      FOI_to_C_list,
                                      prob_fun,
                                      var_to_fit,
                                      fit_type,
                                      base_info,
                                      lookup_path,
                                      out_path,
                                      var_to_average,
                                      scenario_ids,
                                      dts_tag) {
  
  # browser()
  
  one_id <- ids_vec[i]
  cat("tile id =", one_id, "\n")

  tile_nm <- paste0("tile_", one_id)
         
  tile_out_path <- file.path(out_path, tile_nm)
  
  file_name <- paste0(tile_nm, ".txt")

  tile <- fread(file.path(in_path, file_name),
                header = TRUE,
                sep = ",",
                na.strings = c("NA", "-1.#IND", "Peipsi", "Moskva", "IJsselmeer", "Zeeuwse meren"),
                fill = TRUE,
                data.table = FALSE)
  
  h2o.init()
  
  foi <- vapply(seq_len(no_fits),
                wrapper_to_make_h2o_preds,
                numeric(nrow(tile)),
                RF_mod_name = RF_mod_name,
                model_in_path = model_in_path, 
                dataset = tile, 
                predictors = predictors,
                start_h2o = FALSE,
                shut_h2o = FALSE)
  
  h2o.shutdown(prompt = FALSE)
  
  write_out_rds(foi, tile_out_path, "FOI_all_squares.rds") 
  
  foi <- cbind(tile[, base_info], foi)
  
  foi <- inner_join(
    age_struct[, c("age_id", "ADM_0")],
    foi, 
    by = "ADM_0")
  
  foi <- as.matrix(foi)
  
  R0_and_burden <- loop(
    fctr_combs[scenario_ids],
    wrapper_to_multi_factor_R0_and_burden,
    foi_data = foi, 
    age_data = age_struct,
    age_band_tags = age_band_tgs,
    age_band_lower_bounds = age_band_lower_bounds,
    age_band_upper_bounds = age_band_upper_bounds,
    parallel_2 = TRUE,    
    var_names = var_names, 
    FOI_values = FOI_values,
    FOI_to_Inf_list = FOI_to_Inf_list,
    FOI_to_C_list = FOI_to_C_list,
    prob_fun = prob_fun,
    var_to_fit = var_to_fit,
    fit_type = fit_type,
    base_info = base_info,
    lookup_path = lookup_path,
    out_path = tile_out_path,
    no_fits = no_fits,
    parallel = FALSE)
  
  col_names <- as.character(seq_len(no_fits))
  
  means_all_scenarios <- loop(
    seq_along(var_to_average),
    average_foi_and_burden_predictions,
    vars = var_to_average,
    in_path = tile_out_path,
    out_path = tile_out_path,
    scenario_ids = scenario_ids,
    col_names = col_names,
    base_info = base_info,
    dts_tag = dts_tag,
    covariate_dts = tile, 
    parallel = FALSE)

}  
