wrapper_to_load_tile_dataset <- function(
  i, ids_vec, in_path, 
  no_fits, RF_mod_name, model_in_path, 
  predictors, age_struct, 
  var_names, fctr_combs, age_band_tgs, 
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
  
  foi <- vapply(seq_len(no_fits),
                wrapper_to_make_h2o_preds,
                numeric(no_fits),
                RF_mod_name = RF_mod_name,
                model_in_path = model_in_path, 
                dataset = tile, 
                predictors = predictors)
  
  foi <- cbind(tile[, base_info], foi)
  
  foi <- inner_join(
    age_struct[, c("age_id", "ADM_0")],
    foi, 
    by = "ADM_0")
  
  foi <- as.matrix(foi)
  
  R0_and_burden <- loop(
    fctr_combs,
    wrapper_to_multi_factor_R0_and_burden,
    foi_data = foi, 
    age_data = age_struct,
    age_band_tags = age_band_tgs,
    age_band_lower_bounds = age_band_L_bounds,
    age_band_upper_bounds = age_band_U_bounds,
    parallel_2 = TRUE,    
    var_names = var_names, 
    FOI_values = FOI_values,
    FOI_to_Inf_list = FOI_to_Inf_list,
    FOI_to_C_list = FOI_to_C_list,
    prob_fun = prob_fun,
    var_to_fit = var_to_fit,
    fit_type = fit_type,
    base_info = base_info,
    no_fits = no_fits,
    parallel = FALSE)
  
  means_all_scenarios <- loop(
    seq_along(vars)[2],
    average_foi_and_burden_predictions,
    vars = vars,
    in_path = in_path,
    out_path = out_path,
    scenario_ids = scenario_ids,
    col_names = col_names,
    base_info = base_info,
    dts_tag = dts_tag,
    parallel = TRUE)

}  
