filter_all_squares_by_bsample <- function(i, 
                                          boot_samples, 
                                          all_squares,
                                          jn_flds,
                                          out_file_path, 
                                          out_file_name){
  
  # browser()
  
  foi_data <- boot_samples[[i]]
  
  pxl_dataset <- inner_join(all_squares, foi_data[, jn_flds])
  
  # names(foi_data)[names(foi_data) == "population"] <- "adm_pop"
  #
  # pxl_job <- loop(
  #   tile_ls,
  #   filter_and_resample,
  #   foi_dts = foi_data, 
  #   env_var_names = predictors, 
  #   grp_flds = grp_flds, 
  #   jn_flds = jn_flds,
  #   grid_size = new_res,
  #   resample = resample,
  #   parallel = parallel_2)
  # 
  # pxl_dataset <- do.call("rbind", pxl_job)
  # 
  # pxl_dataset$cell <- seq_len(nrow(pxl_dataset))
  # 
  # pxl_dataset$log_pop_den <- log(1 + pxl_dataset$pop_den) 
  # 
  # names(pxl_dataset)[names(pxl_dataset) == "cell"] <- "square"
  
  a <- out_file_name[i]
  
  write_out_rds(pxl_dataset, out_file_path, a)
  
}
