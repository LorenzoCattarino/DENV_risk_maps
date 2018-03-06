filter_resample_and_combine <- function(i, 
                                        boot_samples, 
                                        tile_ls, 
                                        grp_flds, 
                                        new_res, 
                                        predictors, 
                                        out_file_path, 
                                        out_file_name,
                                        parallel_2){
  
  #browser()
  
  foi_data <- boot_samples[[i]]
  
  names(foi_data)[names(foi_data) == "population"] <- "adm_pop"
  
  pxl_job <- loop(
    tile_ls,
    filter_and_resample,
    foi_dts = foi_data, 
    env_var_names = predictors, 
    grp_flds = grp_flds, 
    grid_size = new_res,
    parallel = parallel_2)
  
  pxl_dataset <- do.call("rbind", pxl_job)
  
  pxl_dataset$cell <- seq_len(nrow(pxl_dataset))
  
  names(pxl_dataset)[names(pxl_dataset) == "cell"] <- "square"
  
  a <- out_file_name[i]
  
  write_out_rds(pxl_dataset, out_file_path, a)
  
}
