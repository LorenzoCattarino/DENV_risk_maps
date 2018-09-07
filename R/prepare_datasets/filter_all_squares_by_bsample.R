filter_all_squares_by_bsample <- function(i, 
                                          boot_samples, 
                                          all_squares,
                                          jn_flds,
                                          out_file_path, 
                                          out_file_name){
  
  # browser()
  
  foi_data <- boot_samples[[i]]
  
  pxl_dataset <- inner_join(all_squares, foi_data[, jn_flds])
  
  a <- out_file_name[i]
  
  write_out_rds(pxl_dataset, out_file_path, a)
  
}
