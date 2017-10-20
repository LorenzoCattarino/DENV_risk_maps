average_foi_and_burden_predictions <- function(
  j, vars, in_path, 
  out_path, no_scenarios, col_names,
  base_info){
  
  my_var <- vars[j]
  
  if(j == 1) {
    
    root_name <- paste0(my_var, "_all_squares_0_1667_deg")
    
    dat <- readRDS(file.path(in_path, paste0(root_name, ".rds")))
    
    ret <- average_boot_samples_dim2(dat)
    
    out_name <- paste0(my_var, "_mean_all_squares_0_1667_deg.rds")
    
    write_out_rds(ret, out_path, out_name)
    
  } else {
  
    loop(seq_len(no_scenarios),
         calculate_mean_of_burden_predictions_for_different_scenarios,
         in_path = in_path, 
         my_var = my_var,
         col_names = col_names, 
         base_info = base_info, 
         out_path = out_path,
         parallel = TRUE)
    
  }

}

calculate_mean_of_burden_predictions_for_different_scenarios <- function(
  i, in_path, my_var,
  col_names, base_info, out_path){
  
  root_name <- paste0(my_var, "_all_squares_0_1667_deg")
  
  dat <- readRDS(file.path(in_path, sprintf("%s_%s%s", root_name, i, ".rds")))
  
  ret <- average_boot_samples_dim2(dat[, col_names])
  
  out_name <- sprintf("%s_%s_%s%s", my_var, "mean_all_squares_0_1667_deg", i, ".rds")
  
  ret2 <- cbind(dat[, base_info], ret) 
  
  write_out_rds(ret2, out_path, out_name)
  
}
