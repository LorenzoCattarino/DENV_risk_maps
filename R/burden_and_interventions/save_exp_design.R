save_exp_design <- function(exp_ID, factor_combs, output_folder) {
  
  # Create name of file with experimental design 
  factor_combinations_file_name <- sprintf("factor_combinations_%s%s", 
                                           paste("exp", exp_ID, sep = "_"), ".csv")
  
  # Create output directory 
  dir.create(file.path("output", "dengue_dataset", output_folder, 
                       paste("exp", exp_ID, sep = "_")),
             FALSE, TRUE)
  
  # Write out the experimental design
  write.table(factor_combs, 
              file.path("output", "dengue_dataset", output_folder, 
                        paste("exp", exp_ID, sep = "_"),
                        factor_combinations_file_name), 
              row.names = FALSE, sep = ",")
}
