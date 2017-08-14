save_summary_by_country <- function (i, xx, yy, output_folder) {
  
  exp_ID <- xx[i, "ID.exp"]
  #cat("exp ID =", exp_ID, "\n")
  
  run_ID <- xx[i, "ID.run"]
  #cat("run ID =", run_ID, "\n")
  
  out_by_country <- yy[[i]]
    
  # Create name of file with burden measures summarized by country
  b_measures_file_name <- sprintf("burden_summary_by_country_%s_%s%s", 
                                  paste("exp", exp_ID, sep = "_"),
                                  paste("run", run_ID, sep = "_"), ".csv")
  
  # Create output directory 
  dir.create(file.path("output", "dengue_dataset", output_folder, 
                       paste("exp", exp_ID, sep = "_")),
             FALSE, TRUE)
  
  # Write out the summary of burden of measures
  write.table(out_by_country, 
              file.path("output", "dengue_dataset", output_folder, 
                        paste("exp", exp_ID, sep = "_"),
                        b_measures_file_name), 
              row.names = FALSE, sep = ",")

}
