# load Altitude, all fourier elements for DayTemp, EVI, MIR, NightTemp and RFE, 
# for ONE country,
# as elements of a list

load_env_var <- function(set_id, my_path, FTs_data, txt_file_roots) {
  
  result_folders <- c("altitude", "FTs", "LandCover")

  data_list <- vector("list", length = length(result_folders) + length(FTs_data) - 1)
  
  for (i in 1:length(result_folders))
  {
    
    result_folder <- result_folders[i]
    #cat("result folder = ", result_folder, "\n")
    
    if(result_folder == "altitude") {
      
      root_file_name <- sprintf("%s%s%s", txt_file_roots[i], set_id, ".txt")
      #cat("root file = ", root_file_name, "\n") 
      
      file_ex <- file.exists(file.path(my_path, result_folder, root_file_name))
      
      if(file_ex == TRUE)
      {
        
        txt_file <- fread(file.path(my_path, result_folder, root_file_name), 
                          header = TRUE, 
                          sep = "\t", 
                          na.strings = c("NA", "-1.#IND"), 
                          quote = "", 
                          fill = TRUE, 
                          data.table = FALSE)
        
      }else{
        
        txt_file <- NA 
        
      }
      
      data_list[[1]] <- txt_file
      
    }
    
    if(result_folder == "FTs") {
      
      file_names <- sprintf(paste(txt_file_roots[i], FTs_data, "%s%s", sep="_"), 
                            set_id, ".txt")  
      
      for (j in 1:length(file_names))
      {
        
        root_file_name <- file_names[j] 
        #cat("root file = ", root_file_name, "\n")      
        
        file_ex <- file.exists(file.path(my_path, result_folder, root_file_name))
        
        if(file_ex == TRUE)
        {
          
          txt_file <- fread(file.path(my_path, result_folder, root_file_name), 
                            header = TRUE, 
                            sep = "\t", 
                            na.strings = c("NA", "-1.#IND"),
                            quote = "", 
                            fill = TRUE,
                            data.table = FALSE)
          
        }else{
          
          txt_file <- NA
          
        }
        
        data_list[[1 + j]] <- txt_file
        
      }
      
    }     
    
    if(result_folder == "LandCover") {
      
      root_file_name <- sprintf("%s%s%s", txt_file_roots[i], set_id, ".txt")
      #cat("root file = ", root_file_name, "\n") 
      
      file_ex <- file.exists(file.path(my_path, result_folder, root_file_name))
      
      if(file_ex == TRUE)
      {
        
        txt_file <- fread(file.path(my_path, result_folder, root_file_name), 
                          header = TRUE, 
                          sep = "\t", 
                          na.strings = c("NA", "-1.#IND"),
                          quote = "", 
                          fill = TRUE,
                          data.table = FALSE)
        
      }else{
        
        txt_file <- NA
        
      }
      
      data_list[[1 + j + 1]] <- txt_file
      
    }
    
  }
  
  data_list
  
}
