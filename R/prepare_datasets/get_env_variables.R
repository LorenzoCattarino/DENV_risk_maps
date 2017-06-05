get_env_variables <- function(
  x, no_vars, pop_vars, 
  alt_vars, FT_elements, 
  FT_data, LC_vars, admin_level, my_path){
  
  result_folders <- c("altitude", "FTs", "LandCover")
    
  text_file_name_roots <- c("alt_global", "FTfreqs_2007-2014_64ppyear", "MCD12Q1.A.051.lct1_2012_5km")
    
  out_vec <- NULL
    
  admin_level_tag <- paste ("adm", admin_level, sep="")
  #cat("admin level =", admin_level_tag, "\n")    
    
  country_code <- x$ISO 
  #cat("country code =", country_code, "\n")
  
  admin_code <- x$ID_1  
  #cat("admin unit code =", admin_code, "\n")
    
  for (i in 1:length(result_folders))
  {
    result_folder <- result_folders[i]
    #cat("result folder = ", result_folder, "\n")
    
    if(result_folder == "altitude")
    {
      file_name <- sprintf ("%s_%s_%s%s", text_file_name_roots[i], admin_level_tag, country_code, ".txt")
        
      file_ex <- file.exists(file.path(my_path, admin_level_tag, result_folder, file_name))
        
      if(file_ex==TRUE)
      {
          
        txt_file <- read.table(file.path(my_path, admin_level_tag, result_folder, file_name), header=TRUE, sep="\t", quote="", stringsAsFactors = FALSE)
        
        info_alt <- as.numeric(txt_file[txt_file$adm1 == admin_code, alt_vars])
          
        if(length(info_alt)==0)
        {
          info_alt <- rep (NA, length(alt_vars))  
        }
        
      }else{
          
        info_alt <- rep (NA, length(alt_vars))
        
      }
      #cat("info alt = ", info_alt, "\n")
        
      stopifnot(length(info_alt)!=0)
        
      out_vec[1] <- info_alt
    }
    
    if(result_folder == "FTs")
    {
      start <- length(out_vec) + 1
      col_offset <- length(c(pop_vars, FT_elements))
      end <- length(out_vec) + col_offset      
      
      file_names <- sprintf (paste(text_file_name_roots[i], FT_data, "%s_%s%s", sep="_"), admin_level_tag, country_code, ".txt")  
      
      for (j in 1:length(FT_data))
      {
        #cat("start = ", start, "\n")
        #cat("end = ", end, "\n")        
        
        file_name <- file_names[j]
        
        file_ex <- file.exists(file.path(my_path, admin_level_tag, result_folder, file_name))
          
        if(file_ex==TRUE)
        {
            
          txt_file <- read.table(file.path(my_path, admin_level_tag, result_folder, file_name), header=TRUE, sep="\t", quote="")
          info_FTs <- as.numeric(txt_file[txt_file$adm1 == admin_code, c(pop_vars, FT_elements)])
            
          if(length(info_FTs)==0)
          {
            info_FTs <- rep (NA, length(c(pop_vars, FT_elements)))  
          }
          
        }else{
            
          info_FTs <- rep (NA, length(c(pop_vars, FT_elements)))
          
        }
        #cat("info FTs= ", info_FTs, "\n")
          
        stopifnot(length(info_FTs)!=0)
          
        out_vec[start:end] <- info_FTs
        
        start <- end + 1
        end <- start + col_offset - 1
      }
    }
    
    if(result_folder == "LandCover")
    {
      file_name <- sprintf ("%s_%s_%s%s", text_file_name_roots[i], admin_level_tag, country_code, ".txt")
      
      file_ex <- file.exists(file.path(my_path, admin_level_tag, result_folder, file_name))
        
      if(file_ex==TRUE)
      {
          
        txt_file <- read.table(file.path(my_path, admin_level_tag, result_folder, file_name), header=TRUE, sep="\t", quote="")
        info_LC <- as.numeric(txt_file[txt_file$adm1 == admin_code, LC_vars])
          
        if(length(info_LC)==0)
        {
          info_LC <- rep (NA, length(LC_vars))   
        }
          
      }else{
          
        info_LC <- rep (NA, length(LC_vars))
        
      }
      #cat("info LC= ", info_LC, "\n")
      
      stopifnot(length(info_LC)!=0)
        
      start <- length(out_vec) + 1
      end <- no_vars
        
      #cat("start = ", start, "\n")
      #cat("end = ", end, "\n")        
      #cat("places available = ", length(out_vec[start:end]), "\n")
      #cat("places needed = ", length(info_LC), "\n")
        
      stopifnot(length(out_vec[start:end])==length(info_LC))
        
      out_vec[start:end]<- info_LC 
    }
    #print(out_vec)    
  }
  out_vec
}

         