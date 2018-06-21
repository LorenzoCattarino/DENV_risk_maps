wrapper_to_get_env_var_for_pixels <- function(x, 
                                              in_path,
                                              out_path,
                                              FTs_data, 
                                              txt_file_roots, 
                                              all_vars, 
                                              all_var_names, 
                                              raster_1,
                                              raster_2,
                                              raster_3,
                                              raster_4){
  
  
  get_raster_values_from_xy <- function(i, lat, lon) {
    
    my_raster <- paste0("raster_", i)
    ras_obj <- get(my_raster)
    ext_coord <- cbind(lon, lat)
    cell_numbers <- cellFromXY(ras_obj, ext_coord)
    ras_obj[cell_numbers] 
    
  }
  
  tile_id <- x$set_id
  cat("tile id =", tile_id, "\n")
  
  file_name <- paste0("tile_", tile_id, ".txt")
  
  # 1) load all env variables for the tile
  all_env_var <- load_env_var(set_id = tile_id, 
                              my_path = in_path, 
                              FTs_data = FTs_data, 
                              txt_file_roots = txt_file_roots)
  
  # 2) assign NA to all the mode terms of the records with too small const_term value, for each FT variable
  all_env_var_2 <- lapply(seq_along(all_env_var), assign_NA, all_env_var)
  
  # 3) cbind all env variable to each other (also subset and change column names)
  all_env_var_combined <- append_env_var_by_tile(all_env_var_2, all_vars, all_var_names)
  
  
  #### add population
  
  # find the row and col number of the Landscan pixel where Tini grid cells' lat and long fall
  T_lat <- all_env_var_combined$latitude  
  T_lon <- all_env_var_combined$longitude
  
  L_row <- floor((90.0 - T_lat) * 120.0)
  L_col <- floor(120.0 * (180.0 + T_lon))
  
  cell_numbers <- cellFromRowCol(raster_1, L_row, L_col)
  pop_values <- raster_1[cell_numbers] 
  
  all_env_var_combined$population <- pop_values
  
  ####
  
  
  all_raster_values <- vapply(seq(2, 4, 1), get_raster_values_from_xy, numeric(length(T_lat)), T_lat, T_lon)
  
  colnames(all_raster_values) <- c("travel_time", "TSI", "aedes_gen")
  
  all_env_var_combined <- cbind(all_env_var_combined, all_raster_values)
  
  dir.create(out_path, FALSE, TRUE)
  
  write.table(all_env_var_combined, 
              file.path(out_path, file_name), 
              row.names = FALSE, 
              sep = ",")
  
}

wrapper_to_load_env_var <- function(x, ...){
  
  a <- x$set_id
  #cat("set id =", a, "\n")    
  
  out <- load_env_var(set_id = a, ...)
  
}

# load Altitude, all fourier elements for DayTemp, EVI, MIR, NightTemp and RFE, 
# for ONE country,
# as elements of a list

load_env_var <- function(set_id, my_path, FTs_data, txt_file_roots) {
  
  # browser()
  
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

append_env_var_by_tile <- function(x, a, b){
  
  out <- x[[1]]
  
  names(out) <- b[[1]]
  
  for (i in seq(2, length(x))){
    
    cat("dataset no =", i, "\n")
    
    dataset <- x[i]
    
    if(!is.na(dataset))
    {
      
      vars <- a[[i]]
      cat("variables =", vars, "\n")
      
      sub_dataset <- dataset[[1]][, vars]
      
      out <- cbind(out, sub_dataset)
      
      var_names <- b[[i]] 
      
      names(out)[names(out) %in% vars] <- var_names
      
    }
    
  }
  
  out
  
}

assign_NA <- function(i, var_list) {
  
  year.i <- 2007
  year.f <- 2014
  ppyear <- 64
  n.years <- year.f - year.i + 1
  ntimes <- ppyear * n.years 
  
  dat <- var_list[[i]]
  
  if (i != 1 | i != 7) {
    
    if(i == 2 | i == 3) {
      
      # DayTemp or NightTemp
      
      na.val <- -273 * ntimes
      na.rows <- which(dat$const_term < na.val)
      dat[na.rows, -(1:3)] <- NA
      
    } else if(i == 4) {
      
      # EVI
      
      na.val <- -0.2 * ntimes
      na.rows <- which(dat$const_term < na.val)
      dat[na.rows, -(1:3)] <- NA
      
    } else if(i == 5) {
      
      # MIR
      
      na.val <- 0 * ntimes
      na.rows <- which(dat$const_term < na.val)
      dat[na.rows, -(1:3)] <- NA
    }    
    
  }
  
  dat
  
}
