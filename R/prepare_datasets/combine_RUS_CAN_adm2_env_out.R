append.and.aggregate.CAN_RUS_adm2 <- function(x, altitude_variables, fourier_transform_elements, FTs_data, LandCover_variables, destination_folder, origin_admin_level, aggregation_admin_level)
{
  result_folders <- c("altitude", "FTs", "LandCover")
                        
  missing_admin_1_RUS <- c(4, 12, 26, 45, 46, 80) # north of +60deg of latitude
  missing_admin_1_CAN <- c(6, 8, 13)              # north of +60deg of latitude
  
  country_code <- x
  cat("country code = ", country_code, "\n")
    
  admin_code <- paste ("adm", origin_admin_level, sep="")
  cat("admin level code = ", admin_code, "\n")    
    
  for (i in 1:length(result_folders))
  {
    result_folder <- result_folders[i]
    cat("result folder = ", result_folder, "\n")
    
    if(result_folder == "altitude")
    {
      
      if(country_code == "RUS")
      {
      
        root_file_name <- sprintf ("alt_global_%s_%s", admin_code, country_code)
        cat("root file = ", root_file_name, "\n") 
      
        out_vec <- NULL
        
        for (z in 1:83)  
        {
          
          file_name <- sprintf ("%s%s%s%s", root_file_name, z, "of83", ".txt")
          #cat("file = ", file_name, "\n")
          
          file_ex <- file.exists(file.path("data", "processed", admin_code, result_folder, file_name))
          
          if(file_ex==TRUE)
          {
            txt_file <- read.table(file.path("data", "processed", admin_code, result_folder, file_name), header=TRUE, sep="\t", quote="")
          }else{
            txt_file <- c(z, rep (NA, length(altitude_variables) + 6))
          }
          
          out_vec <- rbind(out_vec, txt_file)
          
        }
        
        cat("missing admin 1 = ", out_vec[which(is.na(out_vec$NAME_1)==TRUE),"adm1"], "\n")
        
        out_file_name <- paste(root_file_name, "txt", sep=".")
        
        write.table(out_vec, 
                    file.path(destination_folder, "processed",
                              paste("adm", origin_admin_level, sep=""),
                              result_folder,
                              out_file_name), sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
        
        missing_admin_logical <- out_vec$adm1 %in% missing_admin_1_RUS
        out_vec_2 <- out_vec[!missing_admin_logical,]
    
        by_adm1_NAME_1 <- out_vec_2 %>% group_by(adm1, NAME_1)
        wgt_mean <- by_adm1_NAME_1 %>% summarise_each(funs(weighted.mean(., pop, na.rm = TRUE)), one_of(altitude_variables))
        aggregated_output_file_name <- sprintf ("alt_global_%s_%s%s", paste("adm", aggregation_admin_level, sep=""), country_code, ".txt") 
        
        write.table(wgt_mean, 
                    file.path(destination_folder, "processed", 
                              paste("adm", aggregation_admin_level, sep=""), 
                              result_folder, 
                              aggregated_output_file_name), sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
        
      }
      
      if(country_code == "CAN")
      {

        root_file_name <- sprintf ("alt_global_%s_%s", admin_code, country_code)
        cat("root file = ", root_file_name, "\n") 
        
        out_vec <- NULL
        
        for (z in 1:13)  
        {
          
          file_name <- sprintf ("%s%s%s%s", root_file_name, z, "of13", ".txt")
          #cat("file = ", file_name, "\n")
          
          file_ex <- file.exists(file.path("data", "processed", admin_code, result_folder, file_name))
          
          if(file_ex==TRUE)
          {
            txt_file <- read.table(file.path("data", "processed", admin_code, result_folder, file_name), header=TRUE, sep="\t", quote="")
          }else{
            txt_file <- c(z, rep (NA, length(altitude_variables) + 6))
          }
          
          out_vec <- rbind(out_vec, txt_file)
          
        }
        
        cat("missing admin 1 = ", out_vec[which(is.na(out_vec$NAME_1)==TRUE),"adm1"], "\n")
        
        out_file_name <- paste(root_file_name, "txt", sep=".")
        
        write.table(out_vec, 
                    file.path(destination_folder, "processed",
                              paste("adm", origin_admin_level, sep=""),
                              result_folder,
                              out_file_name), sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
        
        missing_admin_logical <- out_vec$adm1 %in% missing_admin_1_CAN
        out_vec_2 <- out_vec[!missing_admin_logical,]
        
        by_adm1_NAME_1 <- out_vec_2 %>% group_by(adm1, NAME_1)
        wgt_mean <- by_adm1_NAME_1 %>% summarise_each(funs(weighted.mean(., pop, na.rm = TRUE)), one_of(altitude_variables))
        aggregated_output_file_name <- sprintf ("alt_global_%s_%s%s", paste("adm", aggregation_admin_level, sep=""), country_code, ".txt") 
        
        write.table(wgt_mean, 
                    file.path(destination_folder, "processed", 
                              paste("adm", aggregation_admin_level, sep=""), 
                              result_folder, 
                              aggregated_output_file_name), sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
        
      }  
    
    }
      
    if(result_folder == "FTs")
    {
      
      file_names <- sprintf (paste("FTfreqs_2007-2014_64ppyear", FTs_data, "%s_%s", sep="_"), admin_code, country_code)  
      
      if(country_code == "RUS")
      {
      
        for (j in 1:length(file_names))
        {
        
          root_file_name <- file_names[j] 
          cat("root file = ", root_file_name, "\n")      
      
          out_vec <- NULL
        
          for (z in 1:83)  
          {
          
            file_name <- sprintf ("%s%s%s%s", root_file_name, z, "of2445", ".txt")
            #cat("file = ", file_name, "\n")
        
            file_ex <- file.exists(file.path("data", "processed", admin_code, result_folder, file_name))
          
            if(file_ex==TRUE)
            {
              txt_file <- read.table(file.path("data", "processed", admin_code, result_folder, file_name), header=TRUE, sep="\t", quote="")
            }else{
              txt_file <- c(z, rep (NA, length(fourier_transform_elements) + 7))
            }
        
            out_vec <- rbind(out_vec, txt_file)
        
            #aggregate(no.pu ~ estimate + action, data = selected_effort, sum)
          }
        
          cat("missing admin 1 = ", out_vec[which(is.na(out_vec$NAME_1)==TRUE),"adm1"], "\n")
      
          out_file_name <- paste(root_file_name, "txt", sep=".")
          
          write.table(out_vec, 
                      file.path(destination_folder, "processed",
                                paste("adm", origin_admin_level, sep=""),
                                result_folder,
                                out_file_name), sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
          
          missing_admin_logical <- out_vec$adm1 %in% missing_admin_1_RUS 
          out_vec_2 <- out_vec[!missing_admin_logical,]
          
          by_adm1_NAME_1 <- out_vec_2 %>% group_by(adm1, NAME_1)
          wgt_mean <- by_adm1_NAME_1 %>% summarise_each(funs(weighted.mean(., pop, na.rm = TRUE)), one_of(fourier_transform_elements))
          sum <- summarise(by_adm1_NAME_1, pop = sum(pop))
          
          aggregated_output_file_name <- sprintf ("FTfreqs_2007-2014_64ppyear_%s_%s_%s%s", FTs_data[j], paste("adm", aggregation_admin_level, sep=""), country_code, ".txt") 

          write.table(cbind(sum, wgt_mean[, fourier_transform_elements]), 
                      file.path(destination_folder, "processed", 
                                paste("adm", aggregation_admin_level, sep=""), 
                                result_folder, 
                                aggregated_output_file_name), sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)

        }
      
      }
  
      if(country_code == "CAN")
      {
        
        for (j in 1:length(FTs_data))
        {
      
          root_file_name <- file_names[j] 
          cat("root file = ", root_file_name, "\n")
      
          out_vec <- NULL
        
          for (z in 1:13)  
          {
          
            file_name <- sprintf ("%s%s%s%s", root_file_name, z, "of294", ".txt")
            #cat("file = ", file_name, "\n")
        
            file_ex <- file.exists(file.path("data", "processed", admin_code, result_folder, file_name))
        
            if(file_ex==TRUE)
            {
              txt_file <- read.table(file.path("data", "processed", admin_code, result_folder, file_name), header=TRUE, sep="\t", quote="")
            }else{
              txt_file <- c(z, rep (NA, length(fourier_transform_elements) + 7))
            }
        
            out_vec <- rbind(out_vec, txt_file)
        
          }
      
          cat("missing admin 1 = ", out_vec[which(is.na(out_vec$NAME_1)==TRUE),"adm1"], "\n")
      
          out_file_name <- paste(root_file_name, "txt", sep=".")
          
          write.table(out_vec, 
                      file.path(destination_folder, "processed",
                                paste("adm", origin_admin_level, sep=""),
                                result_folder,
                                out_file_name), sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
          
          missing_admin_logical <- out_vec$adm1 %in% missing_admin_1_CAN
          out_vec_2 <- out_vec[!missing_admin_logical,]
          
          by_adm1_NAME_1 <- out_vec_2 %>% group_by(adm1, NAME_1)
          wgt_mean <- by_adm1_NAME_1 %>% summarise_each(funs(weighted.mean(., pop, na.rm = TRUE)), one_of(fourier_transform_elements))
          sum <- summarise(by_adm1_NAME_1, pop = sum(pop))
          
          aggregated_output_file_name <- sprintf ("FTfreqs_2007-2014_64ppyear_%s_%s_%s%s", FTs_data[j], paste("adm", aggregation_admin_level, sep=""), country_code, ".txt") 

          write.table(cbind(sum, wgt_mean[, fourier_transform_elements]), 
                      file.path(destination_folder, "processed", 
                                paste("adm", aggregation_admin_level, sep=""), 
                                result_folder, 
                                aggregated_output_file_name), sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)

        }
    
      }

    }
    
    if(result_folder == "LandCover")
    {

      if(country_code == "RUS")
      {
        
        root_file_name <- sprintf ("MCD12Q1.A.051.lct1_2012_5km_%s_%s", admin_code, country_code)
        cat("root file = ", root_file_name, "\n") 
        
        out_vec <- NULL
        
        for (z in 1:83)  
        {
          
          file_name <- sprintf ("%s%s%s%s", root_file_name, z, "of83", ".txt")
          #cat("file = ", file_name, "\n")
          
          file_ex <- file.exists(file.path("data", "processed", admin_code, result_folder, file_name))
          
          if(file_ex==TRUE)
          {
            txt_file <- read.table(file.path("data", "processed", admin_code, result_folder, file_name), header=TRUE, sep="\t", quote="")
          }else{
            txt_file <- c(z, rep (NA, length(LandCover_variables) + 6))
          }
          
          out_vec <- rbind(out_vec, txt_file)
          
        }
        
        cat("missing admin 1 = ", out_vec[which(is.na(out_vec$NAME_1)==TRUE),"adm1"], "\n")
        
        out_file_name <- paste(root_file_name, "txt", sep=".")
        
        write.table(out_vec, 
                    file.path(destination_folder, "processed",
                              paste("adm", origin_admin_level, sep=""),
                              result_folder,
                              out_file_name), sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
        
        missing_admin_logical <- out_vec$adm1 %in% missing_admin_1_RUS
        out_vec_2 <- out_vec[!missing_admin_logical,]
        
        by_adm1_NAME_1 <- out_vec_2 %>% group_by(adm1, NAME_1)
        wgt_mean <- by_adm1_NAME_1 %>% summarise_each(funs(weighted.mean(., pop, na.rm = TRUE)), one_of(LandCover_variables))
        aggregated_output_file_name <- sprintf ("MCD12Q1.A.051.lct1_2012_5km_%s_%s%s", paste("adm", aggregation_admin_level, sep=""), country_code, ".txt") 

        write.table(wgt_mean, 
                    file.path(destination_folder, "processed", 
                              paste("adm", aggregation_admin_level, sep=""), 
                              result_folder, 
                              aggregated_output_file_name), sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)

      }
      
      if(country_code == "CAN")
      {

        root_file_name <- sprintf ("MCD12Q1.A.051.lct1_2012_5km_%s_%s", admin_code, country_code)
        cat("root file = ", root_file_name, "\n") 
        
        out_vec <- NULL
        
        for (z in 1:13)  
        {
          
          file_name <- sprintf ("%s%s%s%s", root_file_name, z, "of13", ".txt")
          #cat("file = ", file_name, "\n")
          
          file_ex <- file.exists(file.path("data", "processed", admin_code, result_folder, file_name))
          
          if(file_ex==TRUE)
          {
            txt_file <- read.table(file.path("data", "processed", admin_code, result_folder, file_name), header=TRUE, sep="\t", quote="")
          }else{
            txt_file <- c(z, rep (NA, length(LandCover_variables) + 6))
          }
          
          out_vec <- rbind(out_vec, txt_file)
          
        }
        
        cat("missing admin 1 = ", out_vec[which(is.na(out_vec$NAME_1)==TRUE),"adm1"], "\n")
        
        out_file_name <- paste(root_file_name, "txt", sep=".")
        
        write.table(out_vec, 
                    file.path(destination_folder, "processed",
                              paste("adm", origin_admin_level, sep=""),
                              result_folder,
                              out_file_name), sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
        
        missing_admin_logical <- out_vec$adm1 %in% missing_admin_1_CAN
        out_vec_2 <- out_vec[!missing_admin_logical,]
        
        by_adm1_NAME_1 <- out_vec_2 %>% group_by(adm1, NAME_1)
        wgt_mean <- by_adm1_NAME_1 %>% summarise_each(funs(weighted.mean(., pop, na.rm = TRUE)), one_of(LandCover_variables))
        aggregated_output_file_name <- sprintf ("MCD12Q1.A.051.lct1_2012_5km_%s_%s%s", paste("adm", aggregation_admin_level, sep=""), country_code, ".txt") 

        write.table(wgt_mean, 
                    file.path(destination_folder, "processed", 
                              paste("adm", aggregation_admin_level, sep=""), 
                              result_folder, 
                              aggregated_output_file_name), sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
        
      }
    
    }

  }

}
