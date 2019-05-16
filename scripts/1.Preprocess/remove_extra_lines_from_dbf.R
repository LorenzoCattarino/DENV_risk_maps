rm(list=ls())

# load R packages 
library(maptools)

# load data 
all_countries_in_world_shp_admin_1 <- read.csv (file.path("output", "datasets", "all_adm_1_countries.csv"), 
                                                header = TRUE, sep = ",", stringsAsFactors = FALSE)
all_countries_in_world_shp_admin_2 <- read.csv (file.path("output", "datasets", "all_adm_2_countries.csv"), 
                                                header = TRUE, sep = ",", stringsAsFactors = FALSE)

my_fun <- function(x) {grep("\r\n", x)}

### Define parameters

Dataset <- all_countries_in_world_shp_admin_2
adm_code <- 2
shp_folder <- "shapefiles"
REPAIR <- FALSE

###

output <- NULL

for (j in 1:nrow(Dataset))
{
  country_name <- Dataset[j, "country"]
  cat("country name = ", country_name, "\n")

  country_code <- Dataset[j, "country_code"]
  cat("country code = ", country_code, "\n")
  
  file_ex <- file.exists(file.path("data", shp_folder, paste(country_code, "adm_shp", sep="_"), 
                                   paste0(country_code, "_adm", adm_code, ".shp")))
  if(file_ex == TRUE)
  {
    print("shp file exists")
    
    shp_file <- readShapePoly(file.path("data", shp_folder, paste(country_code, "adm_shp", sep="_"), 
                                      paste0(country_code, "_adm", adm_code, ".shp")))
    check_list <- apply(shp_file@data, 2, my_fun)
    check_vec <- sapply(check_list, length)
  
    if(any(check_vec > 0)) 
    { 
      print("extra lines are present in the dbf file")
  
      output <- rbind(output, Dataset[j,])
      
      if(REPAIR)
      {
        bad_cols <- which(check_vec>0)
  
        for (i in 1:length(bad_cols))
        {
  
          col_index <- bad_cols[i]
  
          new_names <- sapply(shp_file@data[[col_index]], function(x) { gsub("\r\n|\r\nAlderney\r\nAlderney\r\n", "", x) })
  
          shp_file@data[[col_index]] <- as.factor(new_names)
  
        }
  
        dir.create(file.path("data", "repaired_shapefiles", paste(country_code, "adm_shp", sep="_")), FALSE, TRUE)
  
        writeSpatialShape(shp_file, file.path("data","repaired_shapefiles", paste(country_code, "adm_shp", sep="_"),
                                              paste0(country_code, "_adm", adm_code)))
      }
    }
  }
}

file_name <- sprintf("adm_%s_countries_dbf_extra_lines%s", adm_code, ".csv")

write.table(output, 
            file.path("output", "datasets", file_name), 
            row.names=FALSE, sep=",")
