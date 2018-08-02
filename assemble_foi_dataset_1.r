# Geocode location of each data point to find long and lat

# install dev version of ggmap for geocode()
if(!require("ggmap", character.only = TRUE)) {
  devtools::install_github("dkahle/ggmap")
}

library(geosphere) # for areaPolygon() 


# ===================================================================
#
# 
# GET a key for the google map API geocoding tool 
# Do enable the google map geocoding tool on your google developer project page
# https://console.cloud.google.com/home/dashboard?project=dengue-mapping
# 
#
# ===================================================================


source(file.path("R", "prepare_datasets", "geolocating_data_points.R"))
source(file.path("R", "utility_functions.R"))


# define parameters -----------------------------------------------------------  


my_api_key <- "AIzaSyBuHLASHGLaorGdZidB5sNa-9C2fxXYj1c"

datasets <- c("additional serology",
              "additional_India_sero_data_Garg",
              "additional_India_sero_data_Shah",
              "additional_sero_data_feb2018",
              "All_caseReport_datasets",
              "NonSerotypeSpecificDatasets",
              "SerotypeSpecificDatasets")

in_pt <- file.path("data", "foi")

foi_out_pt <- file.path("output", "foi")

foi_out_nm <- "FOI_estimates_lon_lat.csv"

fields <- c("type", 
            "ISO",
            "country", 
            "FOI", 
            "variance", 
            "latitude",
            "longitude", 
            "reference", 
            "date")


# register your api key ------------------------------------------------------- 


register_google(key = my_api_key, account_type = "premium", second_limit = 10000, day_limit = 10000000)


# pre processing -------------------------------------------------------------- 


dts_paths <- list.files(in_pt, 
                        pattern = "csv$",
                        full.names = TRUE)

output_dts <- vector("list", length = length(dts_paths))

all_dts <- lapply(dts_paths, read.csv, header = TRUE, sep = ",", stringsAsFactors = FALSE)


# loop ------------------------------------------------------------------------ 


for (i in seq_along(all_dts)){

  one_dts <- all_dts[[i]]  

  dts_name <- datasets[i]
    
  if(dts_name == "All_caseReport_datasets") {
    
    one_dts$type <- "caseReport"
    
  } else {
    
    one_dts$type <- "serology"
    
  }
  
  one_dts_ls <- df_to_list(one_dts, use_names = TRUE)  
  
  n <- 2 # lon and lat
  
  x_and_y <- vapply(one_dts_ls, get_xy, numeric(n))  
  
  x_and_y <- t(x_and_y)
  
  colnames(x_and_y) <- c("longitude", "latitude")
  
  output_dts[[i]] <- cbind(one_dts, x_and_y)
  
}  
  

# subset ----------------------------------------------------------------------


output_dts_2 <- lapply(output_dts, subset_list_df, fields)

All_FOI_estimates <- do.call("rbind", output_dts_2)

All_FOI_estimates <- subset(All_FOI_estimates, !is.na(FOI))
  
All_FOI_estimates <- subset(All_FOI_estimates, ISO != "PYF" & ISO != "HTI")


# save ------------------------------------------------------------------------  


write.csv(All_FOI_estimates, 
          file.path(foi_out_pt, foi_out_nm), 
          row.names = FALSE)
