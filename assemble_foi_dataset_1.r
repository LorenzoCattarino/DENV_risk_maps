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

in_pt <- file.path("data", "foi")

foi_out_pt <- file.path("output", "foi")

foi_out_nm <- "FOI_estimates_lon_lat.csv"

fields <- c("reference", 
            "date", 
            "country",
            "ISO",
            "test_location",
            "longitude",
            "latitude",
            "no_serotypes",
            "FOI", 
            "variance",
            "type")


# register your api key ------------------------------------------------------- 


register_google(key = my_api_key, account_type = "premium", second_limit = 10000, day_limit = 10000000)


# loada data ------------------------------------------------------------------


serology_data <- read.csv(file.path("data", "foi", "All_serology_data_2.csv"),
                          header = TRUE,
                          stringsAsFactors = FALSE)

casereport_data <- read.csv(file.path("data", "foi", "All_caseReport_data.csv"),
                            header = TRUE,
                            stringsAsFactors = FALSE)


# pre processing -------------------------------------------------------------- 


serology_data$type <- "serology"

casereport_data$type <- "caseReport"
casereport_data$longitude <- NA
casereport_data$latitude <- NA

casereport_data <- casereport_data[, fields]

serology_data <- serology_data[, fields]

All_FOI_estimates <- rbind(serology_data, casereport_data)
  
All_FOI_estimates <- subset(All_FOI_estimates, !is.na(FOI))

All_FOI_estimates <- subset(All_FOI_estimates, ISO != "PYF" & ISO != "HTI")

xy_tofind <- All_FOI_estimates[is.na(All_FOI_estimates$longitude), ]


# loop ------------------------------------------------------------------------ 


for (i in seq_len(nrow(xy_tofind))){

  country_name <- xy_tofind[i, "country"]
  # cat("country =", country_name, "\n")
  
  country_code <- xy_tofind[i, "ISO"]
  # cat("country code =", country_code, "\n")
  
  admin_level <- 1 
  
  location <- xy_tofind[i, "test_location"]
  # cat("location =", location, "\n")
  
  location_str <- ifelse(country_name != location, paste(location, country_name, sep = ", "), location)
  #cat("location string = ", location_str, "\n")
  
  geocode_run <- geocode(enc2utf8(location_str), output = "all") # take care of funny characters 
  
  if(geocode_run$status != "OK") {stop("no match!")}
  
  if(length(geocode_run$results) > 1) {
    
    geocode_results <- get_geocode_results (geocode_run)
    
    viewport_areas <- apply(geocode_results, 1, calculate_viewport_area)
    
    result_match <- which(viewport_areas == max(viewport_areas))[1]
    
    location_xy <- geocode_results[result_match, c("lng", "lat")]
    
    location_xy <- as.matrix(location_xy)
    
  } else {
    
    location_xy <- cbind(geocode_run$results[[1]]$geometry$location$lng,
                         geocode_run$results[[1]]$geometry$location$lat)
    
  }
  
  xy_tofind[i, c("longitude", "latitude")] <- location_xy
  
}  
  

# subset ----------------------------------------------------------------------


All_FOI_estimates[is.na(All_FOI_estimates$longitude), ] <- xy_tofind


# save ------------------------------------------------------------------------  


write.csv(All_FOI_estimates, 
          file.path(foi_out_pt, foi_out_nm), 
          row.names = FALSE)
