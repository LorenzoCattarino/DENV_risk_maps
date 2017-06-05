get_admin_name <- function(x, country_code_fld)
{
  # start 
  country_name <- x$country
  #cat("country = ", country_name, "\n")
  
  country_code <- x[country_code_fld]  
  #cat("country code = ", country_code, "\n")
  
  #admin_level <- as.numeric(x["admin"]) 
  admin_level <- 1 
  
  location <- x$test_location
  #cat("location = ", location, "\n")
  
  location_str <- ifelse(country_name != location, paste(location, country_name, sep = ", "), location)
  #cat("location string = ", location_str, "\n")
  
  geocode_run <- geocode (enc2utf8(location_str), output = "all") # take care of funny characters 
  
  if(geocode_run$status!="OK") {stop("no match!")}
    
  if(length(geocode_run$results)>1)
  {
    geocode_results <- get_geocode_results (geocode_run)
  
    viewport_areas <- apply(geocode_results, 1, function(x) {areaPolygon(rbind(c(x["point_1_x"], x["point_1_y"]),
                                                                               c(x["point_2_x"], x["point_2_y"]),
                                                                               c(x["point_3_x"], x["point_3_y"]),
                                                                               c(x["point_4_x"], x["point_4_y"])))})
    result_match <- which(viewport_areas==max(viewport_areas))[1]
    location_xy <- geocode_results[result_match, c("lng","lat")]
    #cat("coordinates = ", unlist(location_xy), "\n")
  }else{
    location_xy <- data.frame(lng=geocode_run$results[[1]]$geometry$location$lng,
                              lat=geocode_run$results[[1]]$geometry$location$lat)
    #cat("coordinates = ", unlist(location_xy), "\n")
  }
  
  shp_folder <- paste(country_code, "adm_shp", sep="_")
  shp_name <- sprintf ("%s_adm%s%s", country_code, admin_level, ".shp") 
  shp <- readShapePoly(file.path("data", 
                                 "shapefiles", 
                                 shp_folder, 
                                 shp_name))
  overlay <- over(SpatialPoints(location_xy),shp)
  
  admin_name <- as.character(overlay$NAME_1)
  #cat("admin 1 name = ", admin_name, "\n")
  
  country_numeric_code <- overlay$ID_0
  adm_numeric_code <- overlay$ID_1
  
  output <- c()
  output[1] <- admin_name
  output[2] <- as.numeric(location_xy["lng"])
  output[3] <- as.numeric(location_xy["lat"])
  output[4] <- country_numeric_code
  output[5] <- adm_numeric_code
  output
}
