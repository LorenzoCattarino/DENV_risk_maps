calculate_viewport_area <- function(x) {
  
  areaPolygon(rbind(c(x["point_1_x"], x["point_1_y"]),
                    c(x["point_2_x"], x["point_2_y"]),
                    c(x["point_3_x"], x["point_3_y"]),
                    c(x["point_4_x"], x["point_4_y"])))

}

get_geocode_results <- function(geocode_run){
  
  number_of_matches <- length(geocode_run$results)
  
  all_results <- data.frame(type=rep(0,number_of_matches,
                                     formatted_address=rep(0,number_of_matches), 
                                     lat=rep(0,number_of_matches), lng=rep(0,number_of_matches)))
  
  all_results$type <- sapply(geocode_run$results, function(x){x$types[[1]]})
  all_results$formatted_address <- sapply(geocode_run$results, function(x){x$formatted_address})
  all_results$lat <- sapply(geocode_run$results, function(x){x$geometry$location$lat})
  all_results$lng <- sapply(geocode_run$results, function(x){x$geometry$location$lng})
  
  all_results$point_1_x <- sapply(geocode_run$results, function(x){x$geometry$viewport$northeast$lng})
  all_results$point_1_y <- sapply(geocode_run$results, function(x){x$geometry$viewport$northeast$lat})
  all_results$point_2_x <- sapply(geocode_run$results, function(x){x$geometry$viewport$northeast$lng})
  all_results$point_2_y <- sapply(geocode_run$results, function(x){x$geometry$viewport$southwest$lat})
  all_results$point_3_x <- sapply(geocode_run$results, function(x){x$geometry$viewport$southwest$lng})
  all_results$point_3_y <- sapply(geocode_run$results, function(x){x$geometry$viewport$southwest$lat})
  all_results$point_4_x <- sapply(geocode_run$results, function(x){x$geometry$viewport$southwest$lng})
  all_results$point_4_y <- sapply(geocode_run$results, function(x){x$geometry$viewport$northeast$lat})
  
  all_results
}

get_admin_info <- function(x, info_names){
  
  country_name <- x$country
  #cat("country = ", country_name, "\n")
  
  country_code <- x$ISO
  #cat("country code = ", country_code, "\n")
  
  #admin_level <- as.numeric(x["admin"]) 
  admin_level <- 1 
  
  location <- x$test_location
  #cat("location = ", location, "\n")
  
  location_str <- ifelse(country_name != location, paste(location, country_name, sep = ", "), location)
  #cat("location string = ", location_str, "\n")
  
  
  # geocode to find spatial coordinates using name of location ---------------
  
  
  geocode_run <- geocode(enc2utf8(location_str), output = "all") # take care of funny characters 
  
  if(geocode_run$status!="OK") {stop("no match!")}
    
  if(length(geocode_run$results)>1) {
    
    geocode_results <- get_geocode_results (geocode_run)
  
    viewport_areas <- apply(geocode_results, 1, calculate_viewport_area)
    
    result_match <- which(viewport_areas==max(viewport_areas))[1]
    
    location_xy <- geocode_results[result_match, c("lng","lat")]
    
    #cat("coordinates = ", unlist(location_xy), "\n")
  
    } else {
    
    location_xy <- data.frame(lng = geocode_run$results[[1]]$geometry$location$lng,
                              lat = geocode_run$results[[1]]$geometry$location$lat)
    #cat("coordinates = ", unlist(location_xy), "\n")
    }
  
  shp_folder <- paste(country_code, "adm_shp", sep="_")
  
  shp_name <- sprintf ("%s_adm%s", country_code, admin_level) 
  
  shp <- readOGR(file.path("data", "shapefiles", shp_folder), 
                 shp_name,
                 stringsAsFactors = FALSE,
                 integer64 = "allow.loss")
  
  
  # map overlay to find admin unit name using spatial coordinates ------------- 
  
  
  xy_spdf <- SpatialPoints(location_xy, proj4string = shp@proj4string)
  
  overlay <- over(xy_spdf, shp)
  
  #admin_name <- as.character(overlay$NAME_1)
  #cat("admin 1 name = ", admin_name, "\n")
  
  country_numeric_code <- overlay$ID_0
  adm_numeric_code <- overlay$ID_1
  
  setNames(c(location_xy$lat,
             location_xy$lng,
             country_numeric_code,
             adm_numeric_code),
           info_names)
  
}
