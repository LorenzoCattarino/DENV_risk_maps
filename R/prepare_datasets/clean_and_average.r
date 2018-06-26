clean_and_average <- function(dat, 
                              env_vars, 
                              grid_size, 
                              grp_flds,
                              resample) {
  
  # browser()
  
  dat[is.na(dat$aedes_gen), "aedes_gen"] <- 0 
    
  extra_layers <- c("travel_time", "TSI", "aedes_gen")
  
  env_vars_to_filter <- env_vars[!env_vars %in% extra_layers]
  
  dat[, env_vars_to_filter][dat[, env_vars_to_filter] == 0] <- NA
  
  xx <- remove_NA_rows(dat, env_vars)
  
  bb <- xx[!is.na(xx$population), ]
  
  if (nrow(bb) > 0) {
    
    bb[bb$population == 0, "population"] <- 1
    
  }
  
  if(resample){
    
    yy <- grid_up(dataset = bb, grid_size = grid_size, rnd_dist = FALSE)
    
    yy$lat.grid <- yy$lat.grid * grid_size
    yy$long.grid <- yy$long.grid * grid_size
    
    yy <- yy[, setdiff(names(yy), c("latitude", "longitude"))]
      
    names(yy)[names(yy) == "lat.grid"] <- "latitude"
    names(yy)[names(yy) == "long.grid"] <- "longitude"
    
  } else {
    
    yy <- bb
  
  }
  
  ret <- average_up(yy, grp_flds, env_vars)
  
  sqr_area_km <- (grid_size * 111.32)^2
  
  ret$pop_den <- ret$population / sqr_area_km 
  
  ret
}
