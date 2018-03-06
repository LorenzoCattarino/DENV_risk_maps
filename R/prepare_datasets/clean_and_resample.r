clean_and_resample <- function(dat, 
                               env_vars, 
                               grid_size, 
                               grp_flds) {
  
  #browser()
  
  dat[, env_vars][dat[, env_vars] == 0] <- NA
  
  xx <- remove_NA_rows(dat, env_vars)
  
  bb <- xx[!is.na(xx$population), ]
  
  if (nrow(bb) > 0) {
    
    bb[bb$population == 0, "population"] <- 1
    
  }
  
  yy <- grid_up(dataset = bb, grid_size = grid_size, rnd_dist = FALSE)
  
  ret <- average_up(yy, c(grp_flds, "cell", "lat.grid", "long.grid"), env_vars)
  
  ret$lat.grid <- ret$lat.grid * grid_size
  ret$long.grid <- ret$long.grid * grid_size
  
  sqr_area_km <- (grid_size * 111.32)^2
  
  ret$pop_den <- ret$population / sqr_area_km 
  
  ret
}
