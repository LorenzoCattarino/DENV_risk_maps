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
