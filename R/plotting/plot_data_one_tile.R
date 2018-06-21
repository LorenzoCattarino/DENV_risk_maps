plot_tiles <- function(var, out_path, res, tile_set, tile_id, my_col) {
  
  #cat("variable =", var, "\n")
  
  year.i <- 2007
  year.f <- 2014
  ppyear <- 64
  
  lons <- seq(round(2 * min(tile_set$longitude) / res), round(2 * max(tile_set$longitude) / res), by = 2) * res / 2
  lats <- seq(round(2 * min(tile_set$latitude) / res), round(2 * max(tile_set$latitude) / res), by = 2) * res/2
  dat.mat <- matrix(NA, nrow = length(lons), ncol = length(lats))
  mm.lons <- match(round(2 * tile_set$longitude / res), round(2 * lons / res))
  mm.lats <- match(round(2 * tile_set$latitude / res), round(2 * lats / res))
  
  scale <- 1
  
  if(grepl("const_term", var)){
    
    ## annual mean
    scale <- ppyear * (year.f - year.i + 1) 
    map_title <- paste("annual mean", gsub(".const_term", "", var), sep = " ") 
    
  }
  
  if(grepl("Re.", var) | grepl("Im.", var)){
    
    scale <- ppyear * (year.f - year.i + 1) / 2 
    
  } 
  
  if(grepl("amplitude", var)){
    
    a <- gsub(".amplitude", "", var)
    b1 <- paste(a, "Re0", sep = "_")
    b2 <- paste(a, "Im0", sep = "_")
    
    ## annual amplitude
    dat.mat[cbind(mm.lons, mm.lats)] <- calc_amplitude(tile_set, b1, b2, ppyear, n.years)
    map_title <- paste("annual amplitude", a, sep = " ")
    
  } else {
    
    dat.mat[cbind(mm.lons, mm.lats)] <- tile_set[, var] / scale
  
  }
  
  # create output dir 
  dir.create(out_path, FALSE, TRUE)
  
  file_name <- paste0(var, "_", tile_id, ".png")
  
  png(file.path(out_path, file_name), 
      width = 5, 
      height = 4, 
      units = "in", 
      res = 300)
  
  par(mar = c(2,2,1,1) + 0.1)
  
  image.plot(lons, 
             lats, 
             dat.mat, 
             asp = 1, 
             col = my_col, 
             xlab = "", 
             ylab = "", 
             legend.shrink = 0.7, 
             legend.width = 1)
  
  title(main = map_title, line = 0.2)
  
  dev.off()
  
}
