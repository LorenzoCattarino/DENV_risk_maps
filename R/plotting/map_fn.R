print.map <- function(x, coord_lon, coord_lat, delta.deg, title, filename, lower, upper, shp) {
  
  # create a vector of ordered (grid) values of latitude & longitude
  lon <- seq(min(coord_lon), max(coord_lon), by = delta.deg)
  lat <- seq(min(coord_lat), max(coord_lat), by = delta.deg)

  map <- matrix(NA, nrow = length(lon), ncol = length(lat))
  map[cbind(match(round(coord_lon, digits = 1), round(lon, digits = 1)), match(round(coord_lat, digits = 1), round(lat, digits = 1)))] = x
  
  png(file.path("figures", filename), width = 960, height = 960)
  par(cex = 1.5, mar = c(6,5,5,3)) 
  image.plot(lon, lat, map, las = 1, asp = 1, col = tim.colors(256), main = title, xlab = "longitude", ylab = "latitude", zlim = c(lower,upper))
  plot(shp, add = TRUE)
  dev.off()
}
