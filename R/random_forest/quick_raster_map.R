quick_raster_map <- function(pred_df, out_pt, out_name) {
  
  dir.create(out_pt, FALSE, TRUE)
  
  browser()
  
  gr_size <- 20
  
  res <- (1 / 120) * gr_size
  
  lats <- seq(-90, 90, by = res)
  lons <- seq(-180, 180, by = res)
  
  
  # ---------------------------------------- load data 
  
  
  pred_df$lat.int <- floor(pred_df$latitude*6+0.5)
  pred_df$long.int <- floor(pred_df$longitude*6+0.5)
  
  lats.int <- lats*6
  lons.int <- lons*6
  
  mat <- matrix(NA, nrow = length(lons) - 1, ncol = length(lats) - 1)
  
  i.lat <- findInterval(pred_df$lat.int, lats.int)
  i.lon <- findInterval(pred_df$long.int, lons.int)
  
  mat[cbind(i.lon, i.lat)] <- pred_df$p_i
  
  png(file.path(out_pt, out_name), 
      type = "cairo", 
      antialias = "none", 
      width = 7, 
      height = 3, 
      units = "in",
      pointsize = 12,
      res = 300)
  
  par(mar = c(0,0,0,0), oma = c(0,0,0,0))
  
  image.plot(lons, lats, mat, asp = 1)
  
  dev.off()

}
