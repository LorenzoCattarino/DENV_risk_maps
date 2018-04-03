quick_raster_map <- function(pred_df, y_var, out_pt, out_name) {
  
  #browser()
  
  gr_size <- 20
  
  res <- (1 / 120) * gr_size
  
  lats <- seq(-90, 90, by = res)
  lons <- seq(-180, 180, by = res)
  
  my_col <- colorRamps::matlab.like(100)
  
  
  # ---------------------------------------- load data 
  
  
  pred_df$lat.int <- floor(pred_df$latitude * 6 + 0.5)
  pred_df$long.int <- floor(pred_df$longitude * 6 + 0.5)
  
  lats.int <- lats * 6
  lons.int <- lons * 6
  
  mat <- matrix(NA, nrow = length(lons) - 1, ncol = length(lats) - 1)
  
  i.lat <- findInterval(pred_df$lat.int, lats.int)
  i.lon <- findInterval(pred_df$long.int, lons.int)
  
  mat[cbind(i.lon, i.lat)] <- pred_df[, y_var]
  
  dir.create(out_pt, FALSE, TRUE)
  
  png(file.path(out_pt, out_name), 
      width = 18, 
      height = 8, 
      units = "cm",
      pointsize = 12,
      res = 300)
  
  par(mar = c(0,0,0,0), oma = c(0,0,0,0))
  
  ticks <- pretty(pred_df[, y_var], n = 5)
  
  image(lons, lats, mat, col = my_col, ylim = c(-60, 90), asp = 1)
  image.plot(lons,
             lats,
             mat,
             legend.only = TRUE,
             col = my_col,
             legend.width = 1,
             legend.shrink = 0.5,
             axis.args = list(at = ticks, 
                              labels = ticks),
             smallplot = c(0.04, 0.08, 0.04, 0.4))
  par(mar = par("mar"))
  dev.off()

}
