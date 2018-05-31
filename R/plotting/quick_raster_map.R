quick_raster_map <- function(pred_df, variable = NULL, statistic, out_pt, out_name) {
  
  # browser()
  
  gr_size <- 20
  
  res <- (1 / 120) * gr_size
  
  lats <- seq(-90, 90, by = res)
  lons <- seq(-180, 180, by = res)
  
  my_col <- matlab.like(100)
  
  if(!is.null(variable)){
    
    if(variable == "p9"){
      
      my_col <- c("red", "orange", "green")
      pred_df[, statistic] <- cut(pred_df[, statistic], breaks = c(-Inf, 80, 90, Inf), right = FALSE, labels = FALSE)
    }
    
  }
  
  
  # ---------------------------------------- load data 
  
  
  pred_df$lat.int <- floor(pred_df$latitude * 6 + 0.5)
  pred_df$long.int <- floor(pred_df$longitude * 6 + 0.5)
  
  lats.int <- lats * 6
  lons.int <- lons * 6
  
  mat <- matrix(NA, nrow = length(lons) - 1, ncol = length(lats) - 1)
  
  i.lat <- findInterval(pred_df$lat.int, lats.int)
  i.lon <- findInterval(pred_df$long.int, lons.int)
  
  mat[cbind(i.lon, i.lat)] <- pred_df[, statistic]
  
  dir.create(out_pt, FALSE, TRUE)
  
  png(file.path(out_pt, out_name), 
      width = 16, 
      height = 5.5, 
      units = "cm",
      pointsize = 12,
      res = 300)
  
  par(mar = c(0,0,0,0), oma = c(0,0,0,0))
  
  ticks <- pretty(pred_df[, statistic], n = 5)
  # ticks <- seq(0, 0.015, 0.005)
  # ticks <- seq(0, 0.06, 0.01)
  
  image(lons, 
        lats, 
        mat, 
        col = my_col, 
        zlim = c(min(ticks), max(ticks)), 
        ylim = c(-60, 60), 
        asp = 1,
        axes = FALSE)
  
  image.plot(lons,
             lats,
             mat,
             col = my_col,
             zlim = c(min(ticks), max(ticks)),
             legend.only = TRUE,
             legend.width = 1,
             legend.shrink = 0.75,
             axis.args = list(at = ticks, 
                              labels = ticks,
                              cex.axis = 0.8),
             smallplot = c(0.04, 0.08, 0.1, 0.5))
  
  par(mar = par("mar"))
  
  dev.off()

}
