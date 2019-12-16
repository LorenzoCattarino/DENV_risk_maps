quick_raster_map <- function(pred_df, 
                             variable = NULL, 
                             statistic, 
                             my_col, 
                             out_pt, 
                             out_name, 
                             z_range = NULL,
                             shp = NULL, 
                             key_ttle = NULL) {
  
  # browser()
  
  n_col <- length(my_col)
  
  gr_size <- 20
  
  res <- (1 / 120) * gr_size
  
  lats <- seq(-90, 90, by = res)
  lons <- seq(-180, 180, by = res)
  
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
  lower <- min(ticks)
  upper <- max(ticks)  
    
  if(!is.null(z_range)){
    
    lower <- z_range[1] 
    upper <- z_range[2]
  
  }

  image(lons, 
        lats, 
        mat, 
        col = my_col, 
        zlim = c(lower, upper), 
        xlim = c(-180, 180), 
        ylim = c(-60, 60),
        asp = 1,
        axes = FALSE)
  
  if(!is.null(shp)){
    
    plot(shp, border = "gray40", lwd = 0.05, add = TRUE)  
  
  }
  
  image.plot(lons,
             lats,
             mat,
             col = my_col,
             zlim = c(lower, upper),
             legend.only = TRUE,
             legend.width = 1,
             legend.shrink = 0.75,
             breaks = seq(lower, upper, length.out = n_col + 1), 
             axis.args = list(cex.axis = 0.8),
             smallplot = c(0.025, 0.065, 0.1, 0.5))
  
  if(!is.null(key_ttle)){
    
    text(-172, 7, key_ttle, cex = 0.9, adj = c(0, NA))
    
  }
  
  par(mar = par("mar"))
  
  dev.off()

}

prediction_df_to_matrix <- function(lats, lons, df_long, statsc){  
  
  df_long$lat.int <- floor(df_long$latitude * 6 + 0.5)
  df_long$long.int <- floor(df_long$longitude * 6 + 0.5)
  
  lats.int <- lats * 6
  lons.int <- lons * 6
  
  mat <- matrix(NA, nrow = length(lons), ncol = length(lats))
  
  i.lat <- findInterval(df_long$lat.int, lats.int)
  i.lon <- findInterval(df_long$long.int, lons.int)
  
  mat[cbind(i.lon, i.lat)] <- df_long[, statsc]
  
  mat
}

assign_grid_coordinates_to_prediction_df <- function (lats, lons, data_df){
  
  pred_mat <- prediction_df_to_matrix(lats, lons, data_df, "mean")
  
  pred_mat_ls <- list(x = lons,
                      y = lats,
                      z = pred_mat)
  
  pred_r_mat <- raster(pred_mat_ls)
  
  pred_r_spdf <- as(pred_r_mat, "SpatialPixelsDataFrame")
  
  as.data.frame(pred_r_spdf)
  
}

creating_color_palette <- function(n){
  
  RdYlBu_7 <- c("#4575b4", "#91bfdb", "#e0f3f8", "#ffffbf", "#fee090", "#fc8d59", "#d73027")
  RdYlBu_7_sat <- shades::saturation(RdYlBu_7, scalefac(2))
  pal_fun <- colorRampPalette(RdYlBu_7_sat)
  pal_fun(n)
  
}
