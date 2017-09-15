wrapper_to_ggplot_map <- function(
  i, vars, my_colors, 
  titles_vec, df_long, country_shp, 
  shp_fort, out_path, do.p9.logic){
  
  
  # ----------------------------------------
  
  
  gr_size <- 20
  
  res <- (1 / 120) * gr_size
  
  lats <- seq(-90, 90, by = res)
  lons <- seq(-180, 180, by = res)
  
  var <- vars[i]
  
  if (i == 1){
    j <- 1
  } 
  if (i > 1 & i <= 7){
    j <- 2 
  }
  if (i > 7){
    j <- 3
  }
  
  col <- my_colors[[j]]
  ttl <- titles_vec[i]
  do.p9 <- do.p9.logic[i]
  
  # ---------------------------------------- create matrix of values
  
  
  df_long$lat.int=floor(df_long$lat.grid*6+0.5)
  df_long$long.int=floor(df_long$long.grid*6+0.5)
  
  lats.int=lats*6
  lons.int=lons*6
  
  mat <- matrix(0, nrow = length(lons), ncol = length(lats))
  
  i.lat <- findInterval(df_long$lat.int, lats.int)
  i.lon <- findInterval(df_long$long.int, lons.int)
  
  mat[cbind(i.lon, i.lat)] <- df_long[, var]
  
  
  # ---------------------------------------- convert matrix to raster object
  
  
  mat_ls <- list(x = lons,
                 y = lats,
                 z = mat)
  
  r_mat <- raster(mat_ls)
  
  
  #----------------------------------------- get raster extent 
  
  
  my_ext <- matrix(r_mat@extent[], nrow = 2, byrow = TRUE) 
  
  
  # ---------------------------------------- apply same extent to the shape file 
  
  
  country_shp@bbox <- my_ext
  
  
  # ---------------------------------------- mask the raster to the shape file
  
  
  r_mat_msk <- mask(r_mat, country_shp)
  
  
  # ---------------------------------------- convert to ggplot-friendly objects 
  
  
  r_spdf <- as(r_mat_msk, "SpatialPixelsDataFrame")
  
  r_df <- as.data.frame(r_spdf)
  
  
  # ---------------------------------------- make map 
  
  
  out_fl_nm <- paste0(var, "_0_1667_deg.png")
  
  map_data_pixel_ggplot(df = r_df, 
                        shp = shp_fort, 
                        out_path = out_path, 
                        out_file_name = out_fl_nm,
                        my_col = col, 
                        ttl = ttl,
                        do.p9 = do.p9)
  
}

map_data_pixel_ggplot <- function(df, shp, out_path, out_file_name, my_col, ttl, do.p9) {
  
  browser()
  
  dir.create(out_path, FALSE, TRUE)
  
  png(file.path(out_path, out_file_name),
      width = 28, # original: 7
      height = 12, # original: 3
      units = "in",
      pointsize = 12,
      res = 300)
  
  if(do.p9){
    
    df$layer1 <- cut(df$layer, breaks = c(-Inf, 50, 70, Inf), right = FALSE)
    
    p <- ggplot(data = df, aes(x = x, y = y)) + 
      geom_tile(aes(fill = layer1)) +
      scale_fill_manual(values = my_col,
                        labels = c("< 50", "50-70", "> 70"),
                        guide = guide_legend(title = ttl, 
                                             keywidth = 4, 
                                             keyheight = 5))
  } else {
    
    p <- ggplot(data = df, aes(x = x, y = y)) +
      geom_tile(aes(fill = layer)) +
      scale_fill_gradientn(colours = my_col, 
                           guide = guide_colourbar(title = ttl, 
                                                   barwidth = dev.size()[1] * 0.15, 
                                                   barheight = dev.size()[1] * 0.7))
    
  }
  
  p2 <- p + geom_path(data = shp,
                      aes(x = long, y = lat, group = group),
                      colour = "gray40",
                      size = 0.3) +
    coord_equal() +
    scale_x_continuous(labels = NULL, limits = c(-180, 180), expand = c(0, 0)) +
    scale_y_continuous(labels = NULL, limits = c(-60, 90), expand = c(0, 0)) +
    theme_void() + 
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          plot.margin = unit(c(0, 0, 0, -0.09), "cm"),
          legend.position = c(dev.size()[1] * 0.005, dev.size()[1] * 0.008),
          legend.text = element_text(size = 25),
          legend.title = element_text(face = "bold", size = 30))#,
  #legend.background = element_rect(fill = alpha("white", 0.2), colour = "gray50"),
  #panel.background = element_rect(fill = "#A6CEE3", colour = NA)) # lightblue2
  
  print(p2)
  
  dev.off()
  
}
