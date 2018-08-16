make_nice_map <- function(parms,
                          map_proj,
                          countries,
                          bbox,
                          pred,
                          sd,
                          out_path,
                          out_file_name){
  
  
  gr_size <- parms$resample_grid_size
  
  my_col <- matlab.like(10)
  
  plot_wdt <- 20
  plot_hgt <- 20 
  barwdt <- 1.5
  barhgt <- 6.5
  pol_brd_sz <- 0.1
  leg_pos_x <- 0.10
  leg_pos_y <- 0.25
  leg_txt_sz <- 10 
  leg_ttl_sz <- 12
  
  
  # common pre processing -------------------------------------------------------
  
  
  res <- (1 / 120) * gr_size
  
  lats <- seq(-90, 90, by = res)
  lons <- seq(-180, 180, by = res)
  
  countries <- countries[!countries@data$NAME_ENGLI == "Caspian Sea", ]
  countries <- spTransform(countries, CRS(map_proj))
  countries_df <- fortify(countries)
  bbox <- spTransform(bbox, CRS(map_proj))
  bbox_df<- fortify(bbox)
  
  
  # plot predictions ------------------------------------------------------------
  
  
  pred_mat <- prediction_df_to_matrix(lats, lons, pred, "best")
  
  pred_mat_ls <- list(x = lons,
                      y = lats,
                      z = pred_mat)
  
  pred_r_mat <- raster(pred_mat_ls)
  
  pred_r_mat <- projectRaster(pred_r_mat, crs = map_proj)
  
  pred_r_spdf <- as(pred_r_mat, "SpatialPixelsDataFrame")
  
  pred_r_df <- as.data.frame(pred_r_spdf)
  
  pred_r_df <- subset(pred_r_df, layer >= 1)
  
  pred_leg_val <- pretty(pred_r_df$layer, 5)
  
  pred_map <- ggplot() +
    geom_polygon(data = bbox_df, aes(long, lat, group = group), fill = "aliceblue") +
    geom_polygon(data = countries_df, aes(long, lat, group = group), fill = "grey70") +
    geom_tile(data = pred_r_df, aes(x = x, y = y, fill = layer)) +
    scale_fill_gradientn(breaks = pred_leg_val,
                         labels = pred_leg_val,
                         limits = c(min(pred_leg_val), max(pred_r_df$layer)),
                         colours = my_col, 
                         guide = guide_colourbar(title = expression("R"[0]), 
                                                 barwidth = barwdt, 
                                                 barheight = barhgt)) +
    geom_path(data = countries_df,
              aes(x = long, y = lat, group = group),
              colour = "gray40",
              size = pol_brd_sz) +
    geom_path(data = bbox_df,
              aes(long, lat, group = group),
              colour = "black",
              size = 0.3) +
    coord_equal() +
    scale_x_continuous(labels = NULL, expand = c(0, 0)) +
    scale_y_continuous(labels = NULL, expand = c(0, 0)) +
    theme_void() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
          legend.position = c(leg_pos_x, leg_pos_y),
          legend.text = element_text(size = leg_txt_sz),
          legend.title = element_text(face = "bold", size = leg_ttl_sz),
          legend.box.background = element_rect(fill = "white", colour = "black"))
  
  
  # plot sd ---------------------------------------------------------------------
  
  
  names(sd)[names(sd) == "latitude"] <- "lat.grid"
  names(sd)[names(sd) == "longitude"] <- "long.grid"
  
  sd_mat <- prediction_df_to_matrix(lats, lons, sd, "sd")
  
  sd_mat_ls <- list(x = lons,
                    y = lats,
                    z = sd_mat)
  
  sd_r_mat <- raster(sd_mat_ls)
  
  sd_r_mat <- projectRaster(sd_r_mat, crs = map_proj)
  
  my_ext <- matrix(sd_r_mat@extent[], nrow = 2, byrow = TRUE)
  
  countries@bbox <- my_ext
  
  sd_r_mat_msk <- mask(sd_r_mat, countries)
  
  sd_r_spdf <- as(sd_r_mat_msk, "SpatialPixelsDataFrame")
  
  sd_r_df <- as.data.frame(sd_r_spdf)
  
  sd_leg_val <- pretty(sd_r_df$layer, 5)
  
  sd_map <- ggplot() +
    geom_polygon(data = bbox_df, aes(long, lat, group = group), fill = "aliceblue") +
    geom_polygon(data = countries_df, aes(long, lat, group = group), fill = "grey70") +
    geom_tile(data = sd_r_df, aes(x = x, y = y, fill = layer)) +
    scale_fill_gradientn(breaks = sd_leg_val,
                         labels = sd_leg_val,
                         limits = c(min(sd_leg_val), max(sd_r_df$layer)),
                         colours = my_col, 
                         guide = guide_colourbar(title = "SD", 
                                                 barwidth = barwdt, 
                                                 barheight = barhgt)) +
    geom_path(data = countries_df,
              aes(x = long, y = lat, group = group),
              colour = "gray40",
              size = pol_brd_sz) +
    geom_path(data = bbox_df,
              aes(long, lat, group = group),
              colour = "black",
              size = 0.3) +
    coord_equal() +
    scale_x_continuous(labels = NULL, expand = c(0, 0)) +
    scale_y_continuous(labels = NULL, expand = c(0, 0)) +
    theme_void() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
          legend.position = c(leg_pos_x, leg_pos_y),
          legend.text = element_text(size = leg_txt_sz),
          legend.title = element_text(face = "bold", size = leg_ttl_sz),
          legend.box.background = element_rect(fill = "white", colour = "black"))
  
  
  # save ------------------------------------------------------------------------
  
  
  dir.create(out_path, FALSE, TRUE)
  
  png(file.path(out_path, out_file_name),
      width = plot_wdt,
      height = plot_hgt,
      units = "cm",
      pointsize = 12,
      res = 300)
  
  grid.arrange(pred_map, sd_map, nrow = 2)
  
  dev.off()
  
}
