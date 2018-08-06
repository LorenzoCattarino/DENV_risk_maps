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

wrapper_to_square_map <- function(x, 
                                  my_colors, 
                                  model_tp, 
                                  shp_fl, 
                                  out_path, 
                                  map_size, 
                                  in_dts_tag){
  
  
  # define parameters / variables ----------------------------------------------- 
  
  
  var <- x$var 
  scenario_id <- x$scenario_id
  statsc <- x$statistic  
  
  gr_size <- 20
  
  res <- (1 / 120) * gr_size
  
  lats <- seq(-90, 90, by = res)
  lons <- seq(-180, 180, by = res)
  
  j <- 2
  
  col <- my_colors[[j]]
  
  if(statsc == "mean" | statsc == "best" | statsc == "median"){
    if(var == "FOI"){
      ttl <- var
    }
    if(grepl("R0", var)){
      ttl <- expression('R'[0])
    }
    if(grepl("I_", var)){
      ttl <- "Annual infections"
    }
    if(grepl("C_", var)){
      ttl <- "Annual cases"
    }
  }
  if(statsc == "sd"){
    ttl <- "SD"
  }
  if(statsc == "interv"){
    ttl <- "95% CI"
  }
  if(statsc == "lCI"){
    ttl <- "2.5_quantile"
  }
  if(statsc == "uCI"){
    ttl <- "97.5_quantile"
  }

  
  # load data -------------------------------------------------------------------  
  
  
  if(var == "FOI"){
    
    out_fl_nm <- paste0(statsc, "_", var, ".png")
    
    mean_pred_fl_nm <- paste0(var, "_", in_dts_tag, ".rds")
    
  } else {
    
    out_fl_nm <- paste0(statsc, "_", var, "_", scenario_id, ".png")
    
    mean_pred_fl_nm <- paste0(var, "_", in_dts_tag, "_", scenario_id, ".rds")

  }
  
  df_long <- readRDS(
    file.path(
      "output",
      "predictions_world",
      model_tp,
      mean_pred_fl_nm))
  
  
  # create matrix of values ----------------------------------------------------- 
  
  
  df_long$lat.int <- floor(df_long$lat.grid * 6 + 0.5)
  df_long$long.int <- floor(df_long$long.grid * 6 + 0.5)
  
  lats.int <- lats * 6
  lons.int <- lons * 6
  
  mat <- matrix(0, nrow = length(lons), ncol = length(lats))
  
  i.lat <- findInterval(df_long$lat.int, lats.int)
  i.lon <- findInterval(df_long$long.int, lons.int)
  
  mat[cbind(i.lon, i.lat)] <- df_long[, statsc]
  

  # convert matrix to raster object --------------------------------------------- 
  
  
  mat_ls <- list(x = lons,
                 y = lats,
                 z = mat)
  
  r_mat <- raster(mat_ls)
  
  
  # get raster extent ----------------------------------------------------------- 
  
  
  my_ext <- matrix(r_mat@extent[], nrow = 2, byrow = TRUE) 
  
  
  # apply same extent to the shape file -----------------------------------------  
  
  
  shp_fl@bbox <- my_ext
  
  
  # mask the raster to the shape file ------------------------------------------- 
  
  
  r_mat_msk <- mask(r_mat, shp_fl)
  
  
  # convert to ggplot-friendly objects ------------------------------------------  
  
  
  r_spdf <- as(r_mat_msk, "SpatialPixelsDataFrame")
  
  r_df <- as.data.frame(r_spdf)
  
  shp_fl_fort <- fortify(shp_fl)
  
  
  # plot differently NA values -------------------------------------------------- 
  
  
  if(var == "R0_r" & (statsc == "mean" | statsc == "best" | statsc == "median")) {
    
    na_cutoff <- 1 
  
  } else {
  
    na_cutoff <- 0  
  
  }  
  
  r_df$layer[r_df$layer < na_cutoff] <- NA
  
  
  # make map --------------------------------------------------------------------  
  

  map_predictions_pixel_ggplot(df = r_df, 
                               shp = shp_fl_fort, 
                               out_path = out_path, 
                               out_file_name = out_fl_nm,
                               my_col = col, 
                               ttl = ttl,
                               map_size = map_size,
                               statsc = statsc)
  
}

map_predictions_pixel_ggplot <- function(df, 
                                         shp, 
                                         out_path, 
                                         out_file_name, 
                                         my_col, 
                                         ttl, 
                                         map_size, 
                                         statsc){
  
  if(map_size == "small"){
    plot_wdt <- 8
    plot_hgt <- 4  
    barwdt <- 1.5
    barhgt <- 6.5
    pol_brd_sz <- 0.1
    leg_pos_x <- 0.15
    leg_pos_y <- 0.3
    leg_txt_sz <- 10 
    leg_ttl_sz <- 12
  }
  if(map_size == "medium"){
    plot_wdt <- 12
    plot_hgt <- 6     
    barwdt <- 0.15
    barhgt <- 0.7
    pol_brd_sz <- 0.1
    leg_pos_x <- 0.025
    leg_pos_y <- 0.09
    leg_txt_sz <- 15 
    leg_ttl_sz <- 22
  }
  if(map_size == "large"){
    plot_wdt <- 28
    plot_hgt <- 12
    barwdt <- 0.15
    barhgt <- 0.7
    pol_brd_sz <- 0.1
    leg_pos_x <- 0.025
    leg_pos_y <- 0.09
    leg_txt_sz <- 15 
    leg_ttl_sz <- 22
  }
  
  dir.create(out_path, FALSE, TRUE)
  
  png(file.path(out_path, out_file_name),
      width = plot_wdt,
      height = plot_hgt,
      units = "in",
      pointsize = 12,
      res = 300)
  
  if(statsc == "p9"){
    
    df$layer1 <- cut(df$layer, breaks = c(-Inf, 50, 70, Inf), right = FALSE)
    
    p <- ggplot() + 
      geom_tile(data = df, aes(x = x, y = y, fill = layer1)) +
      scale_fill_manual(values = my_col,
                        labels = c("< 50", "50-70", "> 70"),
                        guide = guide_legend(title = ttl, 
                                             keywidth = 4, 
                                             keyheight = 5))
  } else {
    
    leg_val <- pretty(df$layer, 5)
    
    #max_leg_val <- max(df$layer)
    max_leg_val <- 0.05
    
    p <- ggplot() +
      geom_tile(data = df, aes(x = x, y = y, fill = layer)) +
      scale_fill_gradientn(breaks = leg_val,
                           labels = leg_val,
                           limits = c(min(leg_val), max_leg_val),
                           colours = my_col, 
                           guide = guide_colourbar(title = ttl, 
                                                   barwidth = barwdt, 
                                                   barheight = barhgt),
                           na.value = "grey90")
    
  }
  
  # p2 <- p + geom_path(data = shp,
  #                     aes(x = long, y = lat, group = group),
  #                     colour = "gray40",
  #                     size = pol_brd_sz) +
    p2 <- p + coord_equal() +
    scale_x_continuous(labels = NULL, limits = c(-180, 180), expand = c(0, 0)) +
    scale_y_continuous(labels = NULL, limits = c(-60, 90), expand = c(0, 0)) +
    theme_void() + 
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), "in"),
          legend.position = c(leg_pos_x, leg_pos_y),    
          legend.text = element_text(size = leg_txt_sz),                       
          legend.title = element_text(face = "bold", size = leg_ttl_sz))
  
  print(p2)
  
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
