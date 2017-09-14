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
  
  if (i == 1) {
    j <- 1
  } else {
    j <- 2 
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