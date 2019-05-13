make_nice_map <- function(parms,
                          countries_df,
                          bbox_df,
                          pred,
                          sd,
                          out_path,
                          out_file_name){
  
  plot_wdt <- parms$plot_wdt
  plot_hgt <- parms$plot_hgt
  barwdt <- parms$barwdt
  barhgt <- parms$barhgt
  pol_brd_sz <- parms$pol_brd_sz
  leg_pos_x <- parms$leg_pos_x
  leg_pos_y <- parms$leg_pos_y
  leg_txt_sz <- parms$leg_txt_sz 
  leg_ttl_sz <- parms$leg_ttl_sz

  my_col <- matlab.like(100)
  
  
  # plot predictions ----------------------------------------------------------
  
  
  pred_leg_val <- pretty(pred$layer, 5)
  
  pred_map <- ggplot() +
    geom_polygon(data = bbox_df, aes(long, lat, group = group), fill = "aliceblue") +
    geom_polygon(data = countries_df, aes(long, lat, group = group), fill = "grey70") +
    geom_tile(data = pred, aes(x = x, y = y, fill = layer)) +
    scale_fill_gradientn(breaks = pred_leg_val,
                         labels = pred_leg_val,
                         limits = c(min(pred_leg_val), max(pred$layer)),
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
          legend.box.background = element_rect(fill = "white", colour = "black"),
          legend.box.margin = margin(0.1, 0.1, 0.1, 0.1, unit = "cm"))
  
  
  # plot sd -------------------------------------------------------------------
  
  
  sd_leg_val <- pretty(sd$layer, 5)
  
  sd_map <- ggplot() +
    geom_polygon(data = bbox_df, aes(long, lat, group = group), fill = "aliceblue") +
    geom_polygon(data = countries_df, aes(long, lat, group = group), fill = my_col[1]) +
    geom_tile(data = sd, aes(x = x, y = y, fill = layer)) +
    scale_fill_gradientn(breaks = sd_leg_val,
                         labels = sd_leg_val,
                         limits = c(min(sd_leg_val), max(sd$layer)),
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
          legend.box.background = element_rect(fill = "white", colour = "black"),
          legend.box.margin = margin(0.1, 0.1, 0.1, 0.1, unit = "cm"))
  
  
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
