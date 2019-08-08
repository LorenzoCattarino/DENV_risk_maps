make_nice_map <- function(bbox_df, 
                          countries_df, 
                          pred, 
                          z_vals = NULL,
                          parms, 
                          my_col, 
                          country_fill_col, 
                          ttl) {
  
  barwdt <- parms$barwdt
  barhgt <- parms$barhgt
  pol_brd_sz <- parms$pol_brd_sz
  leg_pos_x <- parms$leg_pos_x
  leg_pos_y <- parms$leg_pos_y
  leg_txt_sz <- parms$leg_txt_sz 
  leg_ttl_sz <- parms$leg_ttl_sz

  if(is.null(z_vals)){
    z_vals <- pretty(pred[, "layer"], n = 5)
  }

  ggplot() +
    geom_polygon(data = bbox_df, aes(long, lat, group = group), fill = "aliceblue") +
    geom_polygon(data = countries_df, aes(long, lat, group = group), fill = country_fill_col) +
    geom_tile(data = pred, aes(x = x, y = y, fill = layer)) +
    scale_fill_gradientn(breaks = z_vals,
                         labels = z_vals,
                         limits = c(min(z_vals), max(z_vals)),
                         colours = my_col, 
                         guide = guide_colourbar(title = ttl, 
                                                 barwidth = barwdt, 
                                                 barheight = barhgt)) +
    # geom_path(data = countries_df,
    #           aes(x = long, y = lat, group = group),
    #           colour = "gray40",
    #           size = pol_brd_sz) +
    # geom_path(data = bbox_df,
    #           aes(long, lat, group = group),
    #           colour = "black",
    #           size = 0.3) +
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
  
}