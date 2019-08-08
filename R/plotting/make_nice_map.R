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
  bbox <- parms$coord_limits
  
  x1 <- bbox[1]
  x2 <- bbox[2]
  y1 <- bbox[3]
  y2 <- bbox[4]
  
  if(is.null(z_vals)){
    z_vals <- pretty(pred[, "layer"], n = 5)
  }
  
  ggplot() +
    geom_sf(data = countries_df, fill = country_fill_col, color = NA) +
    geom_tile(data = pred, aes(x = x, y = y, fill = layer)) +
    geom_sf(data = countries_df, fill = NA, colour = "gray40", size = pol_brd_sz) +
    scale_fill_gradientn(breaks = z_vals,
                         labels = z_vals,
                         limits = c(min(z_vals), max(z_vals)),
                         colours = my_col, 
                         guide = guide_colourbar(title = ttl, 
                                                 barwidth = barwdt, 
                                                 barheight = barhgt)) +
    coord_sf(datum = NA, xlim = c(x1, x2), ylim = c(y1, y2), expand = FALSE) +
    theme(panel.background = element_rect(fill = "aliceblue"),
          panel.border = element_rect(fill = NA, colour = "black"),
          axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          legend.justification = c(0, 0),
          legend.position = c(leg_pos_x, leg_pos_y),
          plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
          panel.grid.major = element_blank(),
          legend.text = element_text(size = leg_txt_sz),
          legend.title = element_text(face = "bold", size = leg_ttl_sz),
          legend.box.background = element_rect(fill = "white", colour = "black"),
          legend.box.margin = margin(0.1, 0.1, 0.1, 0.1, unit = "cm"))
  
}