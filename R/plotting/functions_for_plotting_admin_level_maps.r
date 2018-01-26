wrapper_to_admin_map <- function(i, 
                                 vars, 
                                 statistics, 
                                 my_colors, 
                                 titles_vec, 
                                 df_long, 
                                 country_shp, 
                                 out_path, 
                                 do.p9.logic,
                                 plot_wdt, 
                                 plot_hgt){
  
  
  # ----------------------------------------

  statsc <- statistics[i]
  out_fl_nm <- paste0(statsc, "_", vars,"_adm_1.png")
  col <- my_colors[[2]]
  ttl <- titles_vec[i]
  do.p9 <- do.p9.logic[i]
  
  
  # ----------------------------------------
  
  
  map_predictions_admin_ggplot(
    df = df_long, 
    shp = country_shp,
    var_to_plot = statsc,
    out_path = out_path, 
    out_file_name = out_fl_nm, 
    my_col = col, 
    ttl = ttl, 
    do.p9 = do.p9, 
    plot_wdt = plot_wdt, 
    plot_hgt = plot_hgt)
  
  
}

map_predictions_admin_ggplot <- function(df, 
                                         shp, 
                                         var_to_plot, 
                                         out_path, 
                                         out_file_name, 
                                         my_col, 
                                         ttl, 
                                         do.p9, 
                                         plot_wdt, 
                                         plot_hgt) {
  
  #browser()
  
  dir.create(out_path, FALSE, TRUE)
  
  png(file.path(out_path, out_file_name),
      width = plot_wdt,
      height = plot_hgt,
      units = "in",
      pointsize = 12,
      res = 300)
  
  if(do.p9){
    
    df$layer1 <- cut(df$layer, breaks = c(-Inf, 50, 70, Inf), right = FALSE)
    
    p <- ggplot() + 
      geom_tile(data = df, aes(x = x, y = y, fill = layer1)) +
      scale_fill_manual(values = my_col,
                        labels = c("< 50", "50-70", "> 70"),
                        guide = guide_legend(title = ttl, 
                                             keywidth = 4, 
                                             keyheight = 5))
  } else {
    
    p <- ggplot() +
      geom_polygon(data = df, 
                   aes_string(x = "long", 
                              y = "lat", 
                              group = "group",
                              fill = var_to_plot),
                   color = NA) +
      scale_fill_gradientn(colours = my_col, 
                           guide = guide_colourbar(title = ttl, 
                                                   barwidth = dev.size()[1] * 0.15, 
                                                   barheight = dev.size()[1] * 0.7),
                           na.value = "grey60")
    
  }
  
  p2 <- p + geom_path(data = shp,
                      aes(x = long, 
                          y = lat, 
                          group = group),
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

map_predictions_admin <- function(
  adm_shp_fl, nat_bord_shp_fl_ls, 
  adm_lv, y_var, file_name, 
  map_colours, my_path, plot_scales, 
  x_lab, y_lab, key_title, color_key,
  low_y_lim, upp_y_lim, 
  plot_width, plot_height,
  leg_title_size, color_change_values){
  
  dir.create(my_path, FALSE, TRUE)
  
  png(file.path(my_path, file_name), 
      width = plot_width, 
      height = plot_height, 
      units = "in", 
      pointsize = 12,
      res = 300)
  
  theme.novpadding <-
    list(layout.heights =
           list(top.padding = 0,
                main.key.padding = 0,
                key.axis.padding = 0,
                axis.xlab.padding = 0,
                xlab.key.padding = 0,
                key.sub.padding = 0,
                bottom.padding = 0),
         layout.widths =
           list(left.padding = 0,
                key.ylab.padding = 0,
                ylab.axis.padding = 0,
                axis.key.padding = 0,
                right.padding = 0),
         axis.line = 
           list(col = "transparent"))
  
  p <- spplot(
    adm_shp_fl, 
    y_var, 
    col = NA,
    at = color_change_values,
    scales = list(x = list(draw = plot_scales, 
                           at = seq(-150, 150, 50)), 
                  y = list(draw = plot_scales,
                           at = seq(-60, 60, 20))),
    xlab = x_lab,
    ylab = y_lab, 
    xlim = c(-180, 180),
    ylim = c(low_y_lim, upp_y_lim),
    col.regions = map_colours,
    colorkey = color_key,
    par.settings = theme.novpadding,
    sp.layout = list(nat_bord_shp_fl_ls))
  
  key <- draw.colorkey(p$legend[[1]]$args$key)
  
  p$legend <- NULL
  
  key$framevp$x <- unit(0.18, "npc")
  key$framevp$y <- unit(0.20, "npc")
  
  print(p)
  
  grid.draw(key)
  
  grid.text(key_title, y = 0.40, x = 0.18, gp = gpar(fontsize = leg_title_size))
  
  dev.off()
  
}
