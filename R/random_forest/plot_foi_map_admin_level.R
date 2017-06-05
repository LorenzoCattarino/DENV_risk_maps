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
