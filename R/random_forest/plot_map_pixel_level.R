map_data_pixel <- function(
  data_to_plot, country_borders, output_path, 
  file_name, plot_width, plot_height,
  row_values, col_values, color_change_values,
  x_lab, y_lab, 
  key_title, color_key,
  low_y_lim, upp_y_lim,
  map_colours, leg_title_size,
  key_ttl_x){
  
  #browser()
  
  dir.create(output_path, FALSE, TRUE)
  
  png(file.path(output_path, file_name),
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
  
  p <- levelplot(
    x = data_to_plot, 
    row.values = row_values,
    column.values = col_values,
    at = color_change_values,
    scales = list(draw = FALSE),
    xlab = x_lab,
    ylab = y_lab, 
    xlim = c(-180, 180),
    ylim = c(low_y_lim, upp_y_lim),
    col.regions = map_colours,
    colorkey = color_key,
    par.settings = theme.novpadding,
    panel = function(...){
      panel.levelplot.raster(...)
      sp.polygons(country_borders, lwd = 0.2, col = "gray30", fill = "transparent")})

  key <- draw.colorkey(p$legend[[1]]$args$key)
  
  p$legend <- NULL
  
  key$framevp$x <- unit(0.18, "npc")
  key$framevp$y <- unit(0.20, "npc")
  
  print(p)
  
  grid.draw(key)
  
  grid.text(key_title, y = 0.40, x = key_ttl_x, gp = gpar(fontsize = leg_title_size))
  
  dev.off()

}