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