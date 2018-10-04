quick_polygon_map <- function(adm_shp_fl, 
                              country, 
                              my_col,
                              var, 
                              out_pt, 
                              out_name){
  
  # country_list <- list("sp.polygons",
  #                      country,
  #                      col = NA,
  #                      fill = my_col[1],
  #                      first = TRUE)
  
  # browser()
  
  theme.novpadding <-list(layout.heights =
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
  
  adm_shp_fl@data[is.na(adm_shp_fl@data[, var]), var] <- 0
  
  pretty_var_range <- pretty(adm_shp_fl@data[, var], n = 5)
  max_var <- max(pretty_var_range)
  min_x <- adm_shp_fl@bbox[1,1]
  max_x <- adm_shp_fl@bbox[1,2]
    
  p <- spplot(adm_shp_fl, 
              var, 
              at = seq(0, max_var, length.out = length(my_col)),
              col = NA,
              scales = list(x = list(draw = FALSE, 
                                     at = seq(-150, 150, 50)), 
                            y = list(draw = FALSE,
                                     at = seq(-60, 60, 20))),
              xlim = c(min_x, max_x),
              ylim = c(-60, 60),
              col.regions = my_col,
              colorkey = list(space = "right", height = 0.4),
              par.settings = theme.novpadding)#,
              #sp.layout = list(country_list))
  
  key <- draw.colorkey(p$legend[[1]]$args$key)
  
  p$legend <- NULL
  
  key$framevp$x <- unit(0.10, "npc")
  key$framevp$y <- unit(0.23, "npc")
  
  dir.create(out_pt, FALSE, TRUE)
  
  png(file.path(out_pt, out_name), 
      width = 16, 
      height = 5.5, 
      units = "cm",
      pointsize = 12,
      res = 300)
  
  print(p)
  
  grid.draw(key)
  
  #grid.text("title", y = 0.40, x = 0.18, gp = gpar(fontsize = 8))
  
  dev.off()
  
}
