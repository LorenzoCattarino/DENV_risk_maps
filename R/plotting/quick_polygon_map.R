quick_polygon_map <- function(adm_shp_fl, 
                              my_col,
                              var, 
                              out_pt, 
                              out_name,
                              leg_ttl = NULL){

  pred_leg_val <- pretty(adm_shp_fl[[var]], n = 5)
  
  p <- ggplot(data = adm_shp_fl) +
    geom_sf(mapping = aes_string(fill = var), color = NA) +
    scale_fill_gradientn(breaks = pred_leg_val,
                         labels = pred_leg_val,
                         limits = c(min(pred_leg_val), max(pred_leg_val)),
                         colours = my_col, 
                         na.value = "grey70",
                         guide = guide_colourbar(title = leg_ttl)) +
    coord_sf(xlim = c(-180, 180), ylim = c(-60, 60), expand = FALSE) +
    theme(legend.justification = c(0, 0),
          legend.position = c(0.01, 0.05),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank())
  
  dir.create(out_pt, FALSE, TRUE)
  
  png(file.path(out_pt, out_name),
      width = 16,
      height = 5.5,
      units = "cm",
      pointsize = 12,
      res = 300)
  
  print(p)
  
  on.exit(dev.off())
  
}

# Could never plot the NA values as grey - sorry sp....
# quick_polygon_map <- function(adm_shp_fl, 
#                               my_col,
#                               var, 
#                               out_pt, 
#                               out_name){
#   
#   # data_na_filled <- list("sp.polygons",
#   #                        adm_shp_fl,
#   #                        col = NA,
#   #                        first = TRUE,
#   #                        fill = "black")
#   
#   theme.novpadding <- list(layout.heights =
#                              list(top.padding = 0,
#                                   main.key.padding = 0,
#                                   key.axis.padding = 0,
#                                   axis.xlab.padding = 0,
#                                   xlab.key.padding = 0,
#                                   key.sub.padding = 0,
#                                   bottom.padding = 0),
#                            layout.widths =
#                              list(left.padding = 0,
#                                   key.ylab.padding = 0,
#                                   ylab.axis.padding = 0,
#                                   axis.key.padding = 0,
#                                   right.padding = 0),
#                            axis.line = 
#                              list(col = "transparent"))
#   
#   # adm_shp_fl@data[is.na(adm_shp_fl@data[, var]), var] <- 0
#   
#   pretty_var_range <- pretty(adm_shp_fl@data[, var], n = 5)
#   max_var <- max(pretty_var_range)
#   min_x <- adm_shp_fl@bbox[1,1]
#   max_x <- adm_shp_fl@bbox[1,2]
#     
#   p <- spplot(adm_shp_fl, 
#               var, 
#               at = seq(0, max_var, length.out = length(my_col)),
#               col = NA,
#               scales = list(x = list(draw = FALSE, 
#                                      at = seq(-150, 150, 50)), 
#                             y = list(draw = FALSE,
#                                      at = seq(-60, 60, 20))),
#               xlim = c(min_x, max_x),
#               ylim = c(-60, 60),
#               col.regions = my_col,
#               colorkey = list(space = "right", height = 0.4),
#               par.settings = theme.novpadding)#,
#   #           sp.layout = list(data_na_filled))
#   
#   key <- draw.colorkey(p$legend[[1]]$args$key)
#   
#   p$legend <- NULL
#   
#   key$framevp$x <- grid::unit(0.10, "npc")
#   key$framevp$y <- grid::unit(0.23, "npc")
#   
#   dir.create(out_pt, FALSE, TRUE)
#   
#   png(file.path(out_pt, out_name), 
#       width = 16, 
#       height = 5.5, 
#       units = "cm",
#       pointsize = 12,
#       res = 300)
#   
#   print(p)
#   
#   grid::grid.draw(key)
#   
#   dev.off()
#   
# }
