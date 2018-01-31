wrapper_to_admin_map <- function(x, 
                                 my_colors, 
                                 df_long, 
                                 country_shp, 
                                 out_path, 
                                 map_size){
  
  
  # define parameters / variables -----------------------------------------------

  
  var <- x$var 
  scenario_id <- x$scenario_id
  statsc <- x$statistic  
  
  col <- my_colors[[2]]
  
  if(statsc == "mean" | statsc == "best" | statsc == "median"){
    if(var == "FOI"){
      ttl <- var
    }
    if(grepl("R0", var)){
      ttl <- expression('R'[0])
    }
    if(grepl("I_", var)){
      ttl <- "Annual infections"
    }
    if(grepl("C_", var)){
      ttl <- "Annual cases"
    }
  }
  if(statsc == "sd"){
    ttl <- "SD"
  }
  if(statsc == "interv"){
    ttl <- "95% CI"
  }
  if(statsc == "lCI"){
    ttl <- "2.5_quantile"
  }
  if(statsc == "uCI"){
    ttl <- "97.5_quantile"
  }
  
  if(var == "FOI"){
    
    out_fl_nm <- paste0(statsc, "_", var, "_adm1.png")
    
  } else {
    
    out_fl_nm <- paste0(statsc, "_", var, "_", scenario_id, "_adm1.png")
    
  }
  
  
  # replace NA values with zero to allow different 
  # colour coding of below-threshold values        ------------------------------
  
  
  na_logic <- apply(as.matrix(df_long[, statsc]), 1, anyNA)
  
  df_long[na_logic, statsc] <- 0
  
  
  # plot differently NA values -------------------------------------------------- 
  
  
  if(var == "R0_r" & (statsc == "mean" | statsc == "best" | statsc == "median")) {
    
    na_cutoff <- 1 
    
  } else {
    
    na_cutoff <- 0  
    
  }  
  
  df_long[df_long[, statsc] < na_cutoff, statsc] <- NA # Error if statsc is NA 
  
  
  # make map --------------------------------------------------------------------  
  
  
  if(map_size == "small"){
    plot_wdt <- 8
    plot_hgt <- 4  
  }
  if(map_size == "medium"){
    plot_wdt <- 12
    plot_hgt <- 6     
  }
  if(map_size == "large"){
    plot_wdt <- 28
    plot_hgt <- 12
  }
  
  dir.create(out_path, FALSE, TRUE)
  
  png(file.path(out_path, out_fl_nm),
      width = plot_wdt,
      height = plot_hgt,
      units = "in",
      pointsize = 12,
      res = 300)

  p <- map_predictions_admin_ggplot(df = df_long, 
                                    shp = country_shp,
                                    var_to_plot = statsc,
                                    out_path = out_path, 
                                    out_file_name = out_fl_nm, 
                                    my_col = col, 
                                    ttl = ttl, 
                                    map_size = map_size)
  
  print(p)
  
  dev.off()  

}

map_predictions_admin_ggplot <- function(df, 
                                         shp, 
                                         var_to_plot, 
                                         my_col, 
                                         ttl,
                                         map_size){
  
  if(map_size == "small"){
    plot_wdt <- 8
    plot_hgt <- 4  
    barwdt <- 1.5
    barhgt <- 6.5
    pol_brd_sz <- 0.1
    leg_pos_x <- 0.15
    leg_pos_y <- 0.3
    leg_txt_sz <- 10 
    leg_ttl_sz <- 12
  }
  if(map_size == "medium"){
    plot_wdt <- 12
    plot_hgt <- 6     
    barwdt <- 0.15
    barhgt <- 0.7
    pol_brd_sz <- 0.1
    leg_pos_x <- 0.025
    leg_pos_y <- 0.09
    leg_txt_sz <- 15 
    leg_ttl_sz <- 22
  }
  if(map_size == "large"){
    plot_wdt <- 28
    plot_hgt <- 12
    barwdt <- 0.15
    barhgt <- 0.7
    pol_brd_sz <- 0.1
    leg_pos_x <- 0.025
    leg_pos_y <- 0.09
    leg_txt_sz <- 15 
    leg_ttl_sz <- 22
  }
  
  if(var_to_plot == "p9"){
    
    df$layer1 <- cut(df$layer, breaks = c(-Inf, 50, 70, Inf), right = FALSE)
    
    p <- ggplot() + 
      geom_tile(data = df, aes(x = x, y = y, fill = layer1)) +
      scale_fill_manual(values = my_col,
                        labels = c("< 50", "50-70", "> 70"),
                        guide = guide_legend(title = ttl, 
                                             keywidth = 4, 
                                             keyheight = 5))
  } else {
    
    leg_val <- pretty(df[, var_to_plot], 5)
    
    p <- ggplot() +
      geom_polygon(data = df, 
                   aes_string(x = "long", 
                              y = "lat", 
                              group = "group",
                              fill = var_to_plot),
                   color = NA) +
      scale_fill_gradientn(breaks = leg_val,
                           labels = leg_val,
                           limits = c(min(leg_val), max(df[, var_to_plot])),
                           colours = my_col, 
                           guide = guide_colourbar(title = ttl, 
                                                   barwidth = barwdt, 
                                                   barheight = barhgt),
                           na.value = "grey70")
    
  }
  
  p + geom_path(data = shp,
                aes(x = long, y = lat, group = group),
                colour = "gray40",
                size = pol_brd_sz) +
    coord_equal() +
    scale_x_continuous(labels = NULL, limits = c(-180, 180), expand = c(0, 0)) +
    scale_y_continuous(labels = NULL, limits = c(-60, 90), expand = c(0, 0)) +
    theme_void() + 
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          plot.margin = unit(c(0, 0, 0, -0.09), "cm"),
          legend.position = c(leg_pos_x, leg_pos_y),
          legend.text = element_text(size = leg_txt_sz),
          legend.title = element_text(face = "bold", size = leg_ttl_sz))

}

map_obs_and_preds <- function(df,
                              var_to_plot,
                              out_path, 
                              out_file_name, 
                              my_col, 
                              ttl, 
                              map_size){
  
  if(map_size == "small"){
    plot_wdt <- 8
    plot_hgt <- 8  
    barwdt <- 1.5
    barhgt <- 6.5
    pol_brd_sz <- 0.1
    leg_txt_sz <- 12 
    leg_ttl_sz <- 15
  }
  
  leg_val <- pretty(df[, var_to_plot], 5)
  
  p <- ggplot() +
    geom_polygon(data = df, 
                 aes_string(x = "long", 
                            y = "lat", 
                            group = "group",
                            fill = var_to_plot),
                 color = NA) +
    scale_fill_gradientn(breaks = leg_val,
                         labels = leg_val,
                         limits = c(min(leg_val), max(df[, var_to_plot])),
                         colours = my_col, 
                         guide = guide_colourbar(title = ttl, 
                                                 barwidth = barwdt, 
                                                 barheight = barhgt),
                         na.value = "grey70") + 
    geom_path(data = countries,
              aes(x = long, y = lat, group = group),
              colour = "gray40",
              size = pol_brd_sz) +
    facet_wrap(~variable, ncol = 1) +
    coord_equal() +
    scale_x_continuous(labels = NULL, limits = c(-180, 180), expand = c(0, 0)) +
    scale_y_continuous(labels = NULL, limits = c(-60, 90), expand = c(0, 0)) +
    theme_void() + 
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), "in"),
          legend.position = "left",
          legend.text = element_text(size = leg_txt_sz),
          legend.title = element_text(face = "bold", size = leg_ttl_sz))
  
  dir.create(out_path, FALSE, TRUE)
  
  png(file.path(out_path, out_file_name),
      width = plot_wdt,
      height = plot_hgt,
      units = "in",
      pointsize = 12,
      res = 300)
  
  print(p)
  
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
