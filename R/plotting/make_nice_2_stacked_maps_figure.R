make_nice_2_stacked_maps_figure <- function(parms,
                                            countries_df,
                                            bbox_df,
                                            pred,
                                            sd,
                                            out_path,
                                            out_file_name){

  
  my_col <- colorRamps::matlab.like(100)
  
  plot_wdt <- parms$plot_wdt
  plot_hgt <- parms$plot_hgt

    
  # plot predictions ----------------------------------------------------------
  
  
  # pred_leg_val <- pretty(pred$layer, 5)
  pred_leg_val <- c(0, 0.02, 0.04, 0.06)
  
  pred_map <- make_nice_map(bbox_df, countries_df, pred, pred_leg_val, parms, my_col, country_fill_col = "grey80", "FOI")
  
  
  # plot sd -------------------------------------------------------------------
  
  
  # cap the sd
  
  # sd_leg_val <- pretty(sd$layer, 5)
  sd_leg_val <- seq(0, 0.02, 0.01)
  
  sd_map <- make_nice_map(bbox_df, countries_df, sd, sd_leg_val, parms, my_col, country_fill_col = "grey80", "SD")
  
  
  # save ------------------------------------------------------------------------
  
  
  dir.create(out_path, FALSE, TRUE)
  
  png(file.path(out_path, out_file_name),
      width = plot_wdt,
      height = plot_hgt,
      units = "cm",
      pointsize = 12,
      res = 300)
  
  grid.arrange(pred_map, sd_map, nrow = 2)
  
  on.exit(dev.off())
  
}
