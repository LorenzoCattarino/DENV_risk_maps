map_burden_admin_scale <- function (
  y_var, map_file, map_title, run_id, exp_id, adm_shp_file, 
  country_border_shp_file, map_colours, output_folder, do.log) {
  
  dir.create(output_folder, FALSE, TRUE)
  
  file_name <- paste0(
    map_file, 
    paste("_exp", exp_id, sep = "_"), 
    paste("_run", run_id, sep = "_"), 
    ".jpg")
  
  # Create list object for country borders
  country_border_shp_list <- list("sp.polygons", 
                                  country_border_shp_file, 
                                  lwd = 0.2, 
                                  col = "gray30", 
                                  first = FALSE)
  
  if (do.log) {
    
    # Log10 transform y_var
    adm_shp_file@data[, y_var] <- log10(adm_shp_file@data[, y_var] + 1)
    
    my_colorkey <- list(at = pretty(10^(adm_shp_file@data[, y_var])-1, 12),
                        labels = list(labels = gsub(" ", "", format(pretty(10^(adm_shp_file@data[, y_var])-1), scientific = FALSE),
                                                    fixed = TRUE),
                                      at = pretty(10^(adm_shp_file@data[, y_var])-1, 6)))
    
  } else {
    
    my_colorkey <- TRUE
  
  }
  
  jpeg(file.path(output_folder, file_name), 
       width = 16, 
       height = 6, 
       units = "in", 
       pointsize = 12,
       res = 300)

  print(spplot(adm_shp_file, 
               zcol = y_var,
               col = NA,
               scales = list(x = list(draw = TRUE, 
                                      at = seq(-150, 150, 50)), 
                             y = list(draw = TRUE,
                                      at = seq(-60, 60, 20))),
               xlab = "Longitude",
               ylab = "Latitude", 
               main = list(label = map_title, cex = 1.3),
               col.regions = map_colours,
               colorkey = my_colorkey,
               ylim = c(-80, 90),
               sp.layout = list(country_border_shp_list)))
  dev.off()
 
}
