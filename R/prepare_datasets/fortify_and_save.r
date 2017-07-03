fortify_and_save <- function(shp_fl, out_path, out_name){
  
  shp_fl <- shp_fl[!shp_fl@data$NAME_ENGLI == "Antarctica" | shp_fl@data$NAME_ENGLI == "Caspian Sea", ]
  
  a <- fortify(shp_fl)
  
  dir.create(file.path("output", "shapefiles"), FALSE, TRUE)
  
  write_out_rds(a, out_path, out_name)

}
