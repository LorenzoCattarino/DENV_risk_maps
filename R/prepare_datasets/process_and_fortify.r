process_and_fortify <- function(shp_fl_1, shp_fl_2, out_path, out_name){
  
  shp_fl_1 <- shp_fl_1[!shp_fl_1@data$NAME_ENGLI == "Antarctica" | shp_fl_1@data$NAME_ENGLI == "Caspian Sea", ]
  
  shp_fl_1_erased <- shp_fl_1 - shp_fl_2
  
  a <- fortify(shp_fl_1_erased, region = "OBJECTID")
  
  dir.create(file.path("output", "shapefiles"), FALSE, TRUE)
  
  writeOGR(shp_fl_1_erased, file.path("output", "shapefiles"), "gadm28_adm0_eras", driver = "ESRI Shapefile")
  
  write_out_rds(a, out_path, out_name)

}
