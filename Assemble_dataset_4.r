# load packages
library(maptools)


# ---------------------------------------- define paramaters 


out_pt <- file.path("output", "datasets") 

out_nm <- "pseudo_absence_points_NUM_CODES.csv"


# ---------------------------------------- load data


pseudo_absence_points <- read.csv(
  file.path("output", 
            "datasets", 
            "pseudo_absence_points.csv"), 
  header = TRUE, 
  sep = ",", 
  stringsAsFactors = FALSE)

# national border shapefile
adm1_shp_fl <- readShapePoly(
  file.path("data", 
            "shapefiles", 
            "gadm28_levels.shp", 
            "gadm28_adm1.shp"))

adm2_shp_fl <- readShapePoly(
  file.path("data", 
            "shapefiles", 
            "gadm28_levels.shp", 
            "gadm28_adm2.shp"))


# ---------------------------------------- run 


pseudo_absence_points$ID_0 <- 0
pseudo_absence_points$ID_1 <- 0
pseudo_absence_points$ID_2 <- 0

for (i in 1:nrow(pseudo_absence_points)){
  
  cat("row number =", i, "\n")
  
  c_name <- pseudo_absence_points[i, "ISO"]
  cat("country code =", c_name, "\n")
  
  adm1_code <- pseudo_absence_points[i, "adm1"]
  cat("admin 1 code =", adm1_code, "\n")
  
  lng <- pseudo_absence_points[i, "longitude"]
  cat("longitude =", lng, "\n")
  
  lat <- pseudo_absence_points[i, "latitude"] 
  cat("latitude =", lat, "\n")
  
  location_xy <- data.frame(lng, lat)
    
  overlay <- over(SpatialPoints(location_xy), adm2_shp_fl)
  
  ID_2 <- overlay$ID_2
  
  my_sub <- adm1_shp_fl@data[adm1_shp_fl@data$ISO == c_name & adm1_shp_fl@data$NAME_1 == adm1_code, ] 
  
  if(nrow(my_sub) > 1){
    
    warning("more one than 1 match")
    
    ID_0 <- my_sub[1, "ID_0"]
    ID_1 <- my_sub[1, "ID_1"]
  
  }else{
    
    ID_0 <- my_sub$ID_0
    ID_1 <- my_sub$ID_1
    
  }
  
  pseudo_absence_points[i, "ID_0"] <- ID_0
  pseudo_absence_points[i, "ID_1"] <- ID_1
  pseudo_absence_points[i, "ID_2"] <- ID_2
}

write.csv(pseudo_absence_points,
  file.path(out_pt, out_nm), 
  row.names = FALSE)
