#### Find countries in adm1 and adm2 GADM shapefiles ####
 
# load packages
library(rgdal)

# load data 
adm0_countries <- readOGR(dsn = file.path("data", "shapefiles", "gadm28_levels.shp"), 
                          layer = "gadm28_adm0",
                          stringsAsFactors = FALSE)

adm1_countries <- readOGR(dsn = file.path("data", "shapefiles", "gadm28_levels.shp"), 
                          layer = "gadm28_adm1",
                          stringsAsFactors = FALSE)

adm2_countries <- readOGR(dsn = file.path("data", "shapefiles", "gadm28_levels.shp"), 
                         layer = "gadm28_adm2",
                         stringsAsFactors = FALSE)

adm0_iso_name_duplicated <- adm0_countries@data[, c("ID_0", "ISO", "NAME_ENGLI")]
adm0_iso_name <- adm0_iso_name_duplicated[!duplicated(adm0_iso_name_duplicated), ]
adm0_iso <- adm0_iso_name$ISO

adm1_iso_name_duplicated <- adm1_countries@data[, c("ISO", "ID_0", "NAME_0", "ID_1", "NAME_1")] 
adm1_iso_name <- adm1_iso_name_duplicated[!duplicated(adm1_iso_name_duplicated), ]
adm1_iso <- adm1_iso_name$ISO

adm2_iso_name_duplicated <- adm2_countries@data[, c("ISO", "NAME_0")] 
adm2_iso_name <- adm2_iso_name_duplicated[!duplicated(adm2_iso_name_duplicated), ]
adm2_iso <- adm2_iso_name$ISO

adm1_in_adm0.logical <- adm0_iso %in% adm1_iso
adm2_in_adm1.logical <- adm1_iso %in% adm2_iso

missing_countries_adm1 <- adm0_iso_name[!adm1_in_adm0.logical, ]
missing_countries_adm2 <- adm1_iso_name[!adm2_in_adm1.logical, ]

# Change column names 
# colnames(adm0_iso_name)[colnames(adm0_iso_name) == "NAME_ENGLI"] <- "country"
# colnames(adm1_iso_name) <- c("country_code", "country")
# colnames(adm2_iso_name) <- c("country_code", "country")
# colnames(missing_countries_adm1) <- c("country_code", "country")
# colnames(missing_countries_adm2) <- c("country_code", "country")
  
# write out list of all countries in the world  
write.csv(adm0_iso_name[order(adm0_iso_name$country), ], 
          file.path("output", "datasets", "all_adm_0_countries.csv"), 
          row.names = FALSE)

# write out all admin 1 GADM boundaries  
write.csv(adm1_iso_name[order(adm1_iso_name$NAME_0), ], 
          file.path("output", "datasets", "all_adm_1_countries.csv"), 
          row.names = FALSE)

# write out list of countries which have admin 2 GADM boundaries  
write.csv(adm2_iso_name[order(adm2_iso_name$country), ], 
          file.path("output", "datasets", "all_adm_2_countries.csv"), 
          row.names = FALSE)

# write out list of countries which DO NOT have admin 1 boundaries 
write.csv(missing_countries_adm1[order(missing_countries_adm1$country), ], 
          file.path("output", "datasets", "missing_countries_adm_1.csv"), 
          row.names = FALSE)

# write out list of countries which DO NOT have admin 2 boundaries 
write.csv(missing_countries_adm2[order(missing_countries_adm2$country), ], 
          file.path("output", "datasets", "missing_countries_adm_2.csv"), 
          row.names = FALSE)
