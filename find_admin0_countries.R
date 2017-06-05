rm(list=ls())

# load packages
library(maptools) 

# load functions
source(file.path("R", "prepare_datasets", "functions_for_finding_no_dengue_countries.R"))

# load data 
#ISO_country_codes.file <- read.csv(file.path("data", "ISO_country_codes.csv"), header = TRUE, sep = ",")
world_shp_admin_0 <- readShapePoly(file.path("data", "shapefiles", "gadm28_levels.shp", "gadm28_adm0.shp"))
world_shp_admin_1 <- readShapePoly(file.path("data", "shapefiles", "gadm28_levels.shp", "gadm28_adm1.shp"))

# find countries which are not listed in the shapefile 
country_name_in_shp_logical <- apply(ISO_country_codes.file, 1, check_country_name_in_shp, world_shp_admin_1, "ISO")
admin0_countries <- ISO_country_codes.file[!country_name_in_shp_logical,]

# Write out .csv
write.table(admin0_countries, file.path("output", "datasets", "admin_0_countries.csv"), row.names = FALSE, sep=",")
