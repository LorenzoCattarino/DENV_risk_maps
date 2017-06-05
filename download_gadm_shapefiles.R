rm(list=ls())

# load packages
library(maptools)

# load functions
source(file.path("R", "prepare_datasets", "download_gadm_shp.R"))

# load data 
world_shp_admin_0 <- readShapePoly(file.path("data", "shapefiles", "gadm28_levels.shp", "gadm28_adm0.shp"))
world_shp_admin_1 <- readShapePoly(file.path("data", "shapefiles", "gadm28_levels.shp", "gadm28_adm1.shp"))
world_shp_admin_2 <- readShapePoly(file.path("data", "shapefiles", "gadm28_levels.shp", "gadm28_adm2.shp"))

all_countries_in_world_shp_admin_0_duplicated <- world_shp_admin_0@data[,c("ISO", "NAME_ENGLI")]
all_countries_in_world_shp_admin_0 <- all_countries_in_world_shp_admin_0_duplicated[!duplicated(all_countries_in_world_shp_admin_0_duplicated),]
all_ISO_codes_admin_0 <- as.character(all_countries_in_world_shp_admin_0$ISO)

all_countries_in_world_shp_admin_1_duplicated <- world_shp_admin_1@data[,c("ISO", "NAME_0")] 
all_countries_in_world_shp_admin_1 <- all_countries_in_world_shp_admin_1_duplicated[!duplicated(all_countries_in_world_shp_admin_1_duplicated),]
all_ISO_codes_admin_1 <- as.character(all_countries_in_world_shp_admin_1$ISO)

all_countries_in_world_shp_admin_2_duplicated <- world_shp_admin_2@data[,c("ISO", "NAME_0")] 
all_countries_in_world_shp_admin_2 <- all_countries_in_world_shp_admin_2_duplicated[!duplicated(all_countries_in_world_shp_admin_2_duplicated),]
all_ISO_codes_admin_2 <- as.character(all_countries_in_world_shp_admin_2$ISO)

admin_1_in_admin_0.logical <- all_ISO_codes_admin_0 %in% all_ISO_codes_admin_1 
admin_2_in_admin_1.logical <- all_ISO_codes_admin_1 %in% all_ISO_codes_admin_2 

missing_countries.admin_1 <- all_countries_in_world_shp_admin_0[!admin_1_in_admin_0.logical,]
missing_countries.admin_2 <- all_countries_in_world_shp_admin_1[!admin_2_in_admin_1.logical,]

# Change column names 
colnames(all_countries_in_world_shp_admin_1) <- c("country_code", "country")
colnames(all_countries_in_world_shp_admin_2) <- c("country_code", "country")
colnames(missing_countries.admin_1) <- c("country_code", "country")
colnames(missing_countries.admin_2) <- c("country_code", "country")
  
# write out list of countries which have admin 1 GADM boundaries  
write.table(all_countries_in_world_shp_admin_1[order(all_countries_in_world_shp_admin_1$country),], 
            file.path("output", "datasets", "all_adm_1_countries.csv"), row.names = FALSE, sep=",")

# write out list of countries which have admin 2 GADM boundaries  
write.table(all_countries_in_world_shp_admin_2[order(all_countries_in_world_shp_admin_2$country),], 
            file.path("output", "datasets", "all_adm_2_countries.csv"), row.names = FALSE, sep=",")

# write out list of countries which DO NOT have admin 1 boundaries 
write.table(missing_countries.admin_1[order(missing_countries.admin_1$country),], 
            file.path("output", "datasets", "missing_countries_adm_1.csv"), row.names = FALSE, sep=",")

# write out list of countries which DO NOT have admin 2 boundaries 
write.table(missing_countries.admin_2[order(missing_countries.admin_2$country),], 
            file.path("output", "datasets", "missing_countries_adm_2.csv"), row.names = FALSE, sep=",")

#### Download shapefiles of all countries with admin 1 boundaries ####
no_cores <- parallel::detectCores()
cl <- parallel::makePSOCKcluster(no_cores)
download <- parallel::clusterApply(cl, all_ISO_codes_admin_1, dwnl.gadm.shp)
parallel::stopCluster(cl)
