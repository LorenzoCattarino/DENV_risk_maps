# Find countries in adm1 and adm2 GADM shapefiles 
 
# load packages
library(rgdal)

# load functions
source(file.path("R", "prepare_datasets", "download_gadm_shp.R"))

# load data 
adm0_countries <- readOGR(dsn = file.path("data", "shapefiles", "gadm28_levels.shp"), 
                          file = "gadm28_adm0")

adm1_countries <- readOGR(dsn = file.path("data", "shapefiles", "gadm28_levels.shp"), 
                          file = "gadm28_adm1")

adm2_countries <- readOGR(dsn = file.path("data", "shapefiles", "gadm28_levels.shp"), 
                          file = "gadm28_adm2")

all_countries_in_adm0_countries_duplicated <- adm0_countries@data[,c("ISO", "NAME_ENGLI")]
all_countries_in_adm0_countries <- all_countries_in_adm0_countries_duplicated[!duplicated(all_countries_in_adm0_countries_duplicated),]
all_ISO_codes_admin_0 <- as.character(all_countries_in_adm0_countries$ISO)

all_countries_in_adm1_countries_duplicated <- adm1_countries@data[,c("ISO", "NAME_0")] 
all_countries_in_adm1_countries <- all_countries_in_adm1_countries_duplicated[!duplicated(all_countries_in_adm1_countries_duplicated),]
all_ISO_codes_admin_1 <- as.character(all_countries_in_adm1_countries$ISO)

all_countries_in_adm2_countries_duplicated <- adm2_countries@data[,c("ISO", "NAME_0")] 
all_countries_in_adm2_countries <- all_countries_in_adm2_countries_duplicated[!duplicated(all_countries_in_adm2_countries_duplicated),]
all_ISO_codes_admin_2 <- as.character(all_countries_in_adm2_countries$ISO)

admin_1_in_admin_0.logical <- all_ISO_codes_admin_0 %in% all_ISO_codes_admin_1 
admin_2_in_admin_1.logical <- all_ISO_codes_admin_1 %in% all_ISO_codes_admin_2 

missing_countries.admin_1 <- all_countries_in_adm0_countries[!admin_1_in_admin_0.logical,]
missing_countries.admin_2 <- all_countries_in_adm1_countries[!admin_2_in_admin_1.logical,]

# Change column names 
colnames(all_countries_in_adm1_countries) <- c("country_code", "country")
colnames(all_countries_in_adm2_countries) <- c("country_code", "country")
colnames(missing_countries.admin_1) <- c("country_code", "country")
colnames(missing_countries.admin_2) <- c("country_code", "country")
  
# write out list of countries which have admin 1 GADM boundaries  
write.table(all_countries_in_adm1_countries[order(all_countries_in_adm1_countries$country),], 
            file.path("output", "datasets", "all_adm_1_countries.csv"), row.names = FALSE, sep=",")

# write out list of countries which have admin 2 GADM boundaries  
write.table(all_countries_in_adm2_countries[order(all_countries_in_adm2_countries$country),], 
            file.path("output", "datasets", "all_adm_2_countries.csv"), row.names = FALSE, sep=",")

# write out list of countries which DO NOT have admin 1 boundaries 
write.table(missing_countries.admin_1[order(missing_countries.admin_1$country),], 
            file.path("output", "datasets", "missing_countries_adm_1.csv"), row.names = FALSE, sep=",")

# write out list of countries which DO NOT have admin 2 boundaries 
write.table(missing_countries.admin_2[order(missing_countries.admin_2$country),], 
            file.path("output", "datasets", "missing_countries_adm_2.csv"), row.names = FALSE, sep=",")
