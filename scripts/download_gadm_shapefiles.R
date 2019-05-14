#### Download shapefiles of all countries with admin 1 boundaries ####

# load functions
source(file.path("R", "prepare_datasets", "download_gadm_shp.R"))

all_adm1_countries <- read.csv(
  file.path("output", "datasets", "all_adm_1_countries.csv"), 
  row.names = FALSE,
  stringsAsFactors = FALSE)

all_ISO_codes_admin_1 <- as.character(all_adm1_countries$ISO)

no_cores <- parallel::detectCores()

cl <- parallel::makePSOCKcluster(no_cores)

download <- parallel::clusterApply(cl, all_ISO_codes_admin_1, dwnl.gadm.shp)

parallel::stopCluster(cl)
