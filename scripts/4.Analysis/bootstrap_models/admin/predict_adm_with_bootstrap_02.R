# Gets only endemic countries 

source(file.path("R", "utility_functions.R"))
source(file.path("R", "create_parameter_list.R"))

library(sf)
library(dplyr)


# define extra parameters -----------------------------------------------------


extra_prms <- list(id = 4)
                   
                   
# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

model_type <- paste0("model_", parameters$id)

out_fl_nm <- "response_endemic.rds"

out_path <- file.path("output", 
                      "predictions_world",
                      "bootstrap_models",
                      model_type,
                      "adm_1")


# load data -------------------------------------------------------------------


adm_shp <- st_read(dsn = file.path("output", "shapefiles"), 
                   layer = "gadm28_adm1_dengue",
                   stringsAsFactors = FALSE)

sqr_preds <- readRDS(file.path("output", 
                               "predictions_world",
                               "bootstrap_models",
                               model_type,
                               "adm_1",
                               "response.rds"))


# -----------------------------------------------------------------------------


adm_shp_2 <- as.data.frame(adm_shp)

endemic_countries <- subset(adm_shp_2, dengue == 1)

ret <- inner_join(sqr_preds, 
                  endemic_countries[, c("ID_0", "ID_1", "dengue")], 
                  by = c("ID_0", "ID_1"))

write_out_rds(ret, out_path, out_fl_nm)
