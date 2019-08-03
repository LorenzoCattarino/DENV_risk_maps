# Gets only endemic countries 

source(file.path("R", "utility_functions.R"))
source(file.path("R", "create_parameter_list.R"))

library(rgdal)
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


adm_shp <- readOGR(dsn = file.path("output", "shapefiles"), 
                   layer = paste0("gadm28_adm1_dengue"),
                   stringsAsFactors = FALSE,
                   integer64 = "allow.loss")

sqr_preds <- readRDS(file.path("output", 
                               "predictions_world",
                               "bootstrap_models",
                               model_type,
                               "adm_1",
                               "response.rds"))


# -----------------------------------------------------------------------------


adm_shp_2 <- adm_shp[!duplicated(data.frame(adm_shp)[,c("ID_0", "ID_1")]),]

endemic_countries <- subset(adm_shp_2@data, dengue == 1)

ret <- inner_join(sqr_preds, 
                  endemic_countries[, c("ID_0", "ID_1", "dengue")], 
                  by = c("ID_0", "ID_1"))

write_out_rds(ret, out_path, out_fl_nm)
