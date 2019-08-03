
library(rgdal)
library(dplyr)

source(file.path("R", "utility_functions.R"))
source(file.path("R", "create_parameter_list.R"))


# define extra parameters -----------------------------------------------------


extra_prms <- list(id = 4,
                   dependent_variable = "FOI")
                   
                   
# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

var_to_fit <- parameters$dependent_variable

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


endemic_countries <- subset(adm_shp@data, dengue == 1)

ret <- inner_join(sqr_preds, 
                  endemic_countries[, c("ID_0", "ID_1", "dengue")], 
                  by = c("ID_0", "ID_1"))

write_out_rds(ret, out_path, out_fl_nm)
