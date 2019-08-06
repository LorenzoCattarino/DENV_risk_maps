# Gets only endemic countries 

source(file.path("R", "utility_functions.R"))
source(file.path("R", "create_parameter_list.R"))

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


sqr_preds <- readRDS(file.path("output", 
                               "predictions_world",
                               "bootstrap_models",
                               model_type,
                               "adm_1",
                               "response.rds"))

endemic_c <- read.csv(file.path("output", 
                                "datasets", 
                                "dengue_endemic_countries.csv"),
                      stringsAsFactors = FALSE)


# -----------------------------------------------------------------------------


ret <- inner_join(sqr_preds, endemic_c[, "ID_0", drop = FALSE], by = "ID_0")

write_out_rds(ret, out_path, out_fl_nm)
