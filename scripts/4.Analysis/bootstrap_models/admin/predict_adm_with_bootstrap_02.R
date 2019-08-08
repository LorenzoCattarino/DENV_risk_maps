# Gets only endemic countries 

source(file.path("R", "utility_functions.R"))
source(file.path("R", "create_parameter_list.R"))

library(dplyr)


# define extra parameters -----------------------------------------------------


extra_prms <- list(id = 4,
                   ID_0_to_remove = c(1, 69, 171, 122, 200, 224, 226, 235, 236, 244, 246))
                   
                   
# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

model_type <- paste0("model_", parameters$id)

out_fl_nm <- "response_endemic.rds"

out_path <- file.path("output", 
                      "predictions_world",
                      "bootstrap_models",
                      model_type,
                      "adm_1")

ID_0_to_remove <- parameters$ID_0_to_remove


# load data -------------------------------------------------------------------


sqr_preds <- readRDS(file.path("output", 
                               "predictions_world",
                               "bootstrap_models",
                               model_type,
                               "adm_1",
                               "response.rds"))

endemic_ID_0_ID_1 <- read.csv(file.path("output", 
                                        "datasets", 
                                        "dengue_endemic_ID_0_ID_1.csv"),
                              stringsAsFactors = FALSE)


# -----------------------------------------------------------------------------


endemic_ID_0_ID_1 <- endemic_ID_0_ID_1[!endemic_ID_0_ID_1$ID_0 %in% ID_0_to_remove,]

ret <- inner_join(sqr_preds, endemic_ID_0_ID_1[, c("ID_0", "ID_1")], by = c("ID_0", "ID_1"))

write_out_rds(ret, out_path, out_fl_nm)
