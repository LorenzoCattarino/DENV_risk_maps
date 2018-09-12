# Filters the global 20 km square dataset based on the original foi dataset 


library(dplyr)

source(file.path("R", "utility_functions.R"))


# define parameters -----------------------------------------------------------


join_fields <- c("data_id", "ID_0", "ID_1")

out_pt <- file.path("output", "EM_algorithm", "best_fit_models", "env_variables")


# load data -------------------------------------------------------------------


foi_data <- read.csv(file.path("output", "foi", "All_FOI_estimates_and_predictors.csv"),
                     stringsAsFactors = FALSE) 

all_sqr_covariates <- readRDS(file.path("output", 
                                        "env_variables", 
                                        "all_squares_env_var_0_1667_deg.rds"))


# join ------------------------------------------------------------------------


pxl_data <- inner_join(all_sqr_covariates, foi_data[, join_fields])

write_out_rds(pxl_data, out_pt, "env_vars_20km.rds")
