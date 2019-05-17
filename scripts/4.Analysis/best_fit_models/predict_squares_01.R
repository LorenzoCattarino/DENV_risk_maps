# Makes predictions for all the squares in the world.

library(ranger)

source(file.path("R", "random_forest", "fit_ranger_RF_and_make_predictions.R"))
source(file.path("R", "plotting", "functions_for_plotting_raster_maps.R"))
source(file.path("R", "utility_functions.R"))
source(file.path("R", "create_parameter_list.R"))


# define parameters ----------------------------------------------------------- 


extra_prms <- list(id = 13,
                   dependent_variable = "Z",
                   no_predictors = 26)   

RF_mod_name <- "RF_obj.rds"

base_info <- c("cell", "latitude", "longitude", "population", "ID_0", "ID_1", "ID_2")

extra_predictors <- NULL


# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

model_type <- paste0("model_", parameters$id)

var_to_fit <- parameters$dependent_variable

foi_offset <- parameters$foi_offset

out_pt <- file.path("output", "predictions_world", "best_fit_models", model_type)
  
out_fl_nm <- "response.rds"


# load data ------------------------------------------------------------------- 


all_sqr_covariates <- readRDS(file.path("output", 
                                        "env_variables", 
                                        "all_squares_env_var_0_1667_deg.rds"))

RF_obj_path <- file.path("output",
                         "EM_algorithm",
                         "best_fit_models",
                         model_type,
                         "optimized_model_objects")

predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection",
                                     "stepwise_v3",
                                     "predictor_rank.csv"), 
                           stringsAsFactors = FALSE)


# get best predictor ---------------------------------------------------------- 


my_predictors <- predictor_rank$name[1:parameters$no_predictors]
my_predictors <- c(my_predictors, extra_predictors)


# submit one job -------------------------------------------------------------- 


RF_obj <- readRDS(file.path(RF_obj_path, RF_mod_name))

p_i <- make_ranger_predictions(RF_obj, all_sqr_covariates, my_predictors)

if(var_to_fit == "FOI"){
  
  p_i <- p_i - foi_offset

}

#### added on 17052019 - but it needs to go before (e.g. to assemble_foi_data_set_x)
mean_age_data <- read.csv(file.path("output",
                                    "datasets",
                                    "country_age_structure_mean.csv"),
                          stringsAsFactors = FALSE)
all_sqr_covariates <- cbind(all_sqr_covariates, p_i = p_i)
library(dplyr)
all_sqr_covariates <- inner_join(all_sqr_covariates, mean_age_data[, c("ID_0", "mean_age", "sd_age")])
####

if(var_to_fit == "Z"){
  
  all_sqr_covariates$p_i <- (all_sqr_covariates$p_i - foi_offset) * all_sqr_covariates$mean_age
  
}

all_sqr_covariates$p_i[all_sqr_covariates$p_i < 0] <- 0

world_sqr_preds <- cbind(all_sqr_covariates[, c(base_info, "mean_age", "sd_age")], best = all_sqr_covariates$p_i)

write_out_rds(world_sqr_preds, out_pt, out_fl_nm)  
