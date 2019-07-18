# Creates a data frame with:  
#
# 1) admin unit observations
# 2) admin unit predictions 
# 3) population weighted average of the square predictions, within the observation's admin unit
# 4) population weighted average of the 1 km pixel predictions, within the observation's admin unit

library(ranger)
library(dplyr)

source(file.path("R", "prepare_datasets", "average_up.R"))
source(file.path("R", "prepare_datasets", "remove_NA_rows.R"))
source(file.path("R", "random_forest", "fit_ranger_RF_and_make_predictions.R"))
source(file.path("R", "utility_functions.R"))
source(file.path("R", "create_parameter_list.R"))


# define parameters -----------------------------------------------------------


extra_prms <- list(id = 15,
                   dependent_variable = "FOI",
                   no_predictors = 26,
                   id_fld = "data_id")   

grp_flds <- c("ID_0", "ID_1", "data_id")

out_name <- "all_scale_predictions.rds"


# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

model_id <- parameters$id

var_to_fit <- parameters$dependent_variable
  
foi_offset <- parameters$foi_offset

pseudoAbs_value <- parameters$pseudoAbs_value[var_to_fit]

model_type <- paste0("model_", model_id)

RF_obj_path <- file.path("output",
                         "EM_algorithm",
                         "best_fit_models",
                         model_type,
                         "optimized_model_objects")

out_pt <- file.path("output",
                    "EM_algorithm",
                    "best_fit_models",
                    model_type,
                    "predictions_data")

covariates_dir <- parameters$covariates_dir

id_field <- parameters$id_fld


# load data ------------------------------------------------------------------- 


foi_dataset <- readRDS(file.path("output", 
                                 "EM_algorithm", 
                                 "best_fit_models", 
                                 model_type,
                                 "adm_foi_data",
                                 "adm_foi_data.rds"))  

sqr_dataset <- readRDS(file.path("output",
                                 "EM_algorithm",
                                 "best_fit_models",
                                 model_type,
                                 "env_variables",
                                 "env_vars_20km.rds"))

adm_dataset <- read.csv(file.path("output",
                                  "env_variables",
                                  "All_adm1_env_var.csv"),
                        stringsAsFactors = FALSE)

predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection",
                                     covariates_dir,
                                     "predictor_rank.csv"), 
                           stringsAsFactors = FALSE)

tile_summary <- read.csv(file.path("data", 
                                   "env_variables", 
                                   "plus60minus60_tiles.csv"), 
                         stringsAsFactors = FALSE)

NA_pixel_tiles <- read.table(file.path("output", 
                                       "datasets", 
                                       "NA_pixel_tiles_20km.txt"), 
                             sep = ",", 
                             header = TRUE)

all_sqr_predictions <- readRDS(file.path("output",
                                         "EM_algorithm",
                                         "best_fit_models",
                                         model_type,
                                         "square_predictions_all_data.rds"))

RF_obj <- readRDS(file.path(RF_obj_path, "RF_obj.rds"))


# pre processing --------------------------------------------------------------


names(foi_dataset)[names(foi_dataset) == var_to_fit] <- "o_j"

foi_dataset[foi_dataset$type == "pseudoAbsence", "o_j"] <- pseudoAbs_value

tile_ids <- tile_summary$tile.id

NA_pixel_tile_ids <- NA_pixel_tiles$tile_id

tile_ids_2 <- tile_ids[!tile_ids %in% NA_pixel_tile_ids]  

my_predictors <- predictor_rank$name[1:parameters$no_predictors]


# run ------------------------------------------------------------------------- 


adm_pred <- make_ranger_predictions(RF_obj, adm_dataset, my_predictors)

if(var_to_fit == "FOI"){
  
  foi_dataset$o_j <- foi_dataset$o_j - foi_offset
  adm_pred <- adm_pred - foi_offset
  all_sqr_predictions <- all_sqr_predictions - foi_offset

}

if(var_to_fit == "Z"){
  
  foi_dataset$o_j <- foi_dataset$o_j * foi_dataset$birth_rate * 35
  adm_pred <- (adm_pred - foi_offset) * adm_dataset$birth_rate * 35
  all_sqr_predictions <- (all_sqr_predictions - foi_offset) * sqr_dataset$birth_rate * 35

}

sqr_preds <- all_sqr_predictions

sqr_dataset_2 <- cbind(sqr_dataset,
                       p_i = sqr_preds)

adm_dataset$admin <- adm_pred
  
fltr_adm <- inner_join(adm_dataset, foi_dataset[, grp_flds])

average_sqr <- average_up(pxl_df = sqr_dataset_2,
                          grp_flds = grp_flds,
                          var_names = "p_i")

average_sqr <- rename(average_sqr, mean_p_i = p_i)

df_lst <- list(foi_dataset[, c(grp_flds, "type", "o_j", "new_weight")],
               fltr_adm[, c(grp_flds, "admin")],
               average_sqr[, c(grp_flds, "mean_p_i")])

join_all <- Reduce(function(...) left_join(...), df_lst)

join_all_2 <- fitted_sero_cell_to_adm(join_all, 
                                      sqr_dataset_2, 
                                      c(id_field, "p_i"), 
                                      c(grp_flds, "type", "new_weight", "o_j", "admin", "mean_p_i"))
  
write_out_rds(join_all_2, out_pt, out_name)  
