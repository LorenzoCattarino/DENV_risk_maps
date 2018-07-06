# Creates a data frame with:  
#
# 1) admin unit observations
# 2) admin unit predictions 
# 3) population weighted average of the square predictions, within the observation's admin unit
# 4) population weighted average of the 1 km pixel predictions, within the observation's admin unit

options(didehpc.cluster = "fi--didemrchnb")

my_resources <- c(
  file.path("R", "prepare_datasets", "average_up.R"),
  file.path("R", "prepare_datasets", "remove_NA_rows.R"),
  file.path("R", "random_forest", "fit_ranger_RF_and_make_predictions.R"),
  file.path("R", "utility_functions.R"))

my_pkgs <- c("ranger", "dplyr", "data.table")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters -----------------------------------------------------------


parameters <- list(
  dependent_variable = "FOI",
  pseudoAbs_value = -0.02,
  no_predictors = 26)   

grp_flds <- c("ID_0", "ID_1", "data_id")

RF_obj_nm <- "RF_obj.rds"

out_name <- "all_scale_predictions.rds"

foi_dts_nm <- "All_FOI_estimates_linear_env_var_area_salje.csv"

covariate_dts_nm <- "env_vars_20km.rds"

model_type_tag <- "_best_model_2"

extra_predictors <- NULL


# define variables ------------------------------------------------------------


var_to_fit <- parameters$dependent_variable
  
model_type <- paste0(var_to_fit, model_type_tag)

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


# are you using the cluster? -------------------------------------------------- 


context::context_load(ctx)


# load data ------------------------------------------------------------------- 


foi_dataset <- read.csv(file.path("output", "foi", foi_dts_nm),
                        stringsAsFactors = FALSE) 

sqr_dataset <- readRDS(file.path("output",
                                 "EM_algorithm",
                                 "best_fit_models",
                                 "env_variables",
                                 covariate_dts_nm))

adm_dataset <- read.csv(file.path("output",
                                  "env_variables",
                                  "All_adm1_env_var.csv"),
                        stringsAsFactors = FALSE)

predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection",
                                     "stepwise",
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


# pre processing --------------------------------------------------------------


names(foi_dataset)[names(foi_dataset) == var_to_fit] <- "o_j"

foi_dataset[foi_dataset$type == "pseudoAbsence", "o_j"] <- parameters$pseudoAbs_value

adm_dataset <- adm_dataset[!duplicated(adm_dataset[, c("ID_0", "ID_1")]), ]

tile_ids <- tile_summary$tile.id

NA_pixel_tile_ids <- NA_pixel_tiles$tile_id

tile_ids_2 <- tile_ids[!tile_ids %in% NA_pixel_tile_ids]  

my_predictors <- predictor_rank$name[1:parameters$no_predictors]
my_predictors <- c(my_predictors, extra_predictors)


# ---------------------------------------- submit one job 


RF_obj <- readRDS(file.path(RF_obj_path, RF_obj_nm))

adm_dataset_2 <- remove_NA_rows(adm_dataset, my_predictors)

adm_dataset_2$admin <- make_ranger_predictions(RF_obj, adm_dataset_2, my_predictors)

fltr_adm <- inner_join(adm_dataset_2, foi_dataset[, grp_flds])

sqr_preds <- all_sqr_predictions

sqr_dataset <- cbind(sqr_dataset[, c(grp_flds, "population")],
                     square = sqr_preds)

average_sqr <- average_up(pxl_df = sqr_dataset,
                          grp_flds = grp_flds,
                          var_names = "square")

# #[c(140, 141, 170, 171)]
# 
# tile_prds <- loop(
#   seq_along(tile_ids),
#   load_predict_filter,
#   ids_vec = tile_ids,
#   predictors = predictors,
#   RF_obj = RF_obj,
#   foi_dts = foi_dataset,
#   grp_flds = grp_fields,
#   parallel = FALSE)
# 
# tile_prds_rb <- do.call("rbind", tile_prds)
# 
# average_pxl <- average_up(
#   pxl_df = tile_prds_rb,
#   grp_flds = grp_fields,
#   var_names = "pred")
# 
# names(average_pxl)[names(average_pxl) == "pred"] <- "mean_pxl_pred"

df_lst <- list(foi_dataset[, c(grp_flds, "type", "o_j")],
               fltr_adm[, c(grp_flds, "admin")],
               average_sqr[, c(grp_flds, "square")])#,
#average_pxl[, c(grp_fields, "mean_pxl_pred")]) 

join_all <- Reduce(function(...) left_join(...), df_lst)

write_out_rds(join_all, out_pt, out_name)
