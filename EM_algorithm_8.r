# Creates a data frame with:  
#
# 1) admin unit observations
# 2) admin unit predictions 
# 3) population weighted average of the square predictions, within the observation's admin unit
# 4) population weighted average of the 1 km pixel predictions, within the observation's admin unit

options(didehpc.cluster = "fi--didemrchnb")

my_resources <- c(
  file.path("R", "utility_functions.r"),
  file.path("R", "prepare_datasets", "average_up.r"),
  file.path("R", "prepare_datasets", "remove_NA_rows.R"),
  file.path("R", "random_forest", "functions_for_fitting_h2o_RF_and_making_predictions.r"),
  file.path("R", "random_forest", "load_predict_filter.r"))

my_pkgs <- c("h2o", "dplyr", "data.table")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# ---------------------------------------- define parameters 


model_type <- "best_model_20km_3c"

var_to_fit <- "R0_1"

pseudoAbsence_value <- 0.5

grp_flds <- c("ADM_0", "ADM_1", "data_id")

RF_obj_nm <- paste0("RF_obj.rds")

out_name <- paste0("all_scale_predictions.rds")

RF_obj_path <- file.path(
  "output",
  "EM_algorithm",
  model_type,
  "optimized_model_objects")

out_pt <- file.path(
  "output",
  "EM_algorithm",
  model_type,
  "predictions_data")


# ---------------------------------------- are you using the cluster? 


context::context_load(ctx)


# ---------------------------------------- load data 


foi_dataset <- read.csv(
  file.path("output", "foi", "All_FOI_estimates_linear_env_var_area.csv"),
  stringsAsFactors = FALSE) 

sqr_dataset <- readRDS(
  file.path("output",
            "EM_algorithm",
            "env_variables",
            "env_vars_20km.rds"))

adm_dataset <- read.csv(  
  file.path("output",
            "env_variables",
            "All_adm1_env_var.csv"),
  header = TRUE,
  sep = ",", 
  stringsAsFactors = FALSE)

# predicting variable rank
predictor_rank <- read.csv(
  file.path("output", 
            "variable_selection", 
            "metropolis_hastings", 
            "exp_1", 
            "variable_rank_final_fits_exp_1.csv"),
  stringsAsFactors = FALSE)

# tiles
tile_summary <- read.csv(
  file.path("data", 
            "env_variables", 
            "plus60minus60_tiles.csv"), 
  header = TRUE, 
  sep = ",", 
  stringsAsFactors = FALSE)

# NA pixel tiles 
NA_pixel_tiles <- read.table(
  file.path("output", 
            "datasets", 
            "NA_pixel_tiles_20km.txt"), 
  sep = ",", 
  header = TRUE)

all_sqr_predictions <- readRDS(
  file.path("output",
            "EM_algorithm",
            model_type,
            "square_predictions_all_data.rds"))


# -------------------------------------- process the original data  


names(foi_dataset)[names(foi_dataset) == var_to_fit] <- "o_j"
names(foi_dataset)[names(foi_dataset) == "ID_0"] <- grp_flds[1]
names(foi_dataset)[names(foi_dataset) == "ID_1"] <- grp_flds[2]

foi_dataset[foi_dataset$type == "pseudoAbsence", "o_j"] <- pseudoAbsence_value


# ---------------------------------------- pre process admin predictions


adm_dataset <- adm_dataset[!duplicated(adm_dataset[, c("ID_0", "ID_1")]), ]


# ---------------------------------------- create some objects 


tile_ids <- tile_summary$tile.id

NA_pixel_tile_ids <- NA_pixel_tiles$tile_id

tile_ids_2 <- tile_ids[!tile_ids %in% NA_pixel_tile_ids]  

my_predictors <- predictor_rank$variable[1:9]

#my_predictors <- c(my_predictors, "RFE_const_term")


# ---------------------------------------- submit one job 


h2o.init()

RF_obj <- h2o.loadModel(file.path(RF_obj_path, RF_obj_nm))

names(adm_dataset)[names(adm_dataset) == "ID_0"] <- grp_flds[1]
names(adm_dataset)[names(adm_dataset) == "ID_1"] <- grp_flds[2] 

adm_dataset_2 <- remove_NA_rows(adm_dataset, my_predictors)

adm_dataset_2$admin <- make_h2o_predictions(RF_obj, adm_dataset_2, my_predictors)

h2o.shutdown(prompt = FALSE)

fltr_adm <- inner_join(
  adm_dataset_2, 
  foi_dataset[, grp_flds])


# -------------------------------------- process square predictions


sqr_preds <- all_sqr_predictions

sqr_dataset <- cbind(sqr_dataset[, c(grp_flds, "population")],
                     square = sqr_preds)

average_sqr <- average_up(
  pxl_df = sqr_dataset,
  grp_flds = grp_flds,
  var_names = "square")


# -------------------------------------- process 1 km predictions


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


# -------------------------------------- join admin, square and pixel level predictions


df_lst <- list(foi_dataset[, c(grp_flds, "type", "o_j")],
               fltr_adm[, c(grp_flds, "admin")],
               average_sqr[, c(grp_flds, "square")])#,
#average_pxl[, c(grp_fields, "mean_pxl_pred")]) 

join_all <- Reduce(function(...) left_join(...), df_lst)


# -------------------------------------- save 


write_out_rds(join_all, out_pt, out_name)

