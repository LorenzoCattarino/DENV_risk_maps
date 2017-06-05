library(data.table)

# in_pred_path <- file.path(
#   "output", 
#   "dengue_dataset", 
#   "predictions", 
#   "tile_sets_0_0083_deg_3")

in_pred_path <- file.path(
  "data",
  "gadm_codes")

fi <- list.files(in_pred_path, 
                 pattern = "^tile",
                 full.names = TRUE)

tile_280 <- fread(
  fi[131],
  header = TRUE, 
  sep = ",",              
  na.strings = c("NA", "-1.#IND", "Peipsi", "Moskva", "IJsselmeer", "Zeeuwse meren"),
  fill = TRUE, 
  data.table = FALSE)

tile_316 <- fread(
  fi[158],
  header = TRUE, 
  sep = ",",              
  na.strings = c("NA", "-1.#IND", "Peipsi", "Moskva", "IJsselmeer", "Zeeuwse meren"),
  fill = TRUE, 
  data.table = FALSE)

pop_280 <- sum(tile_280[tile_280$ADM_0==105 & tile_280$ADM_1==1, "population"], na.rm = TRUE)
pop_316 <- sum(tile_316[tile_316$ADM_0==105 & tile_316$ADM_1==1, "population"], na.rm = TRUE)
pop_316 + pop_280


# predicting variable rank
predictor_rank <- read.csv(
  file.path("output", 
            "dengue_dataset", 
            "variable_selection", 
            "metropolis_hastings", 
            "exp_1", 
            "variable_rank_final_fits_exp_1.csv"),
  stringsAsFactors = FALSE)
# get the vector of best predictors (from MH variable selection routine)
best_predictors <- predictor_rank$variable[1:9]
source(file.path("R", "prepare_datasets", "remove_NA_rows.R"))


tile_280_sub <- remove_NA_rows(tile_280, best_predictors)
tile_316_sub <- remove_NA_rows(tile_316, best_predictors)
pop_280 <- sum(tile_280_sub[tile_280_sub$ADM_0==105 & tile_280_sub$ADM_1==1, "population"], na.rm = TRUE)
pop_316 <- sum(tile_316_sub[tile_316_sub$ADM_0==105 & tile_316_sub$ADM_1==1, "population"], na.rm = TRUE)
pop_316 + pop_280
