rm(list = ls())

# install.packages("didewin",
#                  repos=c(CRAN="https://cran.rstudio.com",
#                          drat="https://richfitz.github.io/drat"))

my_resources <- c(
  file.path("R", "random_forest", "remove_NA_rows.R"),
  file.path("R", "find_NA_pixel_tiles.R"))

my_pkgs <- c("data.table")

CLUSTER <- TRUE


# ------------------------------------- Are you using the cluster?


if(CLUSTER) {
  
  # Load packages
  library(context)
  library(queuer)
  library(didewin)
  
  didewin::didewin_config_global(cluster = "fi--didemrchnb")
  
  root <- "context"
  
  ctx <- context::context_save(packages = my_pkgs,
                               sources = my_resources,
                               root = root)
  
  obj <- didewin::queue_didewin(ctx)
  
}else{
  
  # Load packages
  sapply(my_pkgs, library, character.only = TRUE)
  
  # Load functions 
  sapply(my_resources, source)
  
}


# ---------------------------------------- Load data


# predicting variable rank
predictor_rank <- read.csv((file.path("output", 
                                      "dengue_dataset", 
                                      "predictor_importance", 
                                      "metropolis_hastings", 
                                      "exp_4", 
                                      "variable_rank_1.csv")),
                           stringsAsFactors = FALSE)

# tiles info 
tile_summary <- read.csv(file.path("data", 
                                   "env_variables", 
                                   "plus60minus60_tiles.csv"), 
                         header = TRUE, 
                         sep = ",", 
                         stringsAsFactors = FALSE)


# ---------------------------------------- pre processing


# get the vector of best predictors (from MH variable selection routine)
best_predictors <- predictor_rank$variable[1:9]

# get tile ids
tile_ids <- tile_summary$tile.id


# ---------------------------------------- send tile jobs


if (CLUSTER) {
  
  all_tiles <- queuer::qlapply(
    seq_along(tile_ids), 
    find_tiles_with_all_NA_pred_values, 
    obj,
    ids_vec = tile_ids,
    sel_preds = best_predictors,
    timeout = 0)
  
} else {
  
  all_tiles <- lapply(
    seq_along(tile_ids)[245],
    find_tiles_with_all_NA_pred_values,
    ids_vec = tile_ids,
    sel_preds = best_predictors)
  
}

NA_jobs <- which(all_tiles$results() == TRUE)

NA_pixel_tiles <- tile_summary$tile.id[NA_jobs]

out_df <- data.frame(job_id = NA_jobs, tile_id = NA_pixel_tiles)

write.table(out_df, file.path("output", "datasets", "NA_pixel_tiles.txt"), sep = ",", row.names = FALSE)

# # send one job only to the cluster
# 
# obj$enqueue(wrapper_to_make_preds(
#   seq_along(tile_ids)[69],
#   ids_vec = tile_ids,
#   model_lst = RF_objs,
#   dide_paral = TRUE,
#   sel_preds = best_predictors))
