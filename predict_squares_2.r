# Finds which tiles contain, after resampling and subsetting, no 1 km pixel

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "random_forest", "remove_NA_rows.R"),
  file.path("R", "find_NA_pixel_tiles.R"))

my_pkgs <- c("data.table")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# ------------------------------------- Are you using the cluster?


if(CLUSTER) {
  
  obj <- didehpc::queue_didehpc(ctx)
  
}else{
  
  context::context_load(ctx)
  
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


# ----------------------------------------  submit one job
 

t <- obj$enqueue(wrapper_to_make_preds(
  seq_along(tile_ids)[69],
  ids_vec = tile_ids,
  model_lst = RF_objs,
  dide_paral = TRUE,
  sel_preds = best_predictors))


# ---------------------------------------- submit all jobs


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

write.table(out_df, 
            file.path("output", "datasets", "NA_pixel_tiles.txt"), 
            sep = ",", 
            row.names = FALSE)
