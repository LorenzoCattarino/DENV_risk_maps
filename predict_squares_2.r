# Finds which tiles contain, after resampling and subsetting, no 1 km pixel

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "prepare_datasets", "remove_NA_rows.r"),
  file.path("R", "prepare_datasets", "find_NA_pixel_tiles.R"))

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
predictor_rank <- read.csv(
  file.path("output", 
            "variable_selection", 
            "metropolis_hastings", 
            "exp_1", 
            "variable_rank_final_fits_exp_1.csv"),
  stringsAsFactors = FALSE)

# tiles info 
tile_summary <- read.csv(
  file.path("data", 
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
 

# t <- obj$enqueue(find_tiles_with_all_NA_pred_values(
#   seq_along(tile_ids)[1],
#   ids_vec = tile_ids,
#   sel_preds = best_predictors))


# ---------------------------------------- submit all jobs


if (CLUSTER) {
  
  find_NA_tls <- queuer::qlapply(
    seq_along(tile_ids), 
    find_tiles_with_all_NA_pred_values, 
    obj,
    ids_vec = tile_ids,
    sel_preds = best_predictors)
  
} else {
  
  find_NA_tls <- lapply(
    seq_along(tile_ids)[245],
    find_tiles_with_all_NA_pred_values,
    ids_vec = tile_ids,
    sel_preds = best_predictors)
  
}

NA_jobs <- which(all_tiles$results() == TRUE)

NA_pixel_tiles <- tile_summary$tile.id[NA_jobs]

out_df <- data.frame(job_id = NA_jobs, tile_id = NA_pixel_tiles)

write.table(out_df, 
            file.path("output", "datasets", "NA_pixel_tiles_20km.txt"), 
            sep = ",", 
            row.names = FALSE)
