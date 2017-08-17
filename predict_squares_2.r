# Finds which tiles contain, after resampling and subsetting, no squares

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- FALSE

my_resources <- c(
  file.path("R", "prepare_datasets", "remove_NA_rows.r"),
  file.path("R", "prepare_datasets", "find_NA_pixel_tiles.R"))

my_pkgs <- "data.table"

lcf <- provisionr::package_sources(local = file.path("R_binaries", "data.table_1.10.5.zip"))
context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             package_sources = lcf)


# ---------------------------------------- define parameters 


in_pth <- file.path("output", "env_variables", "all_sets_0_1667_deg")

          
# ------------------------------------- Are you using the cluster?


if(CLUSTER) {
  
  obj <- didehpc::queue_didehpc(ctx)
  
}else{
  
  context::context_load(ctx)
  
}


# ---------------------------------------- Load data


# tiles info 
tile_summary <- read.csv(
  file.path("data", 
            "env_variables", 
            "plus60minus60_tiles.csv"), 
  header = TRUE, 
  stringsAsFactors = FALSE)


# ---------------------------------------- pre processing


# get tile ids
tile_ids <- tile_summary$tile.id


# ----------------------------------------  submit one job
 

# t <- obj$enqueue(find_tiles_with_all_NA_pred_values(
#   seq_along(tile_ids)[185],
#   ids_vec = tile_ids,
#   in_path = in_pth))


# ---------------------------------------- submit all jobs


if (CLUSTER) {

  find_NA_tls <- queuer::qlapply(
    seq_along(tile_ids),
    find_tiles_with_all_NA_pred_values,
    obj,
    ids_vec = tile_ids,
    in_path = in_pth)

} else {

  find_NA_tls <- lapply(
    seq_along(tile_ids)[185],
    find_tiles_with_all_NA_pred_values,
    ids_vec = tile_ids,
    in_path = in_pth)

}

NA_jobs <- which(find_NA_tls$results() == TRUE)

NA_pixel_tiles <- tile_summary$tile.id[NA_jobs]

out_df <- data.frame(job_id = NA_jobs, tile_id = NA_pixel_tiles)

write.table(out_df, 
            file.path("output", "datasets", "NA_pixel_tiles_20km.txt"), 
            sep = ",", 
            row.names = FALSE)
