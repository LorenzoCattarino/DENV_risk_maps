rm(list = ls())

my_resources <- c(
  file.path("R", "prepare_datasets", "append_ISO_codes.R"))

my_pkgs <- c("data.table")

CLUSTER <- FALSE

if(CLUSTER) {
  
  # Load packages
  library(context)
  library(queuer)
  library(didewin)
  
  #my_workdir <- "Z:/Data/processed/fullres/tiles"
  
  didewin::didewin_config_global(cluster = "fi--didemrchnb")
  
  #root <- file.path(my_workdir, "context")
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


# tiles info 
tile_summary <- read.csv("plus60minus60_tiles.csv", 
                         header = TRUE, 
                         sep = ",", 
                         stringsAsFactors = FALSE)


# ---------------------------------------- send tile jobs


tile_ids <- tile_summary$tile.id

if (CLUSTER) {
  
  all_tiles <- queuer::qlapply(
    seq_along(tile_ids), 
    append_ISO_codes, 
    obj,
    ids_vec = tile_ids,
    timeout = 0)
  
} else {
  
  all_tiles <- lapply(
    seq_along(tile_ids)[280],
    append_ISO_codes,
    ids_vec = tile_ids)
  
}
