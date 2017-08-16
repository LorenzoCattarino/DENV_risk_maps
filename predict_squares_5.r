# Takes the averaged value across models, and confidence intervals, 
# of different measured variables (foi, R0, burden...), for each tile. 

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "utility_functions.r"),
  file.path("R", "prepare_datasets", "calculate_mean_across_fits.r"),
  file.path("R", "prepare_datasets", "wrapper_to_mean_across_fits.r"))

my_pkgs <- "data.table"

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# ---------------------------------------- define parameters


model_tp <- "boot_model_20km_cw"

no_fits <- 50

NA_tile_fl_name <- "NA_pixel_tiles_20km.txt"

bs_inf <- c("cell", "lat.grid", "long.grid", "population")

orig_in_pth  <- file.path("output", "env_variables", "all_sets_0_1667_deg")

in_pth <- file.path(
  "output", 
  "predictions_world", 
  model_tp,
  "tile_sets_0_1667_deg")

var_names <- "foi"


# ---------------------------------------- define variables


out_pth <- file.path(
  "output", 
  "predictions_world", 
  model_tp,
  "tile_sets_0_1667_deg",
  "averages")


# ---------------------------------------- are you using the cluster?


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "12and16Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  context::parallel_cluster_start(8, ctx)
  
}


# ---------------------------------------- load data


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
            NA_tile_fl_name), 
  sep = ",", 
  header = TRUE)


# ---------------------------------------- pre processing


# get tile ids
tile_ids <- tile_summary$tile.id

NA_pixel_tile_ids <- NA_pixel_tiles$tile_id

tile_ids_2 <- tile_ids[!tile_ids %in% NA_pixel_tile_ids]  


# ---------------------------------------- submit one job 


t <- obj$enqueue(
  wrapper_to_mean_across_fits(
    seq_along(tile_ids_2)[1],
    ids_vec = tile_ids_2,
    in_path = in_pth,
    orig_in_path = orig_in_pth,
    var_names = var_names,
    parallel = TRUE,
    base_info = bs_inf,
    out_path = out_pth))


# ---------------------------------------- submit all jobs


# if (CLUSTER) {
# 
#   pred_tiles <- queuer::qlapply(
#     seq_along(tile_ids_2),
#     wrapper_to_mean_across_fits,
#     obj,
#     ids_vec = tile_ids_2,
#     in_path = in_pth,
#     orig_in_path = orig_in_pth,
#     var_names = var_names,
#     parallel = TRUE,
#     base_info = bs_inf,
#     out_path = out_pth)
# 
# } else {
# 
#   pred_tiles <- lapply(
#     seq_along(tile_ids_2)[1],
#     wrapper_to_mean_across_fits,
#     ids_vec = tile_ids_2,
#     in_path = in_pth,
#     orig_in_path = orig_in_pth,
#     var_names = var_names,
#     parallel = TRUE,
#     base_info = bs_inf,
#     out_path = out_pth)
# 
# }
# 
# if (!CLUSTER) {
#   context::parallel_cluster_stop()
# }
