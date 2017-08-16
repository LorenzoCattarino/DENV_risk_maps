# Resamples all the 1 km pixels, in each tile, to squares with a coarser resolution

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "prepare_datasets", "resample.R"),
  file.path("R", "prepare_datasets", "grid_up_foi_dataset.R"),
  file.path("R", "prepare_datasets", "remove_NA_rows.r"),
  file.path("R", "prepare_datasets", "average_up.R"))

my_pkgs <- c("data.table", "dplyr")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# ---------------------------------------- define parameters


in_pt <- file.path("data", "env_variables", "all_sets_gadm_codes")

group_fields <- c("cell", "lat.grid", "long.grid")

gr_size <- 20

new_res <- (1 / 120) * gr_size

out_pt <- file.path(
"output",
"env_variables",
"all_sets_0_1667_deg")


# ---------------------------------------- are you using the cluster? 


if (CLUSTER) {
  
  obj <- didehpc::queue_didehpc(ctx)
  
} else {
  
  context::context_load(ctx)
  
}


# ---------------------------------------- load data 


all_predictors <- read.table(
  file.path("output", 
            "datasets", 
            "all_predictors.txt"), 
  header = TRUE, 
  stringsAsFactors = FALSE)


# ---------------------------------------- pre processing


var_names <- all_predictors$variable

fi <- list.files(in_pt, 
                 pattern = "^tile",
                 full.names = TRUE)


# ---------------------------------------- submit one job


t <- obj$enqueue(
  resample(fi[1],
  grp_flds = group_fields,
  grid_size = new_res,
  env_var_names = var_names,
  out_path = out_pt))


# ---------------------------------------- submit all jobs


# if (CLUSTER) {
# 
#   resample_tiles <- queuer::qlapply(
#     fi,
#     resample,
#     obj,
#     grp_flds = group_fields,
#     grid_size = new_res,
#     env_var_names = var_names,
#     out_path = out_pt)
# 
# }else{
# 
#   resample_tiles <- lapply(
#     fi[135],
#     resample,
#     grp_flds = group_fields,
#     grid_size = new_res,
#     env_var_names = var_names,
#     out_path = out_pt)
# 
# }
