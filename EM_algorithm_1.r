# Filters each 1km tile based on the original foi dataset  
# and resamples each tile to 20km resolution

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "prepare_datasets", "filter_and_resample.r"),
  file.path("R", "prepare_datasets", "grid_up_foi_dataset.r"),
  file.path("R", "prepare_datasets", "average_up.r"))

my_pkgs <- c("data.table", "dplyr")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             packages = my_pkgs,
                             sources = my_resources)


# ---------------------------------------- define parameters


in_pt <- file.path("data", "gadm_codes")

group_fields <- c("data_id", "ADM_0", "ADM_1", "cell", "lat.grid", "long.grid")

gr_size <- 20
  
new_res <- (1 / 120) * gr_size


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

foi_data <- read.csv(
  file.path("output", "foi", "All_FOI_estimates_linear_env_var.csv"),
  stringsAsFactors = FALSE)


# ---------------------------------------- pre processing


names(foi_data)[names(foi_data) == "ID_0"] <- "ADM_0"
names(foi_data)[names(foi_data) == "ID_1"] <- "ADM_1"

var_names <- all_predictors$variable

fi <- list.files(in_pt, 
                 pattern = "^tile",
                 full.names = TRUE)


# ---------------------------------------- submit one test job


# t <- obj$enqueue(
#   wrapper_to_tiles_to_dts(
#     fi[1],
#     foi_dts = foi_data,
#     env_var_names = var_names,
#     grp_flds = group_fields,
#     grid_size = new_res))


# ---------------------------------------- submit all jobs


if (CLUSTER) {
  
  pxl_job <- queuer::qlapply(
    fi,
    filter_and_resample,
    obj,
    foi_dts = foi_data, 
    env_var_names = var_names, 
    grp_flds = group_fields, 
    grid_size = new_res)

} else {
  
  pxl_job <- lapply(
    fi[159],
    filter_and_resample,
    foi_dts = foi_data, 
    env_var_names = var_names, 
    grp_flds = group_fields, 
    grid_size = new_res)
  
}
