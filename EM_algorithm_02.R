# Loads filtered and resampled tiles, and rbind them together
# Also removed NA squares, assigned ID and renamed cell field

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "prepare_datasets", "filter_and_resample.R"),
  file.path("R", "prepare_datasets", "clean_and_average.R"),
  file.path("R", "prepare_datasets", "remove_NA_rows.R"),
  file.path("R", "prepare_datasets", "grid_up.R"),
  file.path("R", "prepare_datasets", "average_up.R"),
  file.path("R", "utility_functions.R"))

my_pkgs <- c("data.table", "dplyr")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             packages = my_pkgs,
                             sources = my_resources)


# define parameters -----------------------------------------------------------  


parameters <- list(
  resample_grid_size = 20,
  no_predictors = 9)   

out_pt <- file.path("output", "EM_algorithm", "best_fit_models", "env_variables")

out_fl_nm <- "env_vars_20km_4.rds"


# rebuild the queue ----------------------------------------------------------- 


if (CLUSTER) {
  
  obj <- didehpc::queue_didehpc(ctx)

} else {
  
  context::context_load(ctx)

}

all_tasks <- obj$task_bundle_info()

task_b_name <- all_tasks[nrow(all_tasks), "name"]

pxl_job_t <- obj$task_bundle_get(task_b_name)

pxl_job <- pxl_job_t$results()


# run ------------------------------------------------------------------------- 


all_pixel_df <- do.call("rbind", pxl_job)

# check duplicate cell values - it is OK to have duplicate! 
# sum(duplicated(all_pixel_df[,1:4]))

# assign cell ID
all_pixel_df$cell <- seq_len(nrow(all_pixel_df))

all_pixel_df$log_pop_den <- log(1 + all_pixel_df$pop_den) 

# rename 
names(all_pixel_df)[names(all_pixel_df) == "cell"] <- "square"

write_out_rds(all_pixel_df, out_pt, out_fl_nm)
