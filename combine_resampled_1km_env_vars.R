options(didewin.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "prepare_datasets", "wrapper_to_create_pixel_foi_dts.R"),
  file.path("R", "prepare_datasets", "grid_up_foi_dataset.R"),
  file.path("R", "prepare_datasets", "average_up.R"),
  file.path("R", "utility_functions.R"))

my_pkgs <- c("data.table", "dplyr")

context::context_log_start()
ctx <- context::context_save(packages = my_pkgs,
                             sources = my_resources,
                             root = "context")


# ---------------------------------------- define parameters 


out_pt <- file.path(
  "output", 
  "env_variables")

out_fl_nm <- "aggreg_pixel_level_env_vars_20km.RDS"


# ---------------------------------------- rebuild the queue


if (CLUSTER) {
  
  obj <- didewin::queue_didewin(ctx)

}else{
  
  context::context_load(ctx)

}

task_b_name <- "ladylike_skua"

pxl_job_t <- obj$task_bundle_get(task_b_name)

pxl_job <- pxl_job_t$results()


# ---------------------------------------- run


all_pixel_df <- do.call("rbind", pxl_job)

# check duplicate cell values - it is OK to have duplicate! 
# sum(duplicated(all_pixel_df[,1:3]))

all_pixel_df$cell <- seq_len(nrow(all_pixel_df))

write_out_rds(dat = all_pixel_df, 
              my_path = out_pt,
              file_name = out_fl_nm)
