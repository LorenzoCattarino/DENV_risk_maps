options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "prepare_datasets", "filter_and_resample.r"),
  file.path("R", "prepare_datasets", "grid_up_foi_dataset.r"),
  file.path("R", "prepare_datasets", "average_up.r"),
  file.path("R", "utility_functions.r"))

my_pkgs <- c("data.table", "dplyr")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             packages = my_pkgs,
                             sources = my_resources)


# ---------------------------------------- define parameters 


out_pt <- file.path("output", "env_variables")

out_fl_nm <- "aggreg_pixel_level_env_vars_20km.RDS"


# ---------------------------------------- rebuild the queue


if (CLUSTER) {
  
  obj <- didehpc::queue_didehpc(ctx)

}else{
  
  context::context_load(ctx)

}

task_b_name <- "snoopy_janenschia"

pxl_job_t <- obj$task_bundle_get(task_b_name)

pxl_job <- pxl_job_t$results()


# ---------------------------------------- run


all_pixel_df <- do.call("rbind", pxl_job)

# check duplicate cell values - it is OK to have duplicate! 
# sum(duplicated(all_pixel_df[,1:4]))

# all_pixel_df$cell <- seq_len(nrow(all_pixel_df))

data_points <- all_pixel_df[all_pixel_df$type != "pseudoAbsence", ]
psAb <- all_pixel_df[all_pixel_df$type == "pseudoAbsence", ]

psAb_spl <- split(psAb, list(psAb$data_id, psAb$ADM_0, psAb$ADM_1), drop = TRUE)

lng <- lapply(psAb_spl, nrow)
       
sub_n_sample <- function(i, a){
  x <- ifelse(nrow(i) >= a, a, nrow(i))
  ids <- sample(seq_len(nrow(i)), x)
  i[ids,]
}
  
psAb_spl_smp <- lapply(psAb_spl, sub_n_sample, 215)

psAbs_square <- do.call("rbind", psAb_spl_smp)

all_pixel_df_2 <- rbind(data_points, psAbs_square)
  
write_out_rds(all_pixel_df_2, out_pt, out_fl_nm)
