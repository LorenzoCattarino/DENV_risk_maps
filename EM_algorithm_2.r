# Loads filtered and resampled tiles, and rbind them together
# Also removed NA squares, assigned ID and renamed cell field

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "prepare_datasets", "filter_and_resample.r"),
  file.path("R", "prepare_datasets", "grid_up_foi_dataset.r"),
  file.path("R", "prepare_datasets", "average_up.r"),
  file.path("R", "prepare_datasets", "remove_NA_rows.r"),
  file.path("R", "utility_functions.r"))

my_pkgs <- c("data.table", "dplyr")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             packages = my_pkgs,
                             sources = my_resources)


# ---------------------------------------- define parameters 


out_pt <- file.path("output", "EM_algorithm", "env_variables")

out_fl_nm <- "aggreg_pixel_level_env_vars_20km.rds"


# ---------------------------------------- load data


predictor_rank <- read.csv(
  file.path("output", 
            "variable_selection", 
            "metropolis_hastings", 
            "exp_1", 
            "variable_rank_final_fits_exp_1.csv"),
  stringsAsFactors = FALSE)


# ---------------------------------------- get the vector of best predictors


my_predictors <- predictor_rank$variable[1:9]


# ---------------------------------------- rebuild the queue


if (CLUSTER) {
  
  obj <- didehpc::queue_didehpc(ctx)

}else{
  
  context::context_load(ctx)

}

task_b_name <- "cubiform_noddy"

pxl_job_t <- obj$task_bundle_get(task_b_name)

pxl_job <- pxl_job_t$results()


# ---------------------------------------- run


all_pixel_df <- do.call("rbind", pxl_job)

# check duplicate cell values - it is OK to have duplicate! 
# sum(duplicated(all_pixel_df[,1:4]))

# assign NA to 0 covariate values 
all_pixel_df[, my_predictors][all_pixel_df[, my_predictors] == 0] <- NA

#remove records with at least one NA predictor value
all_pixel_df <- remove_NA_rows(all_pixel_df, my_predictors)

# assign cell ID
all_pixel_df$cell <- seq_len(nrow(all_pixel_df))

# rename 
names(all_pixel_df)[names(all_pixel_df) == "cell"] <- "square"

write_out_rds(all_pixel_df, out_pt, out_fl_nm)
