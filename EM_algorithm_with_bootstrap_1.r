# Filters each 1km tile based on each bootstrap sample 
# and resamples each tile to 20km resolution
# Also combines all the tiles together and save the output

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "prepare_datasets", "filter_resample_and_combine.R"),
  file.path("R", "prepare_datasets", "filter_and_resample.R"),
  file.path("R", "prepare_datasets", "clean_and_resample.R"),
  file.path("R", "prepare_datasets", "grid_up.R"),
  file.path("R", "prepare_datasets", "average_up.R"),
  file.path("R", "prepare_datasets", "remove_NA_rows.R"),
  file.path("R", "utility_functions.R"))

my_pkgs <- c("data.table", "dplyr")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             packages = my_pkgs,
                             sources = my_resources)


# define parameters ----------------------------------------------------------- 


parameters <- list(
  grid_size = 0.5,
  resample_grid_size = 20,
  no_samples = 200,
  no_predictors = 9)   

parallel_2 <- TRUE

in_pt <- file.path("output", "env_variables", "all_sets_gadm_codes")

group_fields <- c("unique_id", "data_id", "ADM_0", "ADM_1")


# define variables ------------------------------------------------------------


no_samples <- parameters$no_samples

new_res <- (1 / 120) * parameters$resample_grid_size

my_dir <- paste0("grid_size_", parameters$grid_size)

out_pt <- file.path("output", 
                    "EM_algorithm",
                    "bootstrap_models",
                    my_dir, 
                    "env_variables", 
                    "boot_samples")

out_fl_nm_all <- paste0("env_vars_20km_", seq_len(no_samples), ".rds")


# are you using the cluster? -------------------------------------------------- 


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "20Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  context::parallel_cluster_start(8, ctx)
  
}


# load data ------------------------------------------------------------------- 


boot_samples <- readRDS(file.path("output", 
                                  "EM_algorithm",
                                  "bootstrap_models",
                                  my_dir, 
                                  "bootstrap_samples.rds"))

predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection",
                                     "metropolis_hastings",
                                     "exp_1",
                                     "variable_rank_final_fits_exp_1.csv"), 
                           stringsAsFactors = FALSE)


# pre processing -------------------------------------------------------------- 


my_predictors <- predictor_rank$name[1:parameters$no_predictors]

fi <- list.files(in_pt, pattern = "^tile", full.names = TRUE)


# submit one test job --------------------------------------------------------- 


# t <- obj$enqueue(
#   filter_resample_and_combine(
#     seq_len(no_samples)[1],
#     boot_samples = boot_samples,
#     tile_ls = fi,
#     grp_flds = group_fields,
#     new_res = new_res,
#     predictors = my_predictors,
#     out_file_path = out_pt,
#     out_file_name = out_fl_nm_all,
#     parallel_2 = parallel_2))


# submit all jobs ------------------------------------------------------------- 


if (CLUSTER) {

  pxl_jobs <- queuer::qlapply(
    seq_len(no_samples),
    filter_resample_and_combine,
    obj,
    boot_samples = boot_samples,
    tile_ls = fi,
    grp_flds = group_fields,
    new_res = new_res,
    predictors = my_predictors,
    out_file_path = out_pt,
    out_file_name = out_fl_nm_all,
    parallel_2 = parallel_2)

} else {

  pxl_jobs <- lapply(
    seq_len(no_samples)[1],
    filter_resample_and_combine,
    boot_samples = boot_samples,
    tile_ls = fi,
    grp_flds = group_fields,
    new_res = new_res,
    predictors = my_predictors,
    out_file_path = out_pt,
    out_file_name = out_fl_nm_all,
    parallel_2 = parallel_2)

}

if (!CLUSTER) {
  context::parallel_cluster_stop()
}
