# Filters each 1km tile based on each bootstrap sample 
# and resamples each tile to 20km resolution
# Also combines all the tiles together and save the output

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "prepare_datasets", "filter_resample_and_combine.r"),
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


no_fits <- 1
  
in_pt <- file.path("data", "gadm_codes")

group_fields <- c("data_id", "ADM_0", "ADM_1", "cell", "lat.grid", "long.grid")

gr_size <- 20

new_res <- (1 / 120) * gr_size

out_pt <- file.path("output", "EM_algorithm", "env_variables", "boot_samples")

out_fl_nm_all <- paste0("aggreg_pixel_level_env_vars_20km_sample_", seq_len(no_fits), ".rds")


# ---------------------------------------- are you using the cluster? 


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "12and16Core")
  obj <- didehpc::queue_didehpc(ctx, config)
  
} else {
  
  context::context_load(ctx)
  
}


# ---------------------------------------- load data


boot_samples <- readRDS(
  file.path("output",
            "EM_algorithm",
            "bootstrap_samples.rds"))

all_predictors <- read.table(
  file.path("output", 
            "datasets", 
            "all_predictors.txt"), 
  header = TRUE, 
  stringsAsFactors = FALSE)

predictor_rank <- read.csv(
  file.path("output", 
            "variable_selection", 
            "metropolis_hastings", 
            "exp_1", 
            "variable_rank_final_fits_exp_1.csv"),
  stringsAsFactors = FALSE)


# ---------------------------------------- pre processing


my_predictors <- predictor_rank$variable[1:9]

var_names <- all_predictors$variable

fi <- list.files(in_pt, 
                 pattern = "^tile",
                 full.names = TRUE)


# ---------------------------------------- submit one test job


# t <- obj$enqueue(
#   filter_resample_and_combine(
#     seq_along(boot_samples),
#     boot_samples = boot_samples,
#     tile_ls = fi,
#     var_names = var_names,
#     grp_flds = group_fields,
#     new_res = new_res,
#     my_preds = my_predictors,
#     out_file_path = out_pt,
#     out_file_name = out_fl_nm_all))


# ---------------------------------------- submit all jobs


if (CLUSTER) {

  pxl_jobs <- queuer::qlapply(
    seq_along(boot_samples),
    filter_resample_and_combine,
    obj,
    boot_samples = boot_samples,
    tile_ls = fi,
    var_names = var_names,
    grp_flds = group_fields,
    new_res = new_res,
    my_preds = my_predictors,
    out_file_path = out_pt,
    out_file_name = out_fl_nm_all)

} else {

  pxl_jobs <- lapply(
    seq_along(boot_samples),
    filter_resample_and_combine,
    boot_samples = boot_samples,
    tile_ls = fi,
    var_names = var_names,
    grp_flds = group_fields,
    new_res = new_res,
    my_preds = my_predictors,
    out_file_path = out_pt,
    out_file_name = out_fl_nm_all)

}
