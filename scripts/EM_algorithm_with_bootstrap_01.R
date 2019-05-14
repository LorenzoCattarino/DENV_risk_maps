# Filters the global 20 km square dataset based on each bootstrap sample 
# of the original foi dataset

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "prepare_datasets", "filter_all_squares_by_bsample.R"),
  file.path("R", "utility_functions.R"))

my_pkgs <- c("data.table", "dplyr")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             packages = my_pkgs,
                             sources = my_resources)


# define parameters ----------------------------------------------------------- 


parameters <- list(
  id = 1,
  shape_1 = 0,
  shape_2 = 5,
  shape_3 = 1e6,
  all_wgt = 1,
  dependent_variable = "FOI",
  pseudoAbs_value = -0.02,
  grid_size = 1 / 120,
  no_predictors = 9,
  resample_grid_size = 20,
  foi_offset = 0.03,
  no_trees = 500,
  min_node_size = 20,
  no_samples = 200,
  EM_iter = 10) 

join_fields <- c("unique_id", "data_id", "ID_0", "ID_1")


# define variables ------------------------------------------------------------


model_type <- paste0("model_", parameters$id)

no_samples <- parameters$no_samples

my_dir <- paste0("grid_size_", parameters$grid_size)

out_pt <- file.path("output", 
                    "EM_algorithm",
                    "bootstrap_models",
                    model_type, 
                    "env_variables", 
                    "boot_samples")

out_fl_nm_all <- paste0("sample_", seq_len(no_samples), ".rds")


# are you using the cluster? -------------------------------------------------- 


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "20Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  
}


# load data ------------------------------------------------------------------- 


boot_samples <- readRDS(file.path("output", 
                                  "EM_algorithm",
                                  "bootstrap_models",
                                  my_dir, 
                                  "bootstrap_samples.rds"))

all_sqr_covariates <- readRDS(file.path("output", 
                                        "env_variables", 
                                        "all_squares_env_var_0_1667_deg.rds"))


# submit one test job --------------------------------------------------------- 


# t <- obj$enqueue(
#   filter_all_squares_by_bsample(
#     seq_len(no_samples)[1],
#     boot_samples = boot_samples,
#     all_squares = all_sqr_covariates,
#     jn_flds = join_fields,
#     out_file_path = out_pt,
#     out_file_name = out_fl_nm_all))


# submit all jobs ------------------------------------------------------------- 


if (CLUSTER) {

  pxl_jobs <- queuer::qlapply(
    seq_len(no_samples),
    filter_all_squares_by_bsample,
    obj,
    boot_samples = boot_samples,
    all_squares = all_sqr_covariates,
    jn_flds = join_fields,
    out_file_path = out_pt,
    out_file_name = out_fl_nm_all)

} else {

  pxl_jobs <- lapply(
    seq_len(no_samples)[1],
    filter_all_squares_by_bsample,
    boot_samples = boot_samples,
    all_squares = all_sqr_covariates,
    jn_flds = join_fields,
    out_file_path = out_pt,
    out_file_name = out_fl_nm_all)

}
