# Filters the global 20 km square dataset based on each bootstrap sample 
# of the original foi dataset

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "utility_functions.R"),
  file.path("R", "create_parameter_list.R"),
  file.path("R", "prepare_datasets", "set_pseudo_abs_weights.R"),
  file.path("R", "random_forest", "preprocess.R"),
  file.path("R", "random_forest", "wrapper_functions_for_boot_analysis.R"))

my_pkgs <- "dplyr"

context::context_log_start()
ctx <- context::context_save(path = "context",
                             packages = my_pkgs,
                             sources = my_resources)


# define parameters ----------------------------------------------------------- 


extra_prms <- list(id = 29,
                   dependent_variable = "FOI",
                   id_fld = "unique_id",
                   grp_flds = c("unique_id", "ID_0", "ID_1"))


# are you using the cluster? -------------------------------------------------- 


if (CLUSTER) {
  
  obj <- didehpc::queue_didehpc(ctx)
  
} else {
  
  context::context_load(ctx)
  
}


# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

model_type <- paste0("model_", parameters$id)

no_samples <- parameters$no_samples

my_dir <- paste0("grid_size_", parameters$grid_size)

out_pth_1 <- file.path("output", 
                       "EM_algorithm", 
                       "bootstrap_models", 
                       model_type,
                       "adm_foi_data",
                       "boot_samples")

out_pth_2 <- file.path("output", 
                       "EM_algorithm",
                       "bootstrap_models",
                       model_type, 
                       "env_variables", 
                       "boot_samples")


# load data ------------------------------------------------------------------- 


boot_samples <- readRDS(file.path("output", 
                                  "EM_algorithm",
                                  "bootstrap_models",
                                  my_dir, 
                                  "bootstrap_samples.rds"))

all_sqr_covariates <- readRDS(file.path("output", 
                                        "env_variables", 
                                        "all_squares_env_var_0_1667_deg.rds"))

foi_dataset <- read.csv(file.path("output", 
                                  "foi", 
                                  "All_FOI_estimates_and_predictors.csv"),
                        stringsAsFactors = FALSE) 


# preprocess full original foi data set and save it ---------------------------


foi_dataset_2 <- preprocess_adm_data(parameters, foi_dataset)

write_out_rds(foi_dataset_2, 
              file.path("output", 
                        "EM_algorithm", 
                        "bootstrap_models", 
                        model_type,
                        "adm_foi_data"),
              "adm_foi_data.rds")


# submit one test job --------------------------------------------------------- 


# t <- obj$enqueue(
#   get_bsample_and_preprocess(
#     seq_len(no_samples)[1],
#     parms = parameters,
#     boot_samples = boot_samples,
#     all_squares = all_sqr_covariates,
#     out_file_path_1 = out_pth_1,
#     out_file_path_2 = out_pth_2))


# submit all jobs ------------------------------------------------------------- 


if (CLUSTER) {

  pxl_jobs <- queuer::qlapply(
    seq_len(no_samples),
    get_bsample_and_preprocess,
    obj,
    parms = parameters,
    boot_samples = boot_samples,
    all_squares = all_sqr_covariates,
    out_file_path_1 = out_pth_1,
    out_file_path_2 = out_pth_2)

} else {

  pxl_jobs <- lapply(
    seq_len(no_samples)[1],
    get_bsample_and_preprocess,
    parms = parameters,
    boot_samples = boot_samples,
    all_squares = all_sqr_covariates,
    out_file_path_1 = out_pth_1,
    out_file_path_2 = out_pth_2)

}
