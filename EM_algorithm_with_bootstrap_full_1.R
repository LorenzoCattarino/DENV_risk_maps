
options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "random_forest", "fit_ranger_RF_and_make_predictions.R"),
  file.path("R", "random_forest", "exp_max_algorithm.R"),
  file.path("R", "plotting", "quick_raster_map.R"),
  file.path("R", "plotting", "generic_scatter_plot.R"),
  file.path("R", "prepare_datasets", "calculate_wgt_corr.R"),
  file.path("R", "prepare_datasets", "remove_NA_rows.R"),
  file.path("R", "prepare_datasets", "average_up.R"),
  file.path("R", "utility_functions.R"))

my_pkgs <- c("ranger", "dplyr", "fields", "ggplot2", "weights", "colorRamps")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters ----------------------------------------------------------- 


parameters <- list(
  dependent_variable = "FOI",
  pseudoAbs_value = -0.02,
  foi_offset = 0.03,
  grid_size = 5,
  no_trees = 500,
  min_node_size = 20,
  no_samples = 200,
  EM_iter = 10,
  no_predictors = 26)   

grp_flds_1 <- c("ID_0", "ID_1", "unique_id")

grp_flds_2 <- c("ID_0", "ID_1", "data_id")

no_exp <- 20


# define variables ------------------------------------------------------------  


no_samples <- parameters$no_samples

grid_size <- parameters$grid_size

my_dir <- paste0("grid_size_", grid_size)

in_path <- file.path("output", 
                     "EM_algorithm",
                     "bootstrap_models",
                     my_dir, 
                     "env_variables",
                     "boot_samples")

out_path <- file.path("output",
                      "EM_algorithm",
                      "bootstrap_models",
                      my_dir)


# start up the cluster --------------------------------------------------------


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "20Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  context::parallel_cluster_start(8, ctx)
  
}


# load data ------------------------------------------------------------------- 


foi_data <- read.csv(file.path("output", 
                               "foi", 
                               "All_FOI_estimates_and_predictors.csv"),
                     stringsAsFactors = FALSE) 

bt_samples <- readRDS(file.path("output", 
                                "EM_algorithm", 
                                "bootstrap_models", 
                                my_dir, 
                                "bootstrap_samples.rds"))

predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection",
                                     "stepwise",
                                     "predictor_rank.csv"), 
                           stringsAsFactors = FALSE)

covariates_squares <- readRDS(file.path("output", 
                                        "EM_algorithm",
                                        "best_fit_models",
                                        "env_variables", 
                                        "env_vars_20km.rds"))

covariates_adm <- read.csv(file.path("output",
                                     "env_variables",
                                     "All_adm1_env_var.csv"),
                           header = TRUE,
                           stringsAsFactors = FALSE)


# pre process -----------------------------------------------------------------


names(foi_data)[names(foi_data) == parameters$dependent_variable] <- "o_j"

foi_data[foi_data$type == "pseudoAbsence", "o_j"] <- parameters$pseudoAbs_value

all_predictors <- predictor_rank$name

test <- expand.grid(exp_id = seq_len(no_exp), rep_id = seq_len(no_samples))

test <- test[order(test$exp_id, decreasing = FALSE), ]

test_ls <- df_to_list(test, TRUE)
  
  
# run one job -----------------------------------------------------------------


# t <- obj$enqueue(EM_full_routine(test_ls[[1]],
#                                  parms = parameters,
#                                  out_path = out_path,
#                                  boot_ls = bt_samples,
#                                  in_path = in_path,
#                                  predictors = all_predictors,
#                                  grp_flds_1 = grp_flds_1,
#                                  grp_flds_2 = grp_flds_2,
#                                  adm_dataset = covariates_adm,
#                                  foi_data = foi_data,
#                                  sqr_data = covariates_squares))
                 

# run -------------------------------------------------------------------------


if (CLUSTER) {

  multi_full_EM_experiments <- queuer::qlapply(
    test_ls[401:4000],
    EM_full_routine,
    obj,
    parms = parameters,
    out_path = out_path,
    boot_ls = bt_samples,
    in_path = in_path,
    predictors = all_predictors,
    grp_flds_1 = grp_flds_1,
    grp_flds_2 = grp_flds_2,
    adm_dataset = covariates_adm,
    foi_data = foi_data,
    sqr_data = covariates_squares)
  
} else {

  multi_full_EM_experiments <- lapply(
    test_ls[1],
    EM_full_routine,
    parms = parameters,
    out_path = out_path,
    boot_ls = bt_samples,
    in_path = in_path,
    predictors = all_predictors,
    grp_flds_1 = grp_flds_1,
    grp_flds_2 = grp_flds_2,
    adm_dataset = covariates_adm,
    foi_data = foi_data,
    sqr_data = covariates_squares)

}

if(!CLUSTER){
  context::parallel_cluster_stop()  
}
