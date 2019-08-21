
options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "utility_functions.R"),
  file.path("R", "create_parameter_list.R"),
  file.path("R", "random_forest", "preprocess.R"),
  file.path("R", "random_forest", "fit_ranger_RF_and_make_predictions.R"),
  file.path("R", "random_forest", "exp_max_algorithm.R"),
  file.path("R", "random_forest", "join_predictions.R"),
  file.path("R", "random_forest", "full_routine_bootstrap.R"),
  file.path("R", "random_forest", "wrapper_functions_for_boot_analysis.R"),
  file.path("R", "prepare_datasets", "set_pseudo_abs_weights.R"),
  file.path("R", "prepare_datasets", "average_up.R"),
  file.path("R", "prepare_datasets", "calculate_wgt_corr.R"),
  file.path("R", "plotting", "plot_EM_diagnostics.R"))

my_pkgs <- c("ranger", "dplyr", "weights", "ggplot2")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters ----------------------------------------------------------- 


extra_prms <- list(grp_flds = c("unique_id", "ID_0", "ID_1"),
                   id_fld = "unique_id",
                   ranger_threads = 1) 

predictor_numbers <- c(16, 25)

response_vars <- "FOI"
  
grid_sizes <- c(1/120, 0.5, 1, 2, 5, 10)


# start up the cluster --------------------------------------------------------


if (CLUSTER) {
  
  #config <- didehpc::didehpc_config(template = "24Core")
  obj <- didehpc::queue_didehpc(ctx)
  
} else {
  
  context::context_load(ctx)
  
}


# define variables ------------------------------------------------------------  


parameters <- create_parameter_list(extra_params = extra_prms)

covariates_dir <- parameters$covariates_dir

no_samples <- parameters$no_samples


# load data ------------------------------------------------------------------- 


foi_data <- read.csv(file.path("output", 
                               "foi", 
                               "All_FOI_estimates_and_predictors.csv"),
                     stringsAsFactors = FALSE) 

predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection",
                                     covariates_dir,
                                     "predictor_rank.csv"), 
                           stringsAsFactors = FALSE)

all_sqr_covariates <- readRDS(file.path("output", 
                                        "env_variables", 
                                        "all_squares_env_var_0_1667_deg.rds"))

admin_covariates <- read.csv(file.path("output",
                                       "env_variables",
                                       "All_adm1_env_var.csv"),
                             header = TRUE,
                             stringsAsFactors = FALSE)

data_sqr_covariates <- readRDS(file.path("output", 
                                         "EM_algorithm",
                                         "best_fit_models",
                                         "model_1",
                                         "env_variables", 
                                         "env_vars_20km.rds"))


# pre process -----------------------------------------------------------------


all_predictors <- predictor_rank$name

test1 <- expand.grid(var = response_vars, 
                     gs = grid_sizes[c(1, 5)], 
                     no_pred = predictor_numbers, 
                     stringsAsFactors = FALSE)

test2 <- expand.grid(var = response_vars, 
                     gs = grid_sizes[-c(1, 5)], 
                     no_pred = predictor_numbers[2], 
                     stringsAsFactors = FALSE)

test <- rbind(test1, test2)

test_all <- cbind(exp_id = seq_len(nrow(test)), test)

test_all_2 <- expand.grid(exp_id = test_all$exp_id, rep_id = seq_len(no_samples))

test_all_2 <- test_all_2[order(test_all_2$exp_id, decreasing = FALSE), ]

test_all_3 <- left_join(test_all_2, test_all)

write_out_csv(test_all_3, 
              file.path("output", "EM_algorithm", "bootstrap_models"), 
              "boostrap_fit_experiments.csv")

test_ls <- df_to_list(test_all_3, TRUE)


# save a table of unique combinations of factors ------------------------------


bootstrap_experiments_by <- dplyr::group_by(test_all_3, exp_id, var, gs, no_pred)

bootstrap_experiments_uni <- dplyr::summarise_at(bootstrap_experiments_by, "rep_id", min)

write_out_csv(bootstrap_experiments_uni, 
              file.path("output", "EM_algorithm", "bootstrap_models"), 
              "boostrap_fit_experiments_uni.csv")


# run one job -----------------------------------------------------------------


# t <- obj$enqueue(full_routine_bootstrap(test_ls[[1]],
#                                         parms = parameters,
#                                         original_foi_data = foi_data,
#                                         adm_covariates = admin_covariates,
#                                         data_squares = data_sqr_covariates,
#                                         all_squares = all_sqr_covariates,
#                                         all_predictors = all_predictors))


# run -------------------------------------------------------------------------


if (CLUSTER) {

  multi_full_EM_experiments_2 <- queuer::qlapply(
    test_ls[1:200],
    full_routine_bootstrap,
    obj,
    parms = parameters,
    original_foi_data = foi_data,
    adm_covariates = admin_covariates,
    data_squares = data_sqr_covariates,
    all_squares = all_sqr_covariates,
    all_predictors = all_predictors)

} else {

  full_routine_bootstrap(test_ls[[1]],
                         parms = parameters,
                         original_foi_data = foi_data,
                         adm_covariates = admin_covariates,
                         data_squares = data_sqr_covariates,
                         all_squares = all_sqr_covariates,
                         all_predictors = all_predictors)

}
