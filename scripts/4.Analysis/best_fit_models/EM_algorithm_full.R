options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "utility_functions.R"),
  file.path("R", "create_parameter_list.R"),
  file.path("R", "random_forest", "preprocess.R"),
  file.path("R", "random_forest", "fit_ranger_RF_and_make_predictions.R"),
  file.path("R", "random_forest", "exp_max_algorithm.R"),
  file.path("R", "random_forest", "join_predictions.R"),
  file.path("R", "random_forest", "full_routine.R"),
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


extra_prms <- list(grp_flds = c("data_id", "ID_0", "ID_1"),
                   id_fld = "data_id",
                   ranger_threads = NULL)

predictor_numbers <- c(16, 25)

response_vars <- "FOI"


# start up the cluster --------------------------------------------------------


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "20Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  
}


# define variables ------------------------------------------------------------  


parameters <- create_parameter_list(extra_params = extra_prms)

covariates_dir <- parameters$covariates_dir


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


# pre process -----------------------------------------------------------------


all_predictors <- predictor_rank$name

test <- expand.grid(var = response_vars, 
                    no_pred = predictor_numbers, 
                    stringsAsFactors = FALSE)

test_2 <- cbind(exp_id = seq_len(nrow(test)), test)

test_2 <- test_2[order(test_2$exp_id, decreasing = FALSE), ]

write_out_csv(test_2, 
              file.path("output", "EM_algorithm", "best_fit_models"), 
              "best_fit_experiments.csv",
              row.names = FALSE)

test_ls <- df_to_list(test_2, TRUE)


# run one job -----------------------------------------------------------------


# t <- obj$enqueue(full_routine(test_ls[[1]],
#                               parms = parameters,
#                               foi_data = foi_data,
#                               adm_covariates = admin_covariates,
#                               all_squares = all_sqr_covariates,
#                               all_predictors = all_predictors))


# run -------------------------------------------------------------------------


if (CLUSTER) {

  multi_full_routine <- queuer::qlapply(
    test_ls,
    full_routine,
    obj,
    parms = parameters,
    foi_data = foi_data,
    adm_covariates = admin_covariates,
    all_squares = all_sqr_covariates,
    all_predictors = all_predictors)

} else {

  full_routine(test_ls[[1]],
               parms = parameters,
               foi_data = foi_data,
               adm_covariates = admin_covariates,
               all_squares = all_sqr_covariates,
               all_predictors = all_predictors)

}
