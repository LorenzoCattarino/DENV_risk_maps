
options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "random_forest", "fit_ranger_RF_and_make_predictions.R"),
  file.path("R", "random_forest", "EM_full_routine.R"),
  file.path("R", "random_forest", "exp_max_algorithm.R"),
  file.path("R", "plotting", "functions_for_plotting_raster_maps.R"),
  file.path("R", "plotting", "generic_scatter_plot.R"),
  file.path("R", "prepare_datasets", "remove_NA_rows.R"),
  file.path("R", "prepare_datasets", "average_up.R"),
  file.path("R", "prepare_datasets", "calculate_wgt_corr.R"),
  file.path("R", "utility_functions.R"))

my_pkgs <- c("ranger", "dplyr", "fields", "ggplot2", "weights", "colorRamps")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters ----------------------------------------------------------- 


parameters <- list(
  shape_1 = 0,
  shape_2 = 5,
  shape_3 = 1e6,
  all_wgt = 1,
  pseudoAbs_value = c(-0.02, 0.5),
  resample_grid_size = 20,
  foi_offset = 0.03,
  no_trees = 500,
  min_node_size = 20,
  no_samples = 200,
  EM_iter = 10) 

grp_flds_1 <- c("ID_0", "ID_1", "unique_id")

grp_flds_2 <- c("ID_0", "ID_1", "data_id")

predictor_numbers <- c(9, 23, 26)

response_vars <- c("FOI", "R0_1", "R0_2", "R0_3")
  
grid_sizes <- c(1/120, 0.5, 1, 2, 5, 10)


# define variables ------------------------------------------------------------  


no_samples <- parameters$no_samples


# start up the cluster --------------------------------------------------------


if (CLUSTER) {
  
  obj <- didehpc::queue_didehpc(ctx)
  
} else {
  
  context::context_load(ctx)
  
}


# load data ------------------------------------------------------------------- 


foi_data <- read.csv(file.path("output", 
                               "foi", 
                               "All_FOI_estimates_and_predictors.csv"),
                     stringsAsFactors = FALSE) 

predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection",
                                     "stepwise",
                                     "predictor_rank.csv"), 
                           stringsAsFactors = FALSE)

all_sqr_covariates <- readRDS(file.path("output", 
                                        "env_variables", 
                                        "all_squares_env_var_0_1667_deg.rds"))

data_sqr_covariates <- readRDS(file.path("output", 
                                         "EM_algorithm",
                                         "best_fit_models",
                                         "env_variables", 
                                         "env_vars_20km_2.rds"))

admin_covariates <- read.csv(file.path("output",
                                       "env_variables",
                                       "All_adm1_env_var.csv"),
                             header = TRUE,
                             stringsAsFactors = FALSE)


# pre process -----------------------------------------------------------------


all_predictors <- predictor_rank$name

test1 <- expand.grid(var = response_vars, gs = grid_sizes[c(1, 5)], no_pred = predictor_numbers, stringsAsFactors = FALSE)
  
test2 <- expand.grid(var = response_vars[4], gs = grid_sizes[-c(1, 5)], no_pred = predictor_numbers[2], stringsAsFactors = FALSE)
 
test <- rbind(test1, test2)

test_all <- cbind(exp_id = seq_len(nrow(test)), test)

test_all_2 <- expand.grid(exp_id = test_all$exp_id, rep_id = seq_len(no_samples))

test_all_2 <- test_all_2[order(test_all_2$exp_id, decreasing = FALSE), ]

test_all_3 <- left_join(test_all_2, test_all)

write_out_csv(test_all_3, 
              file.path("output", "EM_algorithm", "bootstrap_models"), 
              "boostrap_fit_experiments.csv")

test_ls <- df_to_list(test_all_3, TRUE)

  
# run one job -----------------------------------------------------------------


# t <- obj$enqueue(EM_full_routine(test_ls[[1]],
#                                  parms = parameters,
#                                  data_squares = data_sqr_covariates,
#                                  all_squares = all_sqr_covariates,
#                                  predictors = all_predictors,
#                                  grp_flds_1 = grp_flds_1,
#                                  grp_flds_2 = grp_flds_2,
#                                  adm_dataset = admin_covariates,
#                                  foi_data = foi_data))
                 

# run -------------------------------------------------------------------------


if (CLUSTER) {

  multi_full_EM_experiments_2 <- queuer::qlapply(
    test_ls[2001:5600],
    EM_full_routine,
    obj,
    parms = parameters,
    data_squares = data_sqr_covariates,
    all_squares = all_sqr_covariates,
    predictors = all_predictors,
    grp_flds_1 = grp_flds_1,
    grp_flds_2 = grp_flds_2,
    adm_dataset = admin_covariates,
    foi_data = foi_data)

} else {
  
  EM_full_routine(test_ls[[1]],
                  parms = parameters,
                  data_squares = data_sqr_covariates,
                  all_squares = all_sqr_covariates,
                  predictors = all_predictors,
                  grp_flds_1 = grp_flds_1,
                  grp_flds_2 = grp_flds_2,
                  adm_dataset = admin_covariates,
                  foi_data = foi_data)
  
}
