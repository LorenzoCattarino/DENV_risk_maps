
source(file.path("R", "utility_functions.R"))
source(file.path("R", "create_parameter_list.R"))
source(file.path("R", "random_forest", "variable_selection_stepwise.R"))

library(ggplot2)


# define parameters ----------------------------------------------------------- 


extra_prms <- list(var_to_fit = "FOI",
                   no_reps = 1, 
                   addition = TRUE,
                   parallel_2 = TRUE,
                   stepwise_exp_id = 6)


# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

my_dir <- paste0("grid_size_", parameters$grid_size)

top_ones_within_reps <- parameters$top_ones_within_reps
top_ones_across_reps <- parameters$top_ones_across_reps

stepwise_exp_dir <- paste0("stepwise_v", parameters$stepwise_exp_id)

table_out_path <- file.path("output", 
                            "variable_selection", 
                            stepwise_exp_dir)

plot_out_path <- file.path("figures", 
                           "variable_selection", 
                           stepwise_exp_dir)

no_samples <- seq_len(parameters$no_samples)


# load data -------------------------------------------------------------------


boot_samples <- readRDS(file.path("output", 
                                  "EM_algorithm",
                                  "bootstrap_models",
                                  my_dir, 
                                  "bootstrap_samples.rds"))


# ----------------------------------------------------------------------------- 


all_dir_paths <- file.path(table_out_path, 
                           paste0("sample_", no_samples), 
                           "addition", 
                           "output_from_addition.rds")

bsample_step_addition <- lapply(all_dir_paths, readRDS)

# plot data only for the first 10 samples
lapply(seq_along(bsample_step_addition)[1:10],
       plot_RMSE_addition,
       bsample_step_addition,
       plot_out_path)

write_out_rds(bsample_step_addition, 
              table_out_path, 
              "list_of_outputs_from_addition.rds")  

# bundles <- obj$task_bundle_info()
# 
# my_task_id <- bundles[nrow(bundles), "name"] 
# 
# bsample_step_addition_t <- obj$task_bundle_get(my_task_id)
# 
# bsample_step_addition <- bsample_step_addition_t$results()
# 
# # plot data only for the first 10 samples 
# lapply(seq_along(bsample_step_addition)[1:10], 
#        plot_RMSE_addition, 
#        bsample_step_addition, 
#        plot_out_path)
# 
# all_preds <- lapply(bsample_step_addition, 
#                     get_changed_predictors, 
#                     nrow(bsample_step_addition[[1]][[1]]))
# 
# all_preds_top <- lapply(all_preds, get_top_from_replicates, top_ones_within_reps)
#   
# # calculate selection frequency of all 26 predictors across replicates
# ret1 <- lapply(all_preds_top, calculate_sel_freq, top_ones_across_reps)
# 
# no_samples <- parameters$no_samples
# 
# # save best predictors from stepwise addition - for each bootstrap sample
# lapply(seq(no_samples),
#        save_addition_best_preds,
#        results = ret1, 
#        names = names(boot_samples[[1]]), 
#        table_out_path)
  