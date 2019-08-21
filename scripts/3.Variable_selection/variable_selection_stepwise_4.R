
options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "random_forest", "fit_ranger_RF_and_make_predictions.R"),
  file.path("R", "random_forest", "variable_selection_stepwise.R"),
  file.path("R", "prepare_datasets", "set_pseudo_abs_weights.R"),
  file.path("R", "utility_functions.R"),
  file.path("R", "create_parameter_list.R"))

my_pkgs <- c("ranger", "ggplot2")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters ----------------------------------------------------------- 


extra_prms <- list(var_to_fit = "FOI",
                   addition = FALSE,
                   parallel_2 = TRUE,
                   stepwise_exp_id = 5)

out_fig_name <- "Frequency_of_the_numbers_of_selected_preds"

out_tab_name <- "predictor_rank.csv"


# rebuild the queue object? --------------------------------------------------- 


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "24Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  context::parallel_cluster_start(8, ctx)
  
}


# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

my_dir <- paste0("grid_size_", parameters$grid_size)

stepwise_exp_dir <- paste0("stepwise_v", parameters$stepwise_exp_id)

out_fig_path <- file.path("figures", 
                          "variable_selection", 
                          stepwise_exp_dir)

out_tab_path <- file.path("output", 
                          "variable_selection", 
                          stepwise_exp_dir)


# load data -------------------------------------------------------------------


boot_samples <- readRDS(file.path("output", 
                                  "EM_algorithm",
                                  "bootstrap_models",
                                  my_dir, 
                                  "bootstrap_samples.rds"))


# get results ----------------------------------------------------------------- 


bundles <- obj$task_bundle_info()

my_task_id <- bundles[nrow(bundles), "name"] 

bsample_step_removal_t <- obj$task_bundle_get(my_task_id)

bsample_step_removal <- bsample_step_removal_t$results()


# save all outputs ------------------------------------------------------------


lapply(seq_along(bsample_step_removal), 
       save_removal_outputs, 
       results = bsample_step_removal, 
       out_pth = out_tab_path)


# plot some outputs -----------------------------------------------------------


lapply(seq_along(bsample_step_removal)[1:10], 
       plot_RMSE_removal, 
       bsample_step_removal, 
       out_fig_path)


#  get predictor names --------------------------------------------------------



# predictors are ranked according to the selection frequency 
# across bootstrap samples 

all_preds <- lapply(bsample_step_removal, get_removal_results)

all_predictors <- names(boot_samples[[1]])

all_preds_n <- lapply(all_preds, function(x) which(all_predictors %in% x))

sel_freq <- table(unlist(all_preds_n))

all_preds_sel_freq_rank <- sel_freq[order(sel_freq, decreasing = TRUE)]

all_preds_indices <- as.numeric(names(all_preds_sel_freq_rank))

out <- data.frame(predictor = all_preds_indices, 
                  name = all_predictors[all_preds_indices],
                  sel_freq = as.numeric(all_preds_sel_freq_rank),
                  stringsAsFactors = FALSE)

write_out_csv(out, out_tab_path, out_tab_name)


# get optimal number of predictors --------------------------------------------


# it creates and save the frequency distribution (across bootstrap samples) of 
# the minimum number of predictors contributing to increse the RMSE during removal 

all_numbers <- vapply(all_preds, length, 1)

all_no_sel_vars <- data.frame(no_sel_vars = all_numbers)

p <- ggplot(all_no_sel_vars, aes(no_sel_vars)) +
  geom_histogram() + 
  scale_y_continuous("Frequency") +
  scale_x_continuous("Number of selected variables")

save_plot(p, out_fig_path, out_fig_name, 10, 10)
