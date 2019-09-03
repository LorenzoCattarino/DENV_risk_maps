
source(file.path("R", "utility_functions.R"))
source(file.path("R", "create_parameter_list.R"))
source(file.path("R", "random_forest", "variable_selection_stepwise.R"))

library(ggplot2)


# define parameters ----------------------------------------------------------- 


extra_prms <- list(var_to_fit = "FOI",
                   addition = FALSE,
                   parallel_2 = TRUE,
                   stepwise_exp_id = 5)

out_fig_name <- "Frequency_of_the_numbers_of_selected_preds"

out_tab_name <- "predictor_rank.csv"


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

no_samples <- seq_len(parameters$no_samples)


# load data -------------------------------------------------------------------


boot_samples <- readRDS(file.path("output", 
                                  "EM_algorithm",
                                  "bootstrap_models",
                                  my_dir, 
                                  "bootstrap_samples.rds"))


# get results ----------------------------------------------------------------- 


all_dir_paths <- file.path(out_tab_path, 
                           paste0("sample_", no_samples), 
                           "removal", 
                           "output_from_removal.rds")

bsample_step_removal <- lapply(all_dir_paths, readRDS)


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

write_out_csv(out, out_tab_path, out_tab_name, row.names = FALSE)


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
