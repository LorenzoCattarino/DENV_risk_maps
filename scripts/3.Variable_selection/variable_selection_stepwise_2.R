
source(file.path("R", "utility_functions.R"))
source(file.path("R", "create_parameter_list.R"))
source(file.path("R", "random_forest", "variable_selection_stepwise.R"))

library(ggplot2)


# define parameters ----------------------------------------------------------- 


extra_prms <- list(var_to_fit = "FOI",
                   no_reps = 1, 
                   addition = TRUE,
                   parallel_2 = TRUE,
                   stepwise_exp_id = 6,
                   tolerance = 5)

out_fig_name <- "Frequency_of_the_numbers_of_selected_preds"

out_tab_name <- "predictor_rank.csv"


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

tolerance <- parameters$tolerance


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

write_out_csv(bsample_step_addition[[1]], 
              file.path(table_out_path, "sample_1", "addition"),
              "output_from_addition.csv",
              row.names = FALSE)


#  get predictor names --------------------------------------------------------


bsample_step_addition_tol <- lapply(bsample_step_addition, set_tol, tolerance)
  
all_preds_tol <- lapply(bsample_step_addition_tol, get_addition_results)

all_predictors <- names(boot_samples[[1]])

all_preds_n <- lapply(all_preds_tol, function(x) which(all_predictors %in% x))

sel_freq <- table(unlist(all_preds_n))

all_preds_sel_freq_rank <- sel_freq[order(sel_freq, decreasing = TRUE)]

all_preds_indices <- as.numeric(names(all_preds_sel_freq_rank))

out <- data.frame(predictor = all_preds_indices, 
                  name = all_predictors[all_preds_indices],
                  sel_freq = as.numeric(all_preds_sel_freq_rank),
                  stringsAsFactors = FALSE)

write_out_csv(out, table_out_path, out_tab_name, row.names = FALSE)


# get optimal number of predictors --------------------------------------------


# it creates and save the frequency distribution (across bootstrap samples) of 
# the minimum number of predictors contributing to increse the RMSE during removal 

all_numbers_tol <- vapply(all_preds_tol, length, 1)

all_preds <- lapply(bsample_step_addition, get_addition_results)
all_numbers <- vapply(all_preds, length, 1)

all_no_sel_vars <- rbind(
  data.frame(no_sel_vars = all_numbers, tolerance = "n_min"),
  data.frame(no_sel_vars = all_numbers_tol, tolerance = "n_approx"))

x1 <- min(all_no_sel_vars$no_sel_vars)-1
  
x2 <- max(all_no_sel_vars$no_sel_vars)+1

p <- ggplot(all_no_sel_vars, aes(no_sel_vars, fill = tolerance)) +
  geom_bar(position = "dodge", width = 0.5) + 
  scale_y_continuous("Frequency across bootstrap samples") +
  scale_x_continuous("Number of explanatory variables in minimum RMSE set", 
                     breaks = seq(x1, x2, 1),
                     labels = seq(x1, x2, 1),
                     limits = c(x1, x2)) + 
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1),
        legend.title = element_blank(),
        legend.key.size = unit(0.3, "cm"))

save_plot(p, plot_out_path, out_fig_name, wdt = 15, hgt = 8)

write_out_csv(all_no_sel_vars, table_out_path, "freq_distr_no_top_predictors.csv", row.names = FALSE)
