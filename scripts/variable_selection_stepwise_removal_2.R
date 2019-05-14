
options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "random_forest", "fit_ranger_RF_and_make_predictions.R"),
  file.path("R", "random_forest", "variable_selection_stepwise.R"),
  file.path("R", "prepare_datasets", "set_pseudo_abs_weights.R"),
  file.path("R", "utility_functions.R"))

my_pkgs <- c("ranger", "ggplot2")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters ----------------------------------------------------------- 


parameters <- list(
  grid_size = 5,
  no_steps_L1 = 28)     

top_ones <- parameters$no_steps_L1 # all of them

var_to_fit <- "FOI"

out_fig_name <- "Frequency_of_the_numbers_of_selected_preds.png"

out_tab_name <- "predictor_rank.csv"

out_fig_path <- file.path("figures", 
                          "variable_selection", 
                          "stepwise")

out_tab_path <- file.path("output", 
                          "variable_selection", 
                          "stepwise")


# define variables ------------------------------------------------------------


my_dir <- paste0("grid_size_", parameters$grid_size)


# rebuild the queue object? --------------------------------------------------- 


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "12and16Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  context::parallel_cluster_start(8, ctx)
  
}


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


#  get predictor names --------------------------------------------------------


# plot raw datata 
lapply(seq_along(bsample_step_removal)[1:10], plot_RMSE_removal, bsample_step_removal, out_fig_path)

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

dir.create(out_fig_path, FALSE, TRUE)

png(file.path(out_fig_path, out_fig_name), 
    width = 10, 
    height = 10, 
    units = "cm",
    pointsize = 12,
    res = 200)

print(p)

dev.off()
