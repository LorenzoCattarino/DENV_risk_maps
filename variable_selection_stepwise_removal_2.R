
options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "random_forest", "functions_for_fitting_h2o_RF_and_making_predictions.r"),
  file.path("R", "random_forest", "stepwise_variable_addition_removal.R"),
  file.path("R", "prepare_datasets", "set_pseudo_abs_weights.R"),
  file.path("R", "utility_functions.R"))

my_pkgs <- c("h2o", "ggplot2")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters ----------------------------------------------------------- 


top_ones <- 26 # all of them

parameters <- list(
  grid_size = 1,
  no_trees = 500,
  min_node_size = 20,
  no_steps_L1 = 20,   # 20
  no_steps_L2 = 10,   # 10
  pseudoAbs_value = -0.02,
  all_wgt = 1,
  wgt_limits = c(1, 500),
  no_reps = 10)       # 10

no_fits <- 50

var_to_fit <- "FOI"

out_fig_name <- "Frequency_of_the_numbers_of_selected_preds_pure.png"

out_fig_path <- file.path("figures", 
                          "variable_selection", 
                          "stepwise")

out_tab_name <- "predictor_rank.csv"

out_tab_path <- file.path("output", 
                          "variable_selection", 
                          "stepwise_pure")


# define variables ------------------------------------------------------------


my_dir <- paste0("grid_size_", parameters$grid_size)


# rebuild the queue object? --------------------------------------------------- 


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "20Core")
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


# predictors are ranked according to the selection frequency 
# across bootstrap samples 

all_preds <- lapply(bsample_step_removal, get_removal_results)

all_predictors <- names(boot_samples[[1]])

all_preds_n <- lapply(all_preds, function(x) which(all_predictors %in% x))

all_preds_sel_freq_rank <- calculate_sel_freq(unlist(all_preds_n), top_ones)

out <- data.frame(predictor = all_preds_sel_freq_rank, 
                  name = all_predictors[all_preds_sel_freq_rank],
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
